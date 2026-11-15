                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                ╭────────╮                   ╭───╮                                ┃
┃                                │   ╭────╯                   │   │                                ┃
┃                                │   │                        │   │                                ┃
┃                                │   ╰──╮╭───╮ ╭───╮╭────────╮│   │                                ┃
┃                                │   ╭──╯│   │ │   ││   ╭─╮  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╰─╯  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╭────╯│   │                                ┃
┃                                │   │   │   ╰─╯   ││   ╰────╮│   │                                ┃
┃                                ╰───╯   ╰────╌╰───╯╰────────╯╰───╯                                ┃
┃                                                                                                  ┃
┃    Fuel, version 0.6.0.                                                                          ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://propensive.dev/fuel/                                                              ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package fuel

import scala.quoted.Quotes
import scala.tasty.inspector as sti

import soundness.*

import dysasymptotics.{linearAccess, linearSize}
import stenography.{Bindings, Designator, Syntax}

// The API read from compiled code: the compiler's own TASTy unpickler, through the inspector,
// over the project's `.tasty` files and its dependencies' jars, against the classpath which
// carries everything they reference (the Scala library included). Each package, class, object,
// method, value and type becomes an `Api.Entity` with its declarations' signatures as
// stenography syntax, as Amok's walker made them. Runs on the compiler's thread
// (`Checking.exclusive`): dotc pins its context to the thread of first use.
object Inspection:
  // What the compiler threw while it read something: as often an `AssertionError`, or another
  // `java.lang.Error`, as an `Exception`.
  case class ReadError(cause: Throwable)(using Diagnostics)
  extends Error(m"the compiler could not read what it was given: ${cause.toString.tt}")

  // Every call into the compiler which may fail is made through this. Contingency recovers
  // only from an `Exception`, and the compiler signals what it cannot read as often with an
  // `AssertionError` (stenography's named-tuple check, among others), so whatever it throws is
  // caught here, once, as a `ReadError`, which `safely` and `recover` then see.
  private def reading[result](block: => result): result raises ReadError =
    try block catch case error: Throwable => abort(ReadError(error))

  // The members left out of the model because reading them failed, and the first failure:
  // what a compile reports, so that a model with gaps says why.
  private val skipped0: Atomic.Int = Atomic(0)
  private val firstFailure: Atomic.Ref[Optional[Throwable]] = Atomic.Ref.vacant[Throwable]

  private def skipped(error: Throwable): Unit =
    skipped0.since(_ + 1)
    firstFailure.replace(Unset, error)

  // How many types were rendered, how many applied types from their parts because stenography
  // could not, and how many could not be at all and are shown as `?`.
  val rendered: Atomic.Int = Atomic(0)
  val fallbacks: Atomic.Int = Atomic(0)
  val unrendered: Atomic.Int = Atomic(0)

  // The count and the first failure since the last call, which resets them.
  def failures(): (Int, Optional[Throwable]) = (skipped0.swap(0), firstFailure.swap(Unset))

  // Whether a dependency's parameters are entities too, as the project's own always are: off,
  // since nothing documents them, except to survey where parameters meet members.
  @volatile
  var everyParameter: Boolean = false

  // The keys at which a parameter met a member of the same name (which the member keeps),
  // each with the two kinds, latest first, since the last call, which resets them.
  private val clashes0: Atomic.Ref[List[(Text, Api.Kind, Api.Kind)]] =
    Atomic(Nil: List[(Text, Api.Kind, Api.Kind)])

  def clashes(): List[(Text, Api.Kind, Api.Kind)] = clashes0.swap(Nil).reverse

  // The entities a walk has collected, by key.
  private type Entities = Atomic.Ref[Map[Text, Api.Entity]]

  private def model(entities: Entities): Api = Api(entities()).relinked

  // The entities of every `.tasty` beneath the `directories` (the project's own, marked so)
  // and of every `jars` entry.
  def walk(directories: List[Text], jars: List[Text], classpath: List[Text]): Api | Throwable =
    val tastyFiles: List[Text] = directories.bind(tastyIn)

    Checking.exclusive:
      val entities: Entities = Atomic(Map[Text, Api.Entity]())
      val failure: Atomic.Ref[Optional[Throwable]] = Atomic.Ref.vacant[Throwable]

      object inspector extends sti.Inspector:
        def inspect(using quotes: Quotes)(tastys: scala.List[sti.Tasty[quotes.type]]): Unit =
          recover:
            case error: ReadError => failure() = error.cause

          . protect:
            reading:
              val walker = Walker(entities)(using quotes)

              List.from(tastys).each: tasty =>
                val path: Text = tasty.path.tt
                walker.walk(tasty.ast, own = path.ends(t".tasty") && !path.contains(t".jar"))

      recover:
        case error: ReadError => error.cause

      . protect:
        reading:
          // The inspector, the compiler's, takes the standard library's lists of strings.
          sti.TastyInspector.inspectAllTastyFiles
            ( tastyFiles.stdlib.map(_.s), jars.stdlib.map(_.s), classpath.stdlib.map(_.s) )
            ( inspector )

        failure().or(model(entities))

  // The entities of every `jars` entry, through a session's symbol table: each top-level
  // class the jar's `.tasty` entries name is loaded as the compiler loads any classpath
  // symbol — its signatures, never a body — which is what makes it robust where unpickling
  // whole trees is not; a class which cannot be loaded is left out. A `.class` entry with no
  // `.tasty` beside it is a Java class, which the compiler reads from its classfile, and of
  // which only what is public is kept.
  // The `shadowed` packages are the project's own, whose copies in a jar (a launcher assembly
  // carrying the project itself) are left to the project's sources: loading them into the
  // session ahead of the compile leaves it writing nothing.
  def classpath
    ( jars:     List[Text],
      context:  dotty.tools.dotc.core.Contexts.Context,
      shadowed: List[Text]                             = Nil )
  :   Api | Throwable =

    loading(context): (walker: Walker) =>
      jars.each: (jar: Text) =>
        val entries: List[Text] = archived(jar)
        val tastys: List[Text] = entries.filter(_.ends(t".tasty"))
        val stems: Set[Text] = tastys.map(_.skip(6, Rtl)).to[Set]

        val javas: List[Text] = entries.filter(_.ends(t".class")).filter: entry =>
          val stem: Text = entry.skip(6, Rtl)
          topLevelJava(stem.cut(t"/").last) && !stems.has(stem)

        def shadows(pkg: Text): Boolean =
          shadowed.exists { (p: Text) => pkg == p || pkg.starts(t"$p.") }

        // `pkg/Name.tasty` is the class, `pkg/Name$.tasty` its module; a `$package` carrier
        // holds a package's top-level members.
        def load(name: Text, java: Boolean): Unit =
          val full: Text = name.skip(6, Rtl).tr('/', '.')
          val segments: List[Text] = full.cut(t".")
          val pkg: Text = segments.keep(segments.size - 1).join(t".")
          if !shadows(pkg) then walker.load(full, java)

        tastys.each(load(_, false))
        javas.each(load(_, true))

  // The name of every entry of a jar, as its archive writes it, without a leading `/`.
  private def archived(jar: Text): List[Text] =
    import filesystemBackends.javaBaseFilesystem
    import logging.silentLogging
    import strategies.throwUnsafely

    Zipfile.read(jar.as[Path on Linux]).entries.map: (entry: Zip.Entry) =>
      val name: Text = entry.ref.encode
      if name.starts(t"/") then name.skip(1) else name

  // The entities of the JDK's public classes in the exported packages of its `modules`, read
  // from the runtime image as the compiler reads them, so that `java.lang.String#getBytes` may
  // be named as any other member.
  def jdk
    ( context: dotty.tools.dotc.core.Contexts.Context,
      modules: List[Text]                             = List(t"java.base") )
  :   Api | Throwable =

    loading(context): (walker: Walker) =>
      modules.each: (module: Text) =>
        jdkClasses(module).each(walker.load(_, true))

  // The top-level classes of the packages which a module of the running JDK exports to every
  // module, each as `pkg.Name`, in the order the runtime image lists them. Soundness can list
  // neither the JDK's modules nor the class files of its runtime image (soundness#2502), so
  // they are read here alone, through the boot layer's modules and the `jrt:` filesystem.
  private def jdkClasses(module: Text): List[Text] =
    val image = java.nio.file.FileSystems.getFileSystem(java.net.URI.create("jrt:/")).nn

    java.lang.ModuleLayer.boot().nn.findModule(module.s).nn.optional.lay(Nil): found =>
      val exports = Iterator.from(found.nn.getDescriptor.nn.exports.nn.iterator.nn).to[List]
      val packages: List[Text] = exports.filter(!_.nn.isQualified).map(_.nn.source.nn.tt)

      packages.bind: (pkg: Text) =>
        val directory = image.getPath("/modules", module.s, pkg.tr('.', '/').s).nn

        if !java.nio.file.Files.isDirectory(directory) then Nil else
          val stream = java.nio.file.Files.list(directory).nn
          val paths = Iterator.from(stream.iterator.nn).to[List]
          val files: List[Text] = paths.map(_.nn.getFileName.nn.toString.tt)
          stream.close()

          files.filter(_.ends(t".class")).map(_.skip(6, Rtl)).filter(topLevelJava).map: file =>
            t"$pkg.$file"

  // Whether a class file's simple name is a top-level class's: not a nested or anonymous
  // class's (`Outer$Inner`), nor a module's or package's descriptor.
  private def topLevelJava(name: Text): Boolean =
    !name.contains('$') && name != t"module-info" && name != t"package-info"

  // A walk through a session's symbol table, which `walk` gives its walker to load classes
  // through, as the model of what it loaded.
  private def loading(context: dotty.tools.dotc.core.Contexts.Context)(walk: Walker => Unit)
  :   Api | Throwable =

    Checking.exclusive:
      recover:
        case error: ReadError => error.cause

      . protect:
        reading:
          // A context of the session's with a cache of quotes, which rendering a type through
          // stenography's quoted patterns needs (a run of the compiler sets one up; a bare
          // session context has none), initialised as every run initialises its own first.
          given dotty.tools.dotc.core.Contexts.Context =
            dotty.tools.dotc.quoted.QuotesCache.init(context.fresh)

          context.base.initialize()

          val quotes: Quotes = scala.quoted.runtime.impl.QuotesImpl()
          val entities: Entities = Atomic(Map[Text, Api.Entity]())
          walk(Walker(entities)(using quotes))
          model(entities)

  // Every `.tasty` beneath a directory, as the inspector takes them; none where it is not one.
  def tastyIn(directory: Text): List[Text] =
    import filesystemBackends.javaBaseFilesystem
    import filesystemOptions.dereferenceSymlinks
    import galilei.glob

    safely(directory.as[Path on Linux].glob(Glob.parse(t"**/*.tasty")).map(_.encode)).or(Nil)

  private class Walker(entities: Entities)(using tracked val quotes: Quotes):
    import quotes.reflect.*

    given Bindings = Bindings()

    // The compiler's context behind the quotes, through which its symbol table is reached.
    private def context: dotty.tools.dotc.core.Contexts.Context =
      import scala.quoted.runtime.impl.QuotesImpl

      quotes match
        case quotes: QuotesImpl => quotes.ctx
        case _                  => panic(m"the quotes are not the compiler's")

    // The clauses of a symbol's parameters.
    private def paramLists(symbol: Symbol): List[List[Symbol]] =
      List.from(symbol.paramSymss).map(List.from(_))

    private def skipped(symbol: Symbol): Boolean =
      val name: Text = symbol.name.tt
      val flags = symbol.flags

      // A Java member which says neither `public` nor `protected` is visible only within its
      // package, which the compiler records as its being private within it.
      val packagePrivate: Boolean =
        flags.is(Flags.JavaDefined) && symbol.privateWithin.isDefined && !flags.is(Flags.Protected)

      val hidden: Boolean =
        flags.is(Flags.Private) || flags.is(Flags.PrivateLocal) || flags.is(Flags.Artifact) ||
          (flags.is(Flags.Synthetic) && !flags.is(Flags.Module)) || packagePrivate

      val generated: Boolean =
        name == t"<init>" || name == t"_" || name.contains(t"$$default$$") ||
          name.starts(t"evidence$$") || name.contains(t"$$anon") || name == t"$$package"

      hidden || generated || symbol.isLocalDummy

    // A module class's name without its `$`; a package object's carrier is not an entity.
    private def plain(symbol: Symbol): Text =
      val name: Text = symbol.name.tt
      if name.ends(t"$$") then name.skip(1, Rtl) else name

    private def carrier(symbol: Symbol): Boolean = plain(symbol).ends(t"$$package")

    // The designator of a symbol as a parent: packages are tops and terms, a module class a
    // term, any other class a type; a package object's carrier is its package.
    private def isRoot(symbol: Symbol): Boolean =
      symbol.isNoSymbol || symbol == defn.RootPackage || symbol == defn.RootClass ||
        symbol.name.tt == t"<root>" || symbol.name.tt == t"<empty>"

    private def designator(symbol: Symbol): Optional[Designator] =
      if isRoot(symbol) then Unset
      else if carrier(symbol) then designator(symbol.owner)
      else
        val name: Text = plain(symbol)
        val parent: Optional[Designator] = designator(symbol.owner)
        val term: Boolean = symbol.isPackageDef || symbol.flags.is(Flags.Module) || symbol.isTerm

        parent.lay(Designator.Top(name)): (parent: Designator) =>
          if term then Designator.Term(parent, name) else Designator.Type(parent, name)

    // An entity collected, changed where it has been.
    private def amend(key: Text)(change: Api.Entity => Api.Entity): Unit =
      entities().at(key).let { entity => entities() = entities().define(key, change(entity)) }

    // What an export forwarder forwards to: the member its right-hand side selects, beneath the
    // arguments it passes on; nothing where the tree cannot be read.
    private def forwarded(symbol: Symbol): Optional[Designator] =
      def target(tree: Tree): Optional[Symbol] = tree match
        case Apply(fun, _)       => target(fun)
        case TypeApply(fun, _)   => target(fun)
        case Typed(expr, _)      => target(expr)
        case Inlined(_, _, expr) => target(expr)
        case Select(_, _)        => tree.symbol
        case Ident(_)            => tree.symbol
        case _                   => Unset

      safely:
        reading:
          // The compiler's trees hold an optional right-hand side as the standard `Option`.
          symbol.tree match
            case DefDef(_, _, _, Some(rhs)) => target(rhs).let(designator(_))
            case ValDef(_, _, Some(rhs))    => target(rhs).let(designator(_))
            case _                          => Unset

    // An exported term is a page elsewhere, as an exported type is; the first forwarder to say
    // where names it for both.
    private def forward(parent: Optional[Designator], name: Text, symbol: Symbol): Unit =
      if symbol.flags.is(Flags.Exported) then forwarded(symbol).let: (target: Designator) =>
        amend(Api.key(parent.let(Locator.of), name)): (entity: Api.Entity) =>
          if entity.alias.present then entity else entity.copy(alias = target)

    // `into` is a flag the compiler keeps, which reflection does not offer.
    private def isInto(symbol: Symbol): Boolean =
      safely:
        reading:
          import dotty.tools.dotc.core.Symbols.toDenot
          given dotty.tools.dotc.core.Contexts.Context = context

          symbol match
            case symbol: dotty.tools.dotc.core.Symbols.Symbol =>
              symbol.is(dotty.tools.dotc.core.Flags.Into)

            case _ =>
              false

      . or(false)

    // Whether a class, or an object's class, extends an annotation class.
    private def isAnnotation(symbol: Symbol): Boolean =
      val annotations: Set[Text] =
        Set(t"scala.annotation.Annotation", t"java.lang.annotation.Annotation")

      safely:
        reading:
          val module: Boolean = symbol.flags.is(Flags.Module) && !symbol.isClassDef
          val cls: Symbol = if module then symbol.moduleClass else symbol

          cls.isClassDef && List.from(cls.typeRef.baseClasses).exists: base =>
            annotations.has(base.fullName.tt)

      . or(false)

    private def modifiers(symbol: Symbol): List[Text] =
      val flags = symbol.flags

      val pairs: List[(Boolean, Text)] = List(
        (flags.is(Flags.Protected), t"protected"),
        (flags.is(Flags.Override), t"override"),
        (flags.is(Flags.Abstract) && !flags.is(Flags.Trait) && !symbol.isTypeDef, t"abstract"),
        (flags.is(Flags.Sealed), t"sealed"),
        (flags.is(Flags.Open), t"open"),
        (flags.is(Flags.Final) && !flags.is(Flags.Module) && !flags.is(Flags.Package), t"final"),
        (flags.is(Flags.Erased), t"erased"),
        (flags.is(Flags.Transparent), t"transparent"),
        (flags.is(Flags.Inline), t"inline"),
        (flags.is(Flags.Lazy), t"lazy"),
        // The compiler also marks an object which holds an opaque type, which is not one itself.
        (flags.is(Flags.Opaque) && !flags.is(Flags.Module), t"opaque"),
        (flags.is(Flags.Infix), t"infix"),
        (isInto(symbol), t"into"),
        (isAnnotation(symbol), t"annotation"),
        (flags.is(Flags.JavaDefined) && !flags.is(Flags.Package), t"java"),
        (flags.is(Flags.Implicit), t"implicit"))

      pairs.filter(_(0)).map(_(1))

    private def location(symbol: Symbol): Optional[Api.Location] =
      safely:
        reading:
          symbol.pos.optional.let: pos =>
            Api.Location(pos.sourceFile.path.tt, pos.startLine + 1)

    // A type as stenography renders it; where stenography cannot (its named-tuple check
    // asserts on an applied type whose head is one of proscenium's opaque types, among
    // others), an applied type is rendered from its parts, and anything else is `?`, counted.
    private def syntax(repr: TypeRepr): Syntax =
      Inspection.rendered.since(_ + 1)

      safely(reading(Syntax(repr))).or:
        repr match
          case AppliedType(base, args) =>
            Inspection.fallbacks.since(_ + 1)
            Syntax.Application(syntax(base), List.from(args).map(syntax), false)

          case _ =>
            Inspection.unrendered.since(_ + 1)
            Syntax.Primitive(t"?")

    private def isAny(repr: TypeRepr): Boolean = repr.typeSymbol == defn.AnyClass
    private def isNothing(repr: TypeRepr): Boolean = repr.typeSymbol == defn.NothingClass

    // A name with the bounds of a type parameter, where they are not the trivial ones.
    private def bounded(name: Syntax, lower: TypeRepr, upper: TypeRepr): Syntax =
      val above: Syntax =
        if isNothing(lower) then name else Syntax.Infix(name, t">:", syntax(lower))

      if isAny(upper) then above else Syntax.Infix(above, t"<:", syntax(upper))

    // A type parameter with its bounds, where they are not the trivial ones, and its variance.
    private def typeParam(symbol: Symbol): Syntax =
      val variance: Text =
        if symbol.flags.is(Flags.Covariant) then t"+"
        else if symbol.flags.is(Flags.Contravariant) then t"-"
        else t""

      val name: Syntax = Syntax.Symbolic(t"$variance${symbol.name.tt}")

      symbol.info match
        case TypeBounds(lower, upper) => bounded(name, lower, upper)
        case _                        => name

    // One clause of parameters, as `(x: T, y: U)`, `(using T)` or `[A <: B]`.
    private def clause(params: List[Symbol]): Optional[Syntax] = params.head.let: (head: Symbol) =>
      if head.isType then Syntax.Sequence('[', params.map(typeParam)) else
        val contextual: Boolean = head.flags.is(Flags.Given) || head.flags.is(Flags.Implicit)

        // A name the compiler made up (`evidence$1`, `x$3`) is not shown: the type alone is.
        val items: List[Syntax] = params.map: param =>
          val erased: Text = if param.flags.is(Flags.Erased) then t"erased " else t""
          val synthetic: Boolean = param.name.tt.contains('$') || param.flags.is(Flags.Synthetic)
          val shown: Text = if synthetic then erased.trim else t"$erased${param.name.tt}"
          Syntax.Named(contextual, shown, syntax(param.info.widenByName))

        Syntax.Sequence('(', items)

    // Clauses from the parameter symbols, so that a curried method, a polymorphic one and a
    // using clause each keep their shape; the result is what remains once each is applied.
    private def clauses(symbol: Symbol): List[Syntax] = clauseList(paramLists(symbol))

    private def clauseList(paramss: List[List[Symbol]]): List[Syntax] =
      paramss.bind { params => clause(params).lay(Nil)(List(_)) }

    private def result(repr: TypeRepr): TypeRepr = repr match
      case MethodType(_, _, result) => this.result(result)
      case PolyType(_, _, result)   => this.result(result)
      case ByNameType(result)       => this.result(result)
      case other                    => other

    // The parents every class or case class has without saying so, which are no bases either.
    private val ignored: Set[Text] =
      Set
        ( t"Any", t"AnyRef", t"Object", t"Matchable", t"Product", t"Equals", t"Serializable",
          t"AnyVal" )

    // A class's declared parents, the ones every class has left out: from the tree where there
    // is one (the project's own code); else from the class's info, which a symbol loaded from
    // the classpath has without a tree.
    private def parentTypes(symbol: Symbol): List[TypeRepr] =
      def typeOf(tree: Tree): List[TypeRepr] = tree match
        case tree: TypeTree => List(tree.tpe)
        case term: Term     => List(term.tpe)
        case _              => Nil

      def keep(types: List[TypeRepr]): List[TypeRepr] =
        types.filter: tpe => !ignored.has(tpe.typeSymbol.name.tt)

      val fromTree: List[TypeRepr] =
        safely:
          reading:
            symbol.tree match
              case ClassDef(_, _, parentTrees, _, _) => keep(List.from(parentTrees).bind(typeOf))
              case _                                 => Nil

        . or(Nil)

      if !fromTree.nil then fromTree else
        safely:
          reading:
            import dotty.tools.dotc.core.Symbols.toDenot
            given dotty.tools.dotc.core.Contexts.Context = context

            symbol match
              case symbol: dotty.tools.dotc.core.Symbols.Symbol => symbol.info match
                case info: dotty.tools.dotc.core.Types.ClassInfo =>
                  keep(List.from(info.declaredParents).map(Reflection.typeRepr(using quotes)(_)))

                case _ =>
                  Nil

              case _ =>
                Nil

        . or(Nil)

    private def parents(symbol: Symbol): List[Syntax] = parentTypes(symbol).map(syntax)

    // A `derives` clause leaves no trace on its class: what remains is a synthetic given in the
    // companion for each type class, `derived$Show`, whose result names it.
    private def derivations(symbol: Symbol): List[Syntax] =
      def typeClass(repr: TypeRepr): Syntax = repr match
        case PolyType(_, _, result)   => typeClass(result)
        case MethodType(_, _, result) => typeClass(result)
        case ByNameType(result)       => typeClass(result)
        case AppliedType(base, _)     => syntax(base)
        case other                    => syntax(other)

      safely:
        reading:
          val companion: Symbol = symbol.companionModule

          if companion.isNoSymbol then Nil else
            val instances: List[Symbol] =
              List.from(companion.moduleClass.declarations).filter(_.name.tt.starts(t"derived$$"))

            instances.map { instance => typeClass(instance.info) }

      . or(Nil)

    // A `uses` clause is kept as `@retains` annotations: on the class for what it retains, and on
    // its constructor for what is used `initially`.
    private def usages(symbol: Symbol): List[Syntax] =
      def elements(repr: TypeRepr): List[TypeRepr] = repr match
        case OrType(left, right) =>
          elements(left) + elements(right)

        case other =>
          if other =:= TypeRepr.of[Nothing] then Nil else List(other)

      def retained(owner: Symbol): List[TypeRepr] =
        List.from(owner.annotations).bind: annotation =>
          annotation.tpe match
            case AppliedType(base, arguments)
            if base.typeSymbol.fullName.tt == t"scala.annotation.retains" =>
              List.from(arguments) match
                case List(argument) => elements(argument)
                case _              => Nil

            case _ =>
              Nil

      // What can be named from outside is linked; a parameter or a local is its name alone.
      def reference(repr: TypeRepr): Syntax = repr match
        case ref@TermRef(_, name) =>
          val term: Symbol = ref.termSymbol
          val owner: Symbol = term.owner

          val named: Optional[Designator] =
            if owner.isPackageDef || owner.flags.is(Flags.Module) then designator(term) else Unset

          named.lay(Syntax.Primitive(name.tt))(Syntax.Simple(_))

        case other =>
          syntax(other)

      safely:
        reading:
          val kept: List[Syntax] = retained(symbol).map(reference)

          val initial: List[Syntax] =
            val constructor: Symbol = symbol.primaryConstructor

            if constructor.isNoSymbol then Nil
            else retained(constructor).map { repr => Syntax.Suffix(reference(repr), t" initially") }

          kept + initial

      . or(Nil)

    // The keys of every class a class extends, nearest first, the universal ones left out —
    // except `AnyVal`, which tells search that a value class is a value type.
    private def bases(symbol: Symbol): List[Text] =
      def kept(base: Symbol): Boolean =
        base != symbol && (base.name.tt == t"AnyVal" || !ignored.has(base.name.tt))

      def key(base: Symbol): List[Text] = designator(base).lay(Nil) { d => List(Api.key(d)) }

      safely(reading(List.from(symbol.typeRef.baseClasses).filter(kept).bind(key))).or(Nil)

    // A type's shape (`Api.Shape`), as matching reads it: a class, an alias or an opaque type
    // named by its entity's key with its arguments (an alias is not seen through here, since a
    // symbol met through the session's symbol table does not say whether it is opaque; its
    // declaration records what it stands for, and `Applicables` expands it), a type parameter
    // with its upper bound (none when it is `Any`), a wildcard as its bound, and anything else
    // `Other`. The modularity form `T is Typeclass` is the refinement `Typeclass { type Self =
    // T }`, which is read as `Typeclass[T]`; an alias of one (`is`, prepositional's `by`) is
    // seen through to it, and any other refinement is its parent.
    //
    // A type parameter's bound is read only where the parameter is not already being bounded:
    // an F-bounded one (`E extends Enum<E>`, as Java declares many) refers to itself, and its
    // bound is not expanded again; and a shape deeper than any written type is `Other`.
    private def shape(repr: TypeRepr): Api.Shape = shape(repr, Set(), 0)

    private def shape(repr: TypeRepr, bounding: Set[Text], depth: Int): Api.Shape =
      def shape(repr: TypeRepr): Api.Shape = Walker.this.shape(repr, bounding, depth + 1)

      def param(name: Text, upper: => Optional[TypeRepr]): Api.Shape =
        if bounding.has(name) then Api.Shape.Param(name) else
          val bounded: Optional[Api.Shape] = upper.let: (upper: TypeRepr) =>
            if isAny(upper) then Unset
            else Walker.this.shape(upper, bounding + Set(name), depth + 1)

          Api.Shape.Param(name, bounded)

      // The parameter of a binder at an index, with its upper bound.
      def parameter(names: List[Text], bounds: List[TypeBounds], index: Int): Api.Shape =
        names.zip(bounds).at(index.z).lay(Api.Shape.Other): (name, bound) =>
          param(name, bound.hi)

      def named(symbol: Symbol): Api.Shape =
        designator(symbol).lay(Api.Shape.Other): (d: Designator) => Api.Shape.Named(Api.key(d))

      def read(repr: TypeRepr): Api.Shape = repr match
        case ByNameType(inner)       => shape(inner)
        case AnnotatedType(inner, _) => shape(inner)
        case TypeBounds(_, upper)    => shape(upper)

        // A singleton type (an export forwarder's, `x.type`) is the type of its value.
        case term: TermRef if !term.termSymbol.flags.is(Flags.Module) => shape(term.widen)

        case Refinement(parent, name, info) =>
          if name.tt != t"Self" then shape(parent) else shape(parent) match
            case Api.Shape.Named(key, args) => Api.Shape.Named(key, shape(info) :: args)
            case other                      => other

        case AppliedType(base, args) =>
          repr.dealias match
            case refined: Refinement => shape(refined)

            case _ => shape(base) match
              case Api.Shape.Named(key, _) => Api.Shape.Named(key, List.from(args).map(shape))
              case other                   => other

        // A method's type parameter, or a type lambda's (a parameterised alias's right-hand
        // side refers to its parameters so).
        case ParamRef(binder, index) => binder match
          case PolyType(names, bounds, _) =>
            parameter(List.from(names).map(_.tt), List.from(bounds), index)

          case TypeLambda(names, bounds, _) =>
            parameter(List.from(names).map(_.tt), List.from(bounds), index)

          case _ => Api.Shape.Other

        case _ =>
          val symbol: Symbol = repr.typeSymbol
          val dealiased: TypeRepr = repr.dealias

          if symbol.isNoSymbol then Api.Shape.Other
          else if symbol.isTypeParam || symbol.flags.is(Flags.Param) then
            val upper: Optional[TypeRepr] = symbol.info match
              case TypeBounds(_, upper) => upper
              case _                    => Unset

            param(symbol.name.tt, upper)
          else if symbol.isClassDef || symbol.isTypeDef then
            // An alias of a refinement (`type Showable2 = Showable { type Self = Int }`).
            dealiased match
              case refined: Refinement if symbol.isAliasType => shape(refined)
              case _                                         => named(symbol)
          else
            Api.Shape.Other

      if depth > 24 then Api.Shape.Other else safely(reading(read(repr))).or(Api.Shape.Other)

    // A given object's `Self` member, where it has one: `given text: Text is Textual:` is an
    // object extending `Textual` whose member `Self` is `Text`.
    private def objectSelf(module: Symbol): Optional[TypeRepr] =
      def isSelf(d: Symbol): Boolean = d.isTypeDef && d.name.tt == t"Self"
      safely(reading(List.from(module.declarations).seek(isSelf).let(_.info)))

    // What a given object provides: its first parent, with the type it is for as its first
    // argument where the parent does not say.
    private def objectProvides(module: Symbol): Optional[Api.Shape] =
      parentTypes(module).head.let(shape) match
        case Api.Shape.Named(key, args) =>
          val self: Optional[Api.Shape] = objectSelf(module).let(shape)
          val selfIsArg: Boolean = args.head.let { a => self.let(_ == a).or(false) }.or(false)

          if selfIsArg then Api.Shape.Named(key, args)
          else Api.Shape.Named(key, self.lay(args)(_ :: args))

        case other => other

    // A given object's type as it was written: `Self is Parent` where it has a `Self`, else
    // its first parent.
    private def objectType(module: Symbol): Optional[Syntax] =
      parentTypes(module).head.let(syntax).let: (parent: Syntax) =>
        objectSelf(module).lay(parent): (s: TypeRepr) => Syntax.Infix(syntax(s), t"is", parent)

    private def isContextual(param: Symbol): Boolean =
      param.isTerm && (param.flags.is(Flags.Given) || param.flags.is(Flags.Implicit))

    // Whether a clause is a plain term clause: its first parameter a term, neither `using` nor
    // implicit.
    private def isPlain(params: List[Symbol]): Boolean =
      params.head.let { p => p.isTerm && !isContextual(p) }.or(false)

    // The shape of every term parameter, in order, `using` ones and an extension's receiver
    // among them: what search matches a `-Type` criterion against.
    private def accepts(symbol: Symbol): List[Api.Shape] =
      paramLists(symbol).flat.filter(_.isTerm).map { p => shape(p.info.widenByName) }

    // What an extension extends — the one parameter of its first plain term clause — and what
    // it, or a given, requires: the type of each `using` parameter, a context bound being one.
    private def matching(symbol: Symbol, isExtension: Boolean)
    :   (Optional[Api.Shape], List[Api.Shape]) =

      val all: List[List[Symbol]] = paramLists(symbol)

      val receiver: Optional[Api.Shape] =
        if !isExtension then Unset
        else all.seek(isPlain).let(_.head).let { param => shape(param.info.widenByName) }

      val declared: List[Api.Shape] =
        all.bind(_.filter(isContextual).map { p => shape(p.info.widenByName) })

      // An extension declared in a class or trait (`trait Functor[F[_]]: extension …`) is
      // reached through an instance of it: it requires one, of the class applied to its type
      // parameters.
      val owner: Symbol = symbol.owner
      val modular: Boolean = owner.flags.is(Flags.Module) || owner.flags.is(Flags.Package)

      val enclosing: List[Api.Shape] =
        if !isExtension || !owner.isClassDef || modular then Nil
        else designator(owner).lay(Nil: List[Api.Shape]): (d: Designator) =>
          val params: List[Api.Shape] =
            paramLists(owner.primaryConstructor).flat.filter(_.isType).map: p =>
              Api.Shape.Param(p.name.tt)

          List(Api.Shape.Named(Api.key(d), params))

      (receiver, enclosing + declared)

    private def templateKind(symbol: Symbol): Api.Kind =
      val flags = symbol.flags

      if flags.is(Flags.Trait) then Api.Kind.Trait
      else if flags.is(Flags.Enum) && flags.is(Flags.Case) then Api.Kind.Case
      else if flags.is(Flags.Enum) then Api.Kind.Enum
      else if flags.is(Flags.Case) then Api.Kind.CaseClass
      else Api.Kind.Class

    // The extension's own clauses (up to and including the first plain term clause) and the
    // method's.
    private def extension(symbol: Symbol): (List[Syntax], List[Syntax]) =
      val all: List[List[Symbol]] = paramLists(symbol)

      all.where(isPlain).lay((clauseList(all), List.empty[Syntax])): (index: Ordinal) =>
        (clauseList(all.keep(index.n1)), clauseList(all.skip(index.n1)))

    private def record
      ( parent:      Optional[Designator],
        name:        Text,
        own:         Boolean,
        declaration: Optional[Api.Declaration],
        isType:      Boolean,
        where:       Optional[Api.Location],
        bases:       List[Text]               = Nil )
    :   Unit =

      val key: Text = Api.key(parent.let(Locator.of), name)

      val recorded: Api.Entity = entities().at(key) match
        case entity: Api.Entity =>
          declaration.lay(entity): (declaration: Api.Declaration) =>
            val slot: Optional[Api.Declaration] =
              if isType then entity.template else entity.definition

            val parameter: Boolean = declaration.kind.isParameter

            val other: Optional[Api.Declaration] =
              entity.declarations.seek(_.kind.isParameter != parameter)

            other.let: (other: Api.Declaration) =>
              Inspection.clashes0.since((key, other.kind, declaration.kind) :: _)

            val owned: Boolean = entity.own || own
            val located: Optional[Api.Location] = where.or(entity.location)
            val kept: Optional[Api.Location] = entity.location.or(where)
            def defines(kind: Api.Kind): Boolean = entity.definition.let(_.kind == kind).or(false)

            // A parameter is one entity however many overloads declare it, and a member of its
            // name — a case class's field, which is its constructor's parameter — keeps the
            // entity, whichever is met first.
            if parameter && (slot.present || other.present) then entity
            else if !parameter && slot.let(_.kind.isParameter).or(false) then
              if isType then entity.copy(own = owned, template = declaration, location = located)
              else entity.copy(own = owned, definition = declaration, location = located)
            // A package is met once per member loaded beneath it; it is declared once.
            else if declaration.kind == Api.Kind.Package && entity.definition.present then
              entity.copy(own = owned)
            // A given with a body (`given foo: Show[Doc] with …`) is a value and an object of
            // one name: the given is the declaration, the object no overload of it.
            else if declaration.kind == Api.Kind.Given && defines(Api.Kind.Object) then
              entity.copy(own = owned, definition = declaration, location = kept)
            else if declaration.kind == Api.Kind.Object && defines(Api.Kind.Given) then
              entity.copy(own = owned)
            else if isType then
              entity.copy
                ( own      = owned,
                  template = entity.template.or(declaration),
                  location = kept,
                  bases    = if entity.bases.nil then bases else entity.bases )
            else if entity.definition.absent then
              entity.copy(own = owned, definition = declaration, location = kept)
            else
              entity.copy(own = owned, overloads = entity.overloads + List(declaration))

        case _ =>
          val locator: Optional[Locator] = parent.let(Locator.of)

          if isType
          then Api.Entity(locator, name, own, Unset, declaration, location = where, bases = bases)
          else Api.Entity(locator, name, own, declaration, Unset, location = where)

      entities() = entities().define(key, recorded)

    // The parameters of a method, a constructor or a type alias, each a member of the type form
    // of what it parameterises (`parent`), from every clause: type parameters with their bounds,
    // term parameters with their types. A name the compiler made up (`x$1`, an evidence) is not
    // the API.
    private def parameters(parent: Designator, clauses: List[List[Symbol]], own: Boolean): Unit =
      if own || Inspection.everyParameter then clauses.each: params =>
        params.each: param =>
          if !skipped(param) && !param.name.tt.contains('$') then
            val name: Text = param.name.tt
            val where: Optional[Api.Location] = if own then location(param) else Unset

            if param.isType then
              val bounds: Optional[Syntax] = typeResult(param.info, param.flags)

              val declaration: Api.Declaration =
                Api.Declaration(Api.Kind.TypeParam, Nil, Nil, bounds)

              record(parent, name, own, declaration, true, where)
            else
              val contextual: List[Text] =
                if param.flags.is(Flags.Given) then List(t"using") else Nil

              val mods: List[Text] = contextual + modifiers(param)
              val tpe: Syntax = syntax(param.info.widenByName)
              val declaration: Api.Declaration = Api.Declaration(Api.Kind.Param, mods, Nil, tpe)
              record(parent, name, own, declaration, false, where)

    private def typeForm(parent: Optional[Designator], name: Text): Designator =
      parent.lay(Designator.Top(name)): (parent: Designator) => Designator.Type(parent, name)

    private def overrides(symbol: Symbol): Boolean =
      safely(reading(!symbol.allOverriddenSymbols.isEmpty)).or(false)

    private def member(parent: Optional[Designator], symbol: Symbol, own: Boolean): Unit =
      if !skipped(symbol) then
        val name: Text = plain(symbol)
        val where: Optional[Api.Location] = if own then location(symbol) else Unset
        val flags = symbol.flags
        val inherited: Boolean = overrides(symbol)

        if symbol.isClassDef then template(symbol, own)
        // A class's type parameter is its constructor's, which `template` records.
        else if symbol.isTypeDef && flags.is(Flags.Param) then ()
        else if symbol.isTypeDef then
          // An alias is bounds whose two ends are the same; a parameterised one a lambda.
          val aliased: TypeRepr = symbol.info match
            case TypeBounds(lower, upper) if lower =:= upper => lower
            case other                                       => other

          val (params, rhs) = aliased match
            case TypeLambda(names, bounds, body) =>
              val typeParams: List[Syntax] =
                List.from(names).zip(List.from(bounds)).map: (name, bound) =>
                  bounded(Syntax.Symbolic(name.tt), bound.low, bound.hi)

              (List(Syntax.Sequence('[', typeParams)), typeResult(body, flags))

            case other =>
              (Nil, typeResult(other, flags))

          // What a non-opaque alias stands for, over its parameters, for matching to expand.
          val noAlias: Optional[Api.Shape] = Unset
          val noParams: List[Text] = Nil

          val (aliasOf, aliasParams) =
            if flags.is(Flags.Opaque) then (noAlias, noParams) else aliased match
              case TypeLambda(names, _, body) =>
                val target: Optional[Api.Shape] = shape(body)
                val params: List[Text] = List.from(names).map(_.tt)
                (target, params)

              case TypeBounds(_, _) => (noAlias, noParams)

              case other =>
                val target: Optional[Api.Shape] = shape(other)
                (target, noParams)

          val declaration: Api.Declaration =
            Api.Declaration
              ( Api.Kind.Type,
                modifiers(symbol),
                params,
                rhs,
                aliasOf     = aliasOf,
                aliasParams = aliasParams,
                exported    = flags.is(Flags.Exported) )

          record(parent, name, own, declaration, true, where)

          aliased match
            case TypeLambda(names, bounds, _) if own || Inspection.everyParameter =>
              List.from(names).zip(List.from(bounds)).each: (param, bound) =>
                val bounds: Optional[Syntax] = typeResult(bound, Flags.EmptyFlags)

                val declaration: Api.Declaration =
                  Api.Declaration(Api.Kind.TypeParam, Nil, Nil, bounds)

                record(typeForm(parent, name), param.tt, own, declaration, true, Unset)

            case _ =>
              ()

          // An exported type is a page elsewhere: a reference through the export leads there.
          if flags.is(Flags.Exported) then rhs match
            case Syntax.Simple(target) =>
              amend(Api.key(parent.let(Locator.of), name))(_.copy(alias = target))

            case _ =>
              ()
        else if symbol.isDefDef then
          val kind: Api.Kind =
            if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.ExtensionMethod) then Api.Kind.Extension
            else Api.Kind.Def

          val none: List[Syntax] = Nil

          val (group, own0) =
            if kind == Api.Kind.Extension then extension(symbol) else (none, clauses(symbol))

          val noShape: Optional[Api.Shape] = Unset
          val noShapes: List[Api.Shape] = Nil

          val (receiver, requires) =
            if kind == Api.Kind.Def then (noShape, noShapes)
            else matching(symbol, kind == Api.Kind.Extension)

          val returns: Api.Shape = shape(result(symbol.info))
          val provides: Optional[Api.Shape] = if kind == Api.Kind.Given then returns else Unset

          val declaration: Api.Declaration =
            Api.Declaration
              ( kind,
                modifiers(symbol),
                own0,
                syntax(result(symbol.info)),
                Nil,
                group,
                inherited,
                receiver,
                requires,
                provides,
                exported = flags.is(Flags.Exported),
                returns  = returns,
                accepts  = accepts(symbol) )

          record(parent, name, own, declaration, false, where)
          forward(parent, name, symbol)
          parameters(typeForm(parent, name), paramLists(symbol), own)
        else if symbol.isValDef then
          val kind: Api.Kind =
            if flags.is(Flags.Enum) && flags.is(Flags.Case) then Api.Kind.Case
            else if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.Mutable) then Api.Kind.Var
            else Api.Kind.Val

          // An enum's value case has the enum as its type, which its page already says.
          val tpe: Optional[Syntax] =
            if kind == Api.Kind.Case then Unset else syntax(result(symbol.info))

          // A given with a body is a value of its own object's type, which provides the object's
          // first parent, the type as it was written; a given is lazy by nature, not by
          // declaration.
          val provides: Optional[Api.Shape] =
            if kind != Api.Kind.Given then Unset else
              val tpeSymbol: Symbol = result(symbol.info).typeSymbol

              if tpeSymbol.flags.is(Flags.Module) then objectProvides(tpeSymbol)
              else shape(result(symbol.info))

          val shownType: Optional[Syntax] =
            if kind == Api.Kind.Given && result(symbol.info).typeSymbol.flags.is(Flags.Module)
            then objectType(result(symbol.info).typeSymbol).or(tpe)
            else tpe

          val declared: List[Text] = modifiers(symbol)

          val mods: List[Text] =
            if kind == Api.Kind.Given then declared.filter(_ != t"lazy") else declared

          val declaration: Api.Declaration =
            Api.Declaration
              ( kind,
                mods,
                Nil,
                shownType,
                Nil,
                Nil,
                inherited,
                provides = provides,
                exported = flags.is(Flags.Exported),
                returns  = shape(result(symbol.info)) )

          record(parent, name, own, declaration, false, where)
          forward(parent, name, symbol)

    // An alias's right-hand side, a bound type's bounds, or nothing for an opaque type's
    // underlying (which is not the API).
    private def typeResult(repr: TypeRepr, flags: Flags): Optional[Syntax] = repr match
      case TypeBounds(lower, upper) if lower =:= upper =>
        if flags.is(Flags.Opaque) then Unset else syntax(lower)

      case TypeBounds(lower, upper) =>
        val lo: Optional[Syntax] =
          if isNothing(lower) then Unset else Syntax.Prefix(t">:", syntax(lower))

        val hi: Optional[Syntax] =
          if isAny(upper) then Unset else Syntax.Prefix(t"<:", syntax(upper))

        val both: List[Syntax] = lo.lay(Nil)(List(_)) + hi.lay(Nil)(List(_))
        if both.nil then Unset else Syntax.Compound(both)

      case other => syntax(other)

    private def template(symbol: Symbol, own: Boolean): Unit =
      val here: Optional[Designator] = designator(symbol)
      val where: Optional[Api.Location] = if own then location(symbol) else Unset

      if !carrier(symbol) then
        val name: Text = plain(symbol)
        val parent: Optional[Designator] = designator(symbol.owner)

        if symbol.flags.is(Flags.Module) then
          // A given object (`given foo: Show[Doc] with …`) is a given, which provides its
          // first parent.
          if symbol.flags.is(Flags.Given) then
            val mods: List[Text] = modifiers(symbol)
            val tpe: Optional[Syntax] = objectType(symbol)
            val provides: Optional[Api.Shape] = objectProvides(symbol)

            val declaration: Api.Declaration =
              Api.Declaration(Api.Kind.Given, mods, Nil, tpe, parents(symbol), provides = provides)

            record(parent, name, own, declaration, false, where)
          else
            val declaration: Api.Declaration =
              Api.Declaration
                ( Api.Kind.Object,
                  modifiers(symbol),
                  Nil,
                  Unset,
                  parents(symbol),
                  uses = usages(symbol.moduleClass) )

            record(parent, name, own, declaration, false, where)
        else
          val kind: Api.Kind = templateKind(symbol)
          val primary: Symbol = symbol.primaryConstructor
          val bare: Boolean = kind == Api.Kind.Trait && paramLists(primary).all(_.nil)
          val constructor: List[Syntax] = if bare then Nil else clauses(primary)
          val parentShapes: List[Api.Shape] = parentTypes(symbol).map(shape)

          val declaration: Api.Declaration =
            Api.Declaration
              ( kind,
                modifiers(symbol),
                constructor,
                Unset,
                parents(symbol),
                accepts      = accepts(primary),
                parentShapes = parentShapes,
                derives      = derivations(symbol),
                uses         = usages(symbol) )

          record(parent, name, own, declaration, true, where, bases(symbol))
          here.let: (here: Designator) => parameters(here, paramLists(primary), own)

      // A member which cannot be read is left out, and the rest are still walked.
      List.from(symbol.declarations).each: child =>
        recover:
          case error: ReadError => Inspection.skipped(error.cause)

        . protect(reading(member(here, child, own)))

    private def packageEntity(symbol: Symbol): Unit =
      if symbol.isPackageDef || carrier(symbol) then designator(symbol).let(packageOf)

    private def packageOf(designator: Designator): Unit =
      val declaration: Api.Declaration = Api.Declaration(Api.Kind.Package)
      record(designator.parent, designator.name, false, declaration, false, Unset)
      designator.parent.let(packageOf)

    // A top-level class or module of a dependency, by the name its `.tasty` entry gives,
    // loaded from the classpath; its packages are recorded with it.
    def load(name: Text, java: Boolean = false): Unit =
      import dotty.tools.dotc.core.Names

      // A package object's carrier (`pkg_core$package`) is a module, named without its `$`.
      val isModule: Boolean = name.ends(t"$$") || name.ends(t"$$package")
      val plainName: Text = if name.ends(t"$$") then name.skip(1, Rtl) else name

      // A name looked up in the session's symbol table without a stub being made for one it
      // does not have (as `requiredClass` and `requiredModule` would, which the session would
      // then meet as a bad symbolic reference when it compiles a fragment).
      def static(name: Names.Name): Optional[Symbol] =
        safely:
          reading:
            given dotty.tools.dotc.core.Contexts.Context = context

            val denotation =
              dotty.tools.dotc.core.Denotations.staticRef(name, generateStubs = false)

            if denotation.exists && denotation.symbol.exists
            then Reflection.symbol(using quotes)(denotation.symbol)
            else Unset

      // One `.tasty` holds a class and its companion object, or an object alone, which the
      // symbol table keeps apart: both are walked.
      val module: Optional[Symbol] =
        static(Names.termName(plainName.s)).let: (m: Symbol) =>
          if m.isNoSymbol then Unset else m.moduleClass

      val cls: Optional[Symbol] = if isModule then Unset else static(Names.typeName(plainName.s))

      // A Java class which is not public is not part of the API.
      def kept(symbol: Symbol): Boolean = !java || !skipped(symbol)

      (cls.lay(Nil)(List(_)) + module.lay(Nil)(List(_))).filter(kept).each: (symbol: Symbol) =>
        safely:
          reading:
            packageEntity(symbol.owner)
            template(symbol, false)

    def walk(tree: Tree, own: Boolean): Unit = tree match
      case PackageClause(pid, statements) =>
        packageEntity(pid.symbol)
        List.from(statements).each(walk(_, own))

      case classDef: ClassDef =>
        template(classDef.symbol, own)

      case _ =>
        ()
