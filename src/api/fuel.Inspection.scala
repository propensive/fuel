                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                 ╭─────────╮╭───╮   ╭───╮╭─────────╮╭───╮                         ┃
┃                                 │   ╭─────╯│   │   │   ││   ╭─────╯│   │                         ┃
┃                                 │   ╰───╮  │   │   │   ││   ╰───╮  │   │                         ┃
┃                                 │   ╭───╯  │   │   │   ││   ╭───╯  │   │                         ┃
┃                                 │   │      │   ╰───╯   ││   ╰─────╮│   ╰─────╮                   ┃
┃                                 ╰───╯      ╰───────────╯╰─────────╯╰─────────╯                   ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃    FUEL, version 0.1.0.                                                                          ┃
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

import java.util.concurrent as juc

import scala.jdk.CollectionConverters.*
import scala.quoted.Quotes
import scala.tasty.inspector as sti

import scala.collection.immutable as sci

import soundness.{Language as _, Token as _, *}

import dysasymptotics.linearSize
import stenography.{Bindings, Designator, Syntax}

// The API read from compiled code: the compiler's own TASTy unpickler, through the inspector,
// over the project's `.tasty` files and its dependencies' jars, against the classpath which
// carries everything they reference (the Scala library included). Each package, class, object,
// method, value and type becomes an `Api.Entity` with its declarations' signatures as
// stenography syntax, as Amok's walker made them. Runs on the compiler's thread
// (`Checking.exclusive`): dotc pins its context to the thread of first use.
object Inspection:
  // The members left out of the model because reading them failed, and the first failure:
  // what a compile reports, so that a model with gaps says why.
  private val skipped0: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var firstFailure: Optional[Throwable] = Unset

  private def skipped(error: Throwable): Unit =
    skipped0.incrementAndGet()
    if firstFailure.absent then firstFailure = error

  // How many types were rendered, how many applied types from their parts because stenography
  // could not, and how many could not be at all and are shown as `?`.
  val rendered: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)
  val fallbacks: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)
  val unrendered: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)

  // The count and the first failure since the last call, which resets them.
  def failures(): (Int, Optional[Throwable]) =
    val result = (skipped0.getAndSet(0), firstFailure)
    firstFailure = Unset
    result

  // Whether a dependency's parameters are entities too, as the project's own always are: off,
  // since nothing documents them, except to survey where parameters meet members.
  @volatile
  var everyParameter: Boolean = false

  // The keys at which a parameter met a member of the same name (which the member keeps),
  // each with the two kinds, since the last call, which resets them.
  private val clashes0: juc.ConcurrentLinkedQueue[(Text, Api.Kind, Api.Kind)] = juc.ConcurrentLinkedQueue()

  def clashes(): List[(Text, Api.Kind, Api.Kind)] =
    val result = List.from(clashes0.iterator.nn.asScala.toList)
    clashes0.clear()
    result

  // The entities of every `.tasty` beneath the `directories` (the project's own, marked so)
  // and of every `jars` entry.
  def walk(directories: List[Text], jars: List[Text], classpath: List[Text]): Api | Throwable =
    val tastyFiles: List[Text] = directories.bind(tastyIn)

    Checking.exclusive:
      val collected: juc.ConcurrentHashMap[Text, Api.Entity] = juc.ConcurrentHashMap()

      @scala.caps.unsafe.untrackedCaptures
      var failure: Optional[Throwable] = Unset

      object inspector extends sti.Inspector:
        def inspect(using quotes: Quotes)(tastys: scala.List[sti.Tasty[quotes.type]]): Unit =
          try
            val walker = Walker(collected)(using quotes)

            tastys.foreach: tasty =>
              // The tree is of the one `Quotes` the walker was made with, which the type of
              // `Tasty` says of itself but not of the walker.
              val tree: walker.quotes.reflect.Tree = tasty.ast.asInstanceOf[walker.quotes.reflect.Tree]
              walker.walk(tree, own = tasty.path.endsWith(".tasty") && !tasty.path.contains(".jar"))
          catch case error: Throwable => failure = error

      try
        val ok: Boolean =
          sti.TastyInspector.inspectAllTastyFiles
            (tastyFiles.stdlib.map(_.s), jars.stdlib.map(_.s), classpath.stdlib.map(_.s))(inspector)

        failure.or:
          val entities: sci.Map[Text, Api.Entity] =
            sci.Map.from(collected.entrySet.nn.iterator.nn.asScala.map { entry => (entry.getKey.nn, entry.getValue.nn) })

          Api(entities).relinked
      catch case error: Throwable => error

  // The entities of every `jars` entry, through a session's symbol table: each top-level
  // class the jar's `.tasty` entries name is loaded as the compiler loads any classpath
  // symbol — its signatures, never a body — which is what makes it robust where unpickling
  // whole trees is not; a class which cannot be loaded is left out.
  // The `shadowed` packages are the project's own, whose copies in a jar (a launcher assembly
  // carrying the project itself) are left to the project's sources: loading them into the
  // session ahead of the compile leaves it writing nothing.
  def classpath(jars: List[Text], context: dotty.tools.dotc.core.Contexts.Context, shadowed: List[Text] = Nil): Api | Throwable =
    Checking.exclusive:
      try
        // A context of the session's with a cache of quotes, which rendering a type through
        // stenography's quoted patterns needs (a run of the compiler sets one up; a bare
        // session context has none), initialised as every run initialises its own first.
        given dotty.tools.dotc.core.Contexts.Context = dotty.tools.dotc.quoted.QuotesCache.init(context.fresh)
        context.base.initialize()

        val quotes: Quotes = scala.quoted.runtime.impl.QuotesImpl()
        val collected: juc.ConcurrentHashMap[Text, Api.Entity] = juc.ConcurrentHashMap()
        val walker = Walker(collected)(using quotes)

        jars.each: (jar: Text) =>
          val zip = java.util.zip.ZipFile(jar.s)
          try
            val names: scala.List[String] =
              zip.entries.nn.asScala.map(_.getName.nn).filter(_.endsWith(".tasty")).toList

            // `pkg/Name.tasty` is the class, `pkg/Name$.tasty` its module; a `$package`
            // carrier holds a package's top-level members.
            names.foreach: name =>
              val full: String = name.substring(0, name.length - 6).nn.replace('/', '.').nn
              val pkg: Text = Text(full.substring(0, full.lastIndexOf('.').max(0)).nn)
              val isShadowed: Boolean = shadowed.exists { (p: Text) => pkg == p || pkg.starts(t"$p.") }
              if !isShadowed then walker.load(full.tt)
          finally zip.close()

        val entities: sci.Map[Text, Api.Entity] =
          sci.Map.from(collected.entrySet.nn.iterator.nn.asScala.map { entry => (entry.getKey.nn, entry.getValue.nn) })

        Api(entities).relinked
      catch case error: Throwable => error

  // Every `.tasty` beneath a directory, as the inspector takes them.
  def tastyIn(directory: Text): List[Text] =
    val root = java.nio.file.Path.of(directory.s).nn
    if !java.nio.file.Files.isDirectory(root) then Nil else
      val stream = java.nio.file.Files.walk(root).nn
      try List.from(stream.iterator.nn.asScala.map(_.toString.tt).filter(_.ends(t".tasty")).toList)
      finally stream.close()

  private class Walker(collected: juc.ConcurrentHashMap[Text, Api.Entity])(using val quotes: Quotes):
    import quotes.reflect.*

    given Bindings = Bindings()

    private def skipped(symbol: Symbol): Boolean =
      val name: String = symbol.name
      symbol.flags.is(Flags.Private) || symbol.flags.is(Flags.PrivateLocal) || symbol.flags.is(Flags.Artifact)
      || (symbol.flags.is(Flags.Synthetic) && !symbol.flags.is(Flags.Module))
      || name == "<init>" || name == "_" || name.contains("$default$") || name.startsWith("evidence$")
      || name.contains("$anon") || name == "$package" || symbol.isLocalDummy

    // A module class's name without its `$`; a package object's carrier is not an entity.
    private def plain(symbol: Symbol): String =
      val name = symbol.name
      if name.endsWith("$") then name.substring(0, name.length - 1).nn else name

    private def carrier(symbol: Symbol): Boolean = plain(symbol).endsWith("$package")

    // The designator of a symbol as a parent: packages are tops and terms, a module class a
    // term, any other class a type; a package object's carrier is its package.
    private def designator(symbol: Symbol): Optional[Designator] =
      if symbol.isNoSymbol || symbol == defn.RootPackage || symbol == defn.RootClass || symbol.name == "<root>" || symbol.name == "<empty>" then Unset
      else if carrier(symbol) then designator(symbol.owner)
      else
        val name: Text = plain(symbol).tt
        val parent: Optional[Designator] = designator(symbol.owner)

        if symbol.isPackageDef then parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Term(parent, name) }
        else if symbol.flags.is(Flags.Module) || symbol.isTerm then parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Term(parent, name) }
        else parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Type(parent, name) }

    private def modifiers(symbol: Symbol): List[Text] =
      val flags = symbol.flags

      val pairs: scala.List[(Boolean, Text)] = scala.List(
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
        (flags.is(Flags.Opaque), t"opaque"),
        (flags.is(Flags.Infix), t"infix"),
        (flags.is(Flags.Implicit), t"implicit"))

      List.from(pairs.collect { case (true, modifier) => modifier })

    private def location(symbol: Symbol): Optional[Api.Location] =
      try
        symbol.pos.map { (pos: Position) => Api.Location(pos.sourceFile.path.tt, pos.startLine + 1) }.getOrElse(Unset)
      catch case _: Throwable => Unset

    // A type as stenography renders it; where stenography cannot (its named-tuple check
    // asserts on an applied type whose head is one of proscenium's opaque types, among
    // others), an applied type is rendered from its parts, and anything else is `?`, counted.
    private def syntax(repr: TypeRepr): Syntax =
      Inspection.rendered.incrementAndGet()

      try Syntax(repr) catch case _: Throwable => repr match
        case AppliedType(base, args) =>
          Inspection.fallbacks.incrementAndGet()
          Syntax.Application(syntax(base), List.from(args.map(syntax)), false)
        case _ =>
          Inspection.unrendered.incrementAndGet()
          Syntax.Primitive(t"?")

    private def isAny(repr: TypeRepr): Boolean = repr.typeSymbol == defn.AnyClass
    private def isNothing(repr: TypeRepr): Boolean = repr.typeSymbol == defn.NothingClass

    // A type parameter with its bounds, where they are not the trivial ones, and its variance.
    private def typeParam(symbol: Symbol): Syntax =
      val variance: Text =
        if symbol.flags.is(Flags.Covariant) then t"+" else if symbol.flags.is(Flags.Contravariant) then t"-" else t""

      val name: Syntax = Syntax.Symbolic(t"$variance${symbol.name}")

      symbol.info match
        case TypeBounds(lower, upper) =>
          val bounded: Syntax = if isNothing(lower) then name else Syntax.Infix(name, t">:", syntax(lower))
          if isAny(upper) then bounded else Syntax.Infix(bounded, t"<:", syntax(upper))

        case _ =>
          name

    // One clause of parameters, as `(x: T, y: U)`, `(using T)` or `[A <: B]`.
    private def clause(params: scala.List[Symbol]): Optional[Syntax] = params.headOption match
      case None => Unset
      case Some(head) if head.isType => Syntax.Sequence('[', List.from(params.map(typeParam)))
      case Some(head) =>
        val contextual: Boolean = head.flags.is(Flags.Given) || head.flags.is(Flags.Implicit)

        val items: scala.List[Syntax] = params.map: param =>
          val erased: Text = if param.flags.is(Flags.Erased) then t"erased " else t""
          Syntax.Named(contextual, t"$erased${param.name}", syntax(param.info.widenByName))

        Syntax.Sequence('(', List.from(items))

    // Clauses from the parameter symbols, so that a curried method, a polymorphic one and a
    // using clause each keep their shape; the result is what remains once each is applied.
    private def clauses(symbol: Symbol): List[Syntax] =
      List.from(symbol.paramSymss.flatMap { params => clause(params).let(scala.List(_)).or(scala.Nil) })

    private def result(repr: TypeRepr): TypeRepr = repr match
      case MethodType(_, _, result) => this.result(result)
      case PolyType(_, _, result)   => this.result(result)
      case ByNameType(result)       => this.result(result)
      case other                    => other

    // The parents every class or case class has without saying so, which are no bases either.
    private val ignored: sci.Set[String] =
      sci.Set("Any", "AnyRef", "Object", "Matchable", "Product", "Equals", "Serializable", "AnyVal")

    // A class's declared parents, the ones every class has left out: from the tree where there
    // is one (the project's own code); else from the class's info, which a symbol loaded from
    // the classpath has without a tree.
    private def parentTypes(symbol: Symbol): scala.List[TypeRepr] =
      def typeOf(tree: Tree): Option[TypeRepr] = tree match
        case tree: TypeTree => Some(tree.tpe)
        case term: Term     => Some(term.tpe)
        case _              => None

      def keep(types: scala.List[TypeRepr]): scala.List[TypeRepr] =
        types.filter { tpe => !ignored.contains(tpe.typeSymbol.name) }

      val fromTree: scala.List[TypeRepr] =
        try
          symbol.tree match
            case ClassDef(_, _, parentTrees, _, _) => keep(parentTrees.flatMap(typeOf))
            case _                                 => scala.Nil
        catch case _: Throwable => scala.Nil

      if fromTree.nonEmpty then fromTree else
        try
          import dotty.tools.dotc.core.Symbols.toDenot
          given dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
          val dsym = symbol.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol]

          dsym.info match
            case info: dotty.tools.dotc.core.Types.ClassInfo =>
              keep(info.declaredParents.map(_.asInstanceOf[TypeRepr]))
            case _ => scala.Nil
        catch case _: Throwable => scala.Nil

    private def parents(symbol: Symbol): List[Syntax] = List.from(parentTypes(symbol).map(syntax))

    // The keys of every class a class extends, nearest first, the universal ones left out —
    // except `AnyVal`, which tells search that a value class is a value type.
    private def bases(symbol: Symbol): List[Text] =
      try
        val all: scala.List[Symbol] = symbol.typeRef.baseClasses.filter { base => base != symbol && (base.name == "AnyVal" || !ignored.contains(base.name)) }
        List.from(all.flatMap { base => designator(base).let { (d: Designator) => scala.List(Api.key(d)) }.or(scala.Nil) })
      catch case _: Throwable => Nil

    // A type's shape (`Api.Shape`), as matching reads it: a class, an alias or an opaque type
    // named by its entity's key with its arguments (an alias is not seen through here, since a
    // symbol met through the session's symbol table does not say whether it is opaque; its
    // declaration records what it stands for, and `Applicables` expands it), a type parameter
    // with its upper bound (none when it is `Any`), a wildcard as its bound, and anything else
    // `Other`. The modularity form `T is Typeclass` is the refinement `Typeclass { type Self =
    // T }`, which is read as `Typeclass[T]`; an alias of one (`is`, prepositional's `by`) is
    // seen through to it, and any other refinement is its parent.
    private def shape(repr: TypeRepr): Api.Shape =
      def bound(upper: TypeRepr): Optional[Api.Shape] = if isAny(upper) then Unset else shape(upper)
      def named(symbol: Symbol): Api.Shape = designator(symbol).lay(Api.Shape.Other) { (d: Designator) => Api.Shape.Named(Api.key(d)) }

      try repr match
        case ByNameType(inner)       => shape(inner)
        case AnnotatedType(inner, _) => shape(inner)
        case TypeBounds(_, upper)    => shape(upper)

        // A singleton type (an export forwarder's, `x.type`) is the type of its value.
        case term: TermRef if !term.termSymbol.flags.is(Flags.Module) => shape(term.widen)

        case Refinement(parent, name, info) =>
          if name != "Self" then shape(parent) else shape(parent) match
            case Api.Shape.Named(key, args) => Api.Shape.Named(key, shape(info) :: args)
            case other                      => other

        case AppliedType(base, args) =>
          repr.dealias match
            case refined: Refinement => shape(refined)
            case _ => shape(base) match
              case Api.Shape.Named(key, _) => Api.Shape.Named(key, List.from(args.map(shape)))
              case other                   => other

        // A method's type parameter, or a type lambda's (a parameterised alias's right-hand
        // side refers to its parameters so).
        case ParamRef(binder, index) => binder match
          case PolyType(names, bounds, _) =>
            val upper: Optional[Api.Shape] = bounds(index) match
              case TypeBounds(_, upper) => bound(upper)

            Api.Shape.Param(names(index).tt, upper)

          case TypeLambda(names, bounds, _) =>
            val upper: Optional[Api.Shape] = bounds(index) match
              case TypeBounds(_, upper) => bound(upper)

            Api.Shape.Param(names(index).tt, upper)

          case _ => Api.Shape.Other

        case _ =>
          val symbol: Symbol = repr.typeSymbol
          val dealiased: TypeRepr = repr.dealias

          if symbol.isNoSymbol then Api.Shape.Other
          else if symbol.isTypeParam || symbol.flags.is(Flags.Param) then
            val upper: Optional[Api.Shape] = symbol.info match
              case TypeBounds(_, upper) => bound(upper)
              case _                    => Unset

            Api.Shape.Param(symbol.name.tt, upper)
          else if symbol.isClassDef || symbol.isTypeDef then
            // An alias of a refinement (`type Showable2 = Showable { type Self = Int }`).
            dealiased match
              case refined: Refinement if symbol.isAliasType => shape(refined)
              case _                                         => named(symbol)
          else Api.Shape.Other
      catch case _: Throwable => Api.Shape.Other

    // A given object's `Self` member, where it has one: `given text: Text is Textual:` is an
    // object extending `Textual` whose member `Self` is `Text`.
    private def objectSelf(module: Symbol): Optional[TypeRepr] =
      try module.declarations.find { d => d.isTypeDef && d.name == "Self" }.map(_.info).getOrElse(Unset)
      catch case _: Throwable => Unset

    // What a given object provides: its first parent, with the type it is for as its first
    // argument where the parent does not say.
    private def objectProvides(module: Symbol): Optional[Api.Shape] =
      parentTypes(module).headOption.map(shape).getOrElse(Unset) match
        case Api.Shape.Named(key, args) =>
          val self: Optional[Api.Shape] = objectSelf(module).let(shape)
          val selfIsArg: Boolean = args.stdlib.headOption.exists { a => self.let(_ == a).or(false) }
          self.lay(Api.Shape.Named(key, args)) { (s: Api.Shape) => if selfIsArg then Api.Shape.Named(key, args) else Api.Shape.Named(key, s :: args) }

        case other => other

    // A given object's type as it was written: `Self is Parent` where it has a `Self`, else
    // its first parent.
    private def objectType(module: Symbol): Optional[Syntax] =
      parentTypes(module).headOption.map(syntax).getOrElse(Unset).let: (parent: Syntax) =>
        objectSelf(module).let { (self: TypeRepr) => Syntax.Infix(syntax(self), t"is", parent) }.or(parent)

    private def isContextual(param: Symbol): Boolean = param.isTerm && (param.flags.is(Flags.Given) || param.flags.is(Flags.Implicit))

    // The shape of every term parameter, in order, `using` ones and an extension's receiver
    // among them: what search matches a `-Type` criterion against.
    private def accepts(symbol: Symbol): List[Api.Shape] =
      List.from(symbol.paramSymss.flatten.filter(_.isTerm).map { p => shape(p.info.widenByName) })

    // What an extension extends — the one parameter of its first plain term clause — and what
    // it, or a given, requires: the type of each `using` parameter, a context bound being one.
    private def matching(symbol: Symbol, isExtension: Boolean): (Optional[Api.Shape], List[Api.Shape]) =
      val all: scala.List[scala.List[Symbol]] = symbol.paramSymss

      val receiver: Optional[Api.Shape] =
        if !isExtension then Unset
        else all.find(_.headOption.exists { p => p.isTerm && !isContextual(p) }).flatMap(_.headOption) match
          case Some(param) => shape(param.info.widenByName)
          case None        => Unset

      val declared: List[Api.Shape] = List.from(all.flatMap(_.filter(isContextual).map { p => shape(p.info.widenByName) }))

      // An extension declared in a class or trait (`trait Functor[F[_]]: extension …`) is
      // reached through an instance of it: it requires one, of the class applied to its type
      // parameters.
      val owner: Symbol = symbol.owner
      val enclosing: List[Api.Shape] =
        if !isExtension || !owner.isClassDef || owner.flags.is(Flags.Module) || owner.flags.is(Flags.Package) then Nil
        else designator(owner).lay(Nil: List[Api.Shape]): (d: Designator) =>
          val params: scala.List[Api.Shape] =
            owner.primaryConstructor.paramSymss.flatten.filter(_.isType).map { p => Api.Shape.Param(p.name.tt) }

          List(Api.Shape.Named(Api.key(d), List.from(params)))

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
      val all: scala.List[scala.List[Symbol]] = symbol.paramSymss
      val index: Int = all.indexWhere { params => params.headOption.exists { p => p.isTerm && !p.flags.is(Flags.Given) && !p.flags.is(Flags.Implicit) } }
      val (group, rest) = if index < 0 then (all, scala.Nil) else all.splitAt(index + 1)
      def make(clauses: scala.List[scala.List[Symbol]]): List[Syntax] = List.from(clauses.flatMap { params => clause(params).let(scala.List(_)).or(scala.Nil) })
      (make(group), make(rest))

    private def record(parent: Optional[Designator], name: Text, own: Boolean, declaration: Optional[Api.Declaration], isType: Boolean, where: Optional[Api.Location], bases: List[Text] = Nil): Unit =
      val key: Text = Api.key(parent, name)

      collected.compute(key, (_, existing) => Optional(existing) match
        case Unset =>
          if isType then Api.Entity(parent, name, own, Unset, declaration, location = where, bases = bases)
          else Api.Entity(parent, name, own, declaration, Unset, location = where)

        case entity: Api.Entity =>
          declaration.lay(entity): (declaration: Api.Declaration) =>
            val slot: Optional[Api.Declaration] = if isType then entity.template else entity.definition
            val parameter: Boolean = declaration.kind.isParameter
            val other: Optional[Api.Declaration] = entity.declarations.seek(_.kind.isParameter != parameter)
            other.let { (other: Api.Declaration) => Inspection.clashes0.add((key, other.kind, declaration.kind)) }

            // A parameter is one entity however many overloads declare it, and a member of its
            // name — a case class's field, which is its constructor's parameter — keeps the
            // entity, whichever is met first.
            if parameter && (slot.present || other.present) then entity
            else if !parameter && slot.let(_.kind.isParameter).or(false) then
              if isType then entity.copy(own = entity.own || own, template = declaration, location = where.or(entity.location))
              else entity.copy(own = entity.own || own, definition = declaration, location = where.or(entity.location))
            // A package is met once per member loaded beneath it; it is declared once.
            else if declaration.kind == Api.Kind.Package && entity.definition.present then entity.copy(own = entity.own || own)
            // A given with a body (`given foo: Show[Doc] with …`) is a value and an object of
            // one name: the given is the declaration, the object no overload of it.
            else if declaration.kind == Api.Kind.Given && entity.definition.let(_.kind == Api.Kind.Object).or(false) then
              entity.copy(own = entity.own || own, definition = declaration, location = entity.location.or(where))
            else if declaration.kind == Api.Kind.Object && entity.definition.let(_.kind == Api.Kind.Given).or(false) then
              entity.copy(own = entity.own || own)
            else if isType then entity.copy(own = entity.own || own, template = entity.template.or(declaration), location = entity.location.or(where), bases = if entity.bases.nil then bases else entity.bases)
            else if entity.definition.absent then entity.copy(own = entity.own || own, definition = declaration, location = entity.location.or(where))
            else entity.copy(own = entity.own || own, overloads = entity.overloads + List(declaration)))

    // The parameters of a method, a constructor or a type alias, each a member of the type form
    // of what it parameterises (`parent`), from every clause: type parameters with their bounds,
    // term parameters with their types. A name the compiler made up (`x$1`, an evidence) is not
    // the API.
    private def parameters(parent: Designator, clauses: scala.List[scala.List[Symbol]], own: Boolean): Unit =
      if own || Inspection.everyParameter then clauses.foreach: params =>
        params.foreach: param =>
          if !skipped(param) && !param.name.contains("$") then
            val name: Text = param.name.tt
            val where: Optional[Api.Location] = if own then location(param) else Unset

            if param.isType then
              val bounds: Optional[Syntax] = typeResult(param.info, param.flags)
              record(parent, name, own, Api.Declaration(Api.Kind.TypeParam, Nil, Nil, bounds), true, where)
            else
              val contextual: List[Text] = if param.flags.is(Flags.Given) then List(t"using") else Nil
              val declaration: Api.Declaration = Api.Declaration(Api.Kind.Param, contextual + modifiers(param), Nil, syntax(param.info.widenByName))
              record(parent, name, own, declaration, false, where)

    private def typeForm(parent: Optional[Designator], name: Text): Designator =
      parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Type(parent, name) }

    private def overrides(symbol: Symbol): Boolean =
      try symbol.allOverriddenSymbols.nonEmpty catch case _: Throwable => false

    private def member(parent: Optional[Designator], symbol: Symbol, own: Boolean): Unit =
      if !skipped(symbol) then
        val name: Text = plain(symbol).tt
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

          val (params, rhs): (List[Syntax], Optional[Syntax]) = aliased match
            case TypeLambda(names, bounds, body) =>
              val typeParams: scala.List[Syntax] = names.zip(bounds).map: (name, bound) =>
                val symbolic: Syntax = Syntax.Symbolic(name.tt)
                bound match
                  case TypeBounds(lower, upper) =>
                    val bounded: Syntax = if isNothing(lower) then symbolic else Syntax.Infix(symbolic, t">:", syntax(lower))
                    if isAny(upper) then bounded else Syntax.Infix(bounded, t"<:", syntax(upper))
                  case _ => symbolic

              (List(Syntax.Sequence('[', List.from(typeParams))), typeResult(body, flags))

            case other =>
              (Nil, typeResult(other, flags))

          // What a non-opaque alias stands for, over its parameters, for matching to expand.
          val noAlias: Optional[Api.Shape] = Unset
          val noParams: List[Text] = Nil

          val (aliasOf, aliasParams): (Optional[Api.Shape], List[Text]) =
            if flags.is(Flags.Opaque) then (noAlias, noParams) else aliased match
              case TypeLambda(names, _, body) =>
                val target: Optional[Api.Shape] = shape(body)
                val params: List[Text] = List.from(names.map(_.tt))
                (target, params)

              case TypeBounds(_, _) => (noAlias, noParams)

              case other =>
                val target: Optional[Api.Shape] = shape(other)
                (target, noParams)

          record(parent, name, own, Api.Declaration(Api.Kind.Type, modifiers(symbol), params, rhs, aliasOf = aliasOf, aliasParams = aliasParams), true, where)

          aliased match
            case TypeLambda(names, bounds, _) if own || Inspection.everyParameter =>
              names.zip(bounds).foreach: (param, bound) =>
                val declaration: Api.Declaration = Api.Declaration(Api.Kind.TypeParam, Nil, Nil, typeResult(bound, Flags.EmptyFlags))
                record(typeForm(parent, name), param.tt, own, declaration, true, Unset)
            case _ =>
              ()

          // An exported type is a page elsewhere: a reference through the export leads there.
          if flags.is(Flags.Exported) then rhs match
            case Syntax.Simple(target) => collected.computeIfPresent(Api.key(parent, name), (_, e) => e.nn.copy(alias = target))
            case _                     => ()
        else if symbol.isDefDef then
          val kind: Api.Kind =
            if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.ExtensionMethod) then Api.Kind.Extension
            else Api.Kind.Def

          val none: List[Syntax] = Nil

          val (group, own0): (List[Syntax], List[Syntax]) =
            if kind == Api.Kind.Extension then extension(symbol) else (none, clauses(symbol))

          val noShape: Optional[Api.Shape] = Unset
          val noShapes: List[Api.Shape] = Nil

          val (receiver, requires): (Optional[Api.Shape], List[Api.Shape]) =
            if kind == Api.Kind.Def then (noShape, noShapes) else matching(symbol, kind == Api.Kind.Extension)

          val returns: Api.Shape = shape(result(symbol.info))
          val provides: Optional[Api.Shape] = if kind == Api.Kind.Given then returns else Unset
          val declaration: Api.Declaration = Api.Declaration(kind, modifiers(symbol), own0, syntax(result(symbol.info)), Nil, group, inherited, receiver, requires, provides, exported = flags.is(Flags.Exported), returns = returns, accepts = accepts(symbol))
          record(parent, name, own, declaration, false, where)
          parameters(typeForm(parent, name), symbol.paramSymss, own)
        else if symbol.isValDef then
          val kind: Api.Kind =
            if flags.is(Flags.Enum) && flags.is(Flags.Case) then Api.Kind.Case
            else if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.Mutable) then Api.Kind.Var
            else Api.Kind.Val

          // An enum's value case has the enum as its type, which its page already says.
          val tpe: Optional[Syntax] = if kind == Api.Kind.Case then Unset else syntax(result(symbol.info))

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

          val mods: List[Text] = if kind == Api.Kind.Given then modifiers(symbol).filter(_ != t"lazy") else modifiers(symbol)
          record(parent, name, own, Api.Declaration(kind, mods, Nil, shownType, Nil, Nil, inherited, provides = provides, exported = flags.is(Flags.Exported), returns = shape(result(symbol.info))), false, where)

    // An alias's right-hand side, a bound type's bounds, or nothing for an opaque type's
    // underlying (which is not the API).
    private def typeResult(repr: TypeRepr, flags: Flags): Optional[Syntax] = repr match
      case TypeBounds(lower, upper) if lower =:= upper => if flags.is(Flags.Opaque) then Unset else syntax(lower)
      case TypeBounds(lower, upper) =>
        val lo: Optional[Syntax] = if isNothing(lower) then Unset else Syntax.Prefix(t">:", syntax(lower))
        val hi: Optional[Syntax] = if isAny(upper) then Unset else Syntax.Prefix(t"<:", syntax(upper))
        val both: List[Syntax] = lo.let(List(_)).or(Nil) + hi.let(List(_)).or(Nil)
        if both.nil then Unset else Syntax.Compound(both)
      case other => syntax(other)

    private def template(symbol: Symbol, own: Boolean): Unit =
      val here: Optional[Designator] = designator(symbol)
      val where: Optional[Api.Location] = if own then location(symbol) else Unset

      if !carrier(symbol) then
        val name: Text = plain(symbol).tt
        val parent: Optional[Designator] = designator(symbol.owner)

        if symbol.flags.is(Flags.Module) then
          // A given object (`given foo: Show[Doc] with …`) is a given, which provides its
          // first parent.
          if symbol.flags.is(Flags.Given) then
            record(parent, name, own, Api.Declaration(Api.Kind.Given, modifiers(symbol), Nil, objectType(symbol), parents(symbol), provides = objectProvides(symbol)), false, where)
          else record(parent, name, own, Api.Declaration(Api.Kind.Object, modifiers(symbol), Nil, Unset, parents(symbol)), false, where)
        else
          val kind: Api.Kind = templateKind(symbol)
          val constructor: List[Syntax] = if kind == Api.Kind.Trait && symbol.primaryConstructor.paramSymss.forall(_.isEmpty) then Nil else clauses(symbol.primaryConstructor)
          val parentShapes: List[Api.Shape] = List.from(parentTypes(symbol).map(shape))
          record(parent, name, own, Api.Declaration(kind, modifiers(symbol), constructor, Unset, parents(symbol), accepts = accepts(symbol.primaryConstructor), parentShapes = parentShapes), true, where, bases(symbol))
          here.let { (here: Designator) => parameters(here, symbol.primaryConstructor.paramSymss, own) }

      // A member which cannot be read is left out, and the rest are still walked.
      symbol.declarations.foreach: child =>
        try member(here, child, own)
        catch case error: Throwable => Inspection.skipped(error)

    private def packageEntity(symbol: Symbol): Unit =
      if symbol.isPackageDef || carrier(symbol) then
        designator(symbol).let: (designator: Designator) =>
          record(designator.parent, designator.name, false, Api.Declaration(Api.Kind.Package), false, Unset)
          designator.parent.let { (parent: Designator) => packageOf(parent) }

    private def packageOf(designator: Designator): Unit =
      record(designator.parent, designator.name, false, Api.Declaration(Api.Kind.Package), false, Unset)
      designator.parent.let(packageOf)

    // A top-level class or module of a dependency, by the name its `.tasty` entry gives,
    // loaded from the classpath; its packages are recorded with it.
    def load(name: Text): Unit =
      // A package object's carrier (`pkg_core$package`) is a module, named without its `$`.
      val isModule: Boolean = name.ends(t"$$") || name.ends(t"$$package")
      val plainName: Text = if name.ends(t"$$") then name.keep(name.length - 1) else name

      // A name looked up in the session's symbol table without a stub being made for one it
      // does not have (as `requiredClass` and `requiredModule` would, which the session would
      // then meet as a bad symbolic reference when it compiles a fragment).
      def static(name: dotty.tools.dotc.core.Names.Name): Optional[Symbol] =
        try
          given dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
          val denotation = dotty.tools.dotc.core.Denotations.staticRef(name, generateStubs = false)
          if denotation.exists && denotation.symbol.exists then denotation.symbol.asInstanceOf[Symbol] else Unset
        catch case _: Throwable => Unset

      // One `.tasty` holds a class and its companion object, or an object alone, which the
      // symbol table keeps apart: both are walked.
      val module: Optional[Symbol] =
        static(dotty.tools.dotc.core.Names.termName(plainName.s)).let { (m: Symbol) => if m.isNoSymbol then Unset else m.moduleClass }

      val cls: Optional[Symbol] = if isModule then Unset else static(dotty.tools.dotc.core.Names.typeName(plainName.s))

      (cls.let(List(_)).or(Nil) + module.let(List(_)).or(Nil)).each: (symbol: Symbol) =>
        try
          packageEntity(symbol.owner)
          template(symbol, false)
        catch case _: Throwable => ()

    def walk(tree: Tree, own: Boolean): Unit = tree match
      case PackageClause(pid, statements) =>
        packageEntity(pid.symbol)
        statements.foreach(walk(_, own))

      case classDef: ClassDef =>
        template(classDef.symbol, own)

      case _ =>
        ()

