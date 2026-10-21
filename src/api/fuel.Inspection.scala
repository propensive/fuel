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

          linked(Api(entities))
      catch case error: Throwable => error

  // Every `.tasty` beneath a directory, as the inspector takes them.
  def tastyIn(directory: Text): List[Text] =
    val root = java.nio.file.Path.of(directory.s).nn
    if !java.nio.file.Files.isDirectory(root) then Nil else
      val stream = java.nio.file.Files.walk(root).nn
      try List.from(stream.iterator.nn.asScala.map(_.toString.tt).filter(_.ends(t".tasty")).toList)
      finally stream.close()

  // Every entity's `members`, from the parents its children name, sorted type members before
  // term members and by name within each.
  private def linked(api: Api): Api =
    val children: juc.ConcurrentHashMap[Text, scala.List[Api.Entity]] = juc.ConcurrentHashMap()

    api.entities.values.foreach: entity =>
      entity.parent.let: (parent: Designator) =>
        children.merge(Api.key(parent), scala.List(entity), (a, b) => a.nn ++ b.nn)

    Api(api.entities.map { (key, entity) =>
      val known: scala.List[Api.Entity] = Optional(children.get(key)).or(scala.Nil)
      val members: List[Text] = List.from(known.sortBy(_.name.s).map(_.key))
      (key, entity.copy(members = members))
    })

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

    private def syntax(repr: TypeRepr): Syntax = try Syntax(repr) catch case _: Throwable => Syntax.Primitive(t"?")

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

    private def parents(symbol: Symbol): List[Syntax] =
      // The parents every class or case class has without saying so.
      val ignored: sci.Set[String] =
        sci.Set("Any", "AnyRef", "Object", "Matchable", "Product", "Serializable", "AnyVal")

      def typeOf(tree: Tree): Option[TypeRepr] = tree match
        case tree: TypeTree => Some(tree.tpe)
        case term: Term     => Some(term.tpe)
        case _              => None

      try
        symbol.tree match
          case ClassDef(_, _, parentTrees, _, _) =>
            List.from(parentTrees.flatMap(typeOf).filter { tpe => !ignored.contains(tpe.typeSymbol.name) }.map { tpe => syntax(tpe) })

          case _ => Nil
      catch case _: Throwable => Nil

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

    private def record(parent: Optional[Designator], name: Text, own: Boolean, declaration: Optional[Api.Declaration], isType: Boolean, where: Optional[Api.Location]): Unit =
      val key: Text = Api.key(parent, name)

      collected.compute(key, (_, existing) => Optional(existing) match
        case Unset =>
          if isType then Api.Entity(parent, name, own, Unset, declaration, location = where)
          else Api.Entity(parent, name, own, declaration, Unset, location = where)

        case entity: Api.Entity =>
          declaration.lay(entity): (declaration: Api.Declaration) =>
            if isType then entity.copy(own = entity.own || own, template = entity.template.or(declaration), location = entity.location.or(where))
            else if entity.definition.absent then entity.copy(own = entity.own || own, definition = declaration, location = entity.location.or(where))
            else entity.copy(own = entity.own || own, overloads = entity.overloads + List(declaration)))

    private def member(parent: Optional[Designator], symbol: Symbol, own: Boolean): Unit =
      if !skipped(symbol) then
        val name: Text = plain(symbol).tt
        val where: Optional[Api.Location] = if own then location(symbol) else Unset
        val flags = symbol.flags

        if symbol.isClassDef then template(symbol, own)
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

          record(parent, name, own, Api.Declaration(Api.Kind.Type, modifiers(symbol), params, rhs), true, where)
        else if symbol.isDefDef then
          val kind: Api.Kind =
            if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.ExtensionMethod) then Api.Kind.Extension
            else Api.Kind.Def

          val none: List[Syntax] = Nil

          val (group, own0): (List[Syntax], List[Syntax]) =
            if kind == Api.Kind.Extension then extension(symbol) else (none, clauses(symbol))

          record(parent, name, own, Api.Declaration(kind, modifiers(symbol), own0, syntax(result(symbol.info)), Nil, group), false, where)
        else if symbol.isValDef then
          val kind: Api.Kind =
            if flags.is(Flags.Enum) && flags.is(Flags.Case) then Api.Kind.Case
            else if flags.is(Flags.Given) then Api.Kind.Given
            else if flags.is(Flags.Mutable) then Api.Kind.Var
            else Api.Kind.Val

          // An enum's value case has the enum as its type, which its page already says.
          val tpe: Optional[Syntax] = if kind == Api.Kind.Case then Unset else syntax(result(symbol.info))
          record(parent, name, own, Api.Declaration(kind, modifiers(symbol), Nil, tpe), false, where)

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
          record(parent, name, own, Api.Declaration(Api.Kind.Object, modifiers(symbol), Nil, Unset, parents(symbol)), false, where)
        else
          val kind: Api.Kind = templateKind(symbol)
          val constructor: List[Syntax] = if kind == Api.Kind.Trait && symbol.primaryConstructor.paramSymss.forall(_.isEmpty) then Nil else clauses(symbol.primaryConstructor)
          record(parent, name, own, Api.Declaration(kind, modifiers(symbol), constructor, Unset, parents(symbol)), true, where)

      symbol.declarations.foreach { child => member(here, child, own) }

    private def packageEntity(symbol: Symbol): Unit =
      designator(symbol).let: (designator: Designator) =>
        record(designator.parent, designator.name, false, Api.Declaration(Api.Kind.Package), false, Unset)
        designator.parent.let { (parent: Designator) => packageOf(parent) }

    private def packageOf(designator: Designator): Unit =
      record(designator.parent, designator.name, false, Api.Declaration(Api.Kind.Package), false, Unset)
      designator.parent.let(packageOf)

    def walk(tree: Tree, own: Boolean): Unit = tree match
      case PackageClause(pid, statements) =>
        packageEntity(pid.symbol)
        statements.foreach(walk(_, own))

      case classDef: ClassDef =>
        template(classDef.symbol, own)

      case _ =>
        ()

