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

import scala.collection.immutable as sci

import soundness.{Language as _, Token as _, *}

import stenography.{Designator, Syntax}

// The API of a project and its dependencies, as read from compiled code: every package, type
// and term, by its fully-qualified name, with the signatures of its declarations and, once an
// overlay has been merged, its documentation. As Amok's model: a type and its companion object
// are one *entity* (one page), keyed by their parent and name; which of the two a member
// belongs to is what the parent's kind says, and what the URL's separator encodes — `.` for a
// member of a term, `~` for a member of a type (`#` being a URL's fragment).
object Api:
  // The kinds of declaration, each with the keyword it is declared by. A parameter is an
  // entity too, a member of the type form of what it parameterises (`quux.foo#bar`,
  // `quux.Box#T`), so that an overlay documents it as it does any member.
  enum Kind:
    case Package, Object, Class, CaseClass, Trait, Enum, Case, Type, Def, Val, Var, Given, Extension, Param, TypeParam

    def keyword: Text = this match
      case Package   => t"package"
      case Object    => t"object"
      case Class     => t"class"
      case CaseClass => t"case class"
      case Trait     => t"trait"
      case Enum      => t"enum"
      case Case      => t"case"
      case Type      => t"type"
      case Def       => t"def"
      case Val       => t"val"
      case Var       => t"var"
      case Given     => t"given"
      case Extension => t"extension"
      case Param     => t"param"
      case TypeParam => t"type"

    // Whether a declaration of this kind is a type (a template, as Amok said) or a term.
    def isType: Boolean = this match
      case Class | CaseClass | Trait | Enum | Type | TypeParam => true
      case _                                                   => false

    def isParameter: Boolean = this == Param || this == TypeParam

  // The structure of a type as far as matching needs it — an extension's receiver against the
  // type it may extend, a given's type against the type it is for — apart from how it is
  // shown: a named type, by the key of its entity (an alias seen through, an opaque type not),
  // with its arguments; a type parameter, with its upper bound where it has one; or anything
  // else, which matches nothing in particular.
  enum Shape:
    case Named(key: Text, args: List[Shape] = Nil)
    case Param(name: Text, upper: Optional[Shape] = Unset)
    case Other

    def head: Optional[Text] = this match
      case Named(key, _) => key
      case _             => Unset

  // One declaration: its kind, its modifiers (as written, `final`, `inline`, …), its clauses
  // (type and term parameters), its result (a return type, an alias's right-hand side, a type's
  // bounds), a template's parents, and for an extension the clauses it extends; then what
  // matching reads: an extension's `receiver` (the shape of the one parameter it extends), the
  // `requires` of an extension or a given (the shape of each of its `using` parameter, a
  // context bound among them, and for an extension declared in a class or trait, an instance of
  // it), a given's `provides` (the shape of its type), and a type alias's `aliasOf` (the shape
  // it stands for, over its `aliasParams`; an opaque type stands for nothing).
  case class Declaration
    ( kind:        Kind,
      modifiers:   List[Text]       = Nil,
      clauses:     List[Syntax]     = Nil,
      result:      Optional[Syntax] = Unset,
      parents:     List[Syntax]     = Nil,
      group:       List[Syntax]     = Nil,
      overrides:   Boolean          = false,   // implements or overrides an inherited member, which documents it
      receiver:    Optional[Shape]  = Unset,
      requires:    List[Shape]      = Nil,
      provides:    Optional[Shape]  = Unset,
      aliasOf:     Optional[Shape]  = Unset,
      aliasParams: List[Text]       = Nil,
      exported:    Boolean          = false )   // an export forwarder, which what it forwards to documents

  // An extension method applicable to an entity, as `Applicables` finds it: the `key` of the
  // extension and the `index` of the declaration among the entity's (an overloaded name's
  // variants extend different things), how it reaches the entity — its `tier`, lower the closer
  // (see `Applicables`), the `distance` within the tier, and its `reach`: 0 where no import is
  // needed (a companion's), 1 for the entity's own package, 2 for anywhere else — the given
  // which makes it available (`via`), where one does, and the key of the object or package whose
  // members bring it (`importFrom`).
  case class Applicable
    ( key:        Text,
      index:      Int,
      tier:       Int,
      distance:   Int,
      reach:      Int,
      via:        Optional[Text] = Unset,
      importFrom: Optional[Text] = Unset )

  // Where a declaration was written: its source file and line (from one), for the project's own
  // code, whose source the inspection has.
  case class Location(path: Text, line: Int)

  // What an overlay says of an entity: its one-line `info` and its Markdown `detail`, whether it
  // is `hidden`, the version it was removed `until` and what replaces it, the entities it
  // `refer`s to, and the `import`s its prose resolves references through.
  case class Entry
    ( info:    Optional[Text]   = Unset,
      detail:  Optional[Text]   = Unset,
      hidden:  Boolean          = false,
      until:   Optional[Until]  = Unset,
      refer:   List[Text]       = Nil,
      imports: List[Text]       = Nil,
      file:    Optional[Text]   = Unset,
      line:    Optional[Int]    = Unset )

  case class Until(version: Text, replacement: Optional[Text] = Unset)

  // A type and its companion, or a term, or a package: one page. `definition` is the term's
  // declaration and `template` the type's; `overloads` are the further term declarations of the
  // name; `members` are the keys of its children; `own` says it comes from the project's
  // sources rather than a dependency; `removed` that an overlay documents it although nothing
  // compiled declares it. A type's `bases` are the keys of every class it extends, nearest
  // first; its `extensions` and `givens` are what `Applicables` finds applies to it.
  case class Entity
    ( parent:     Optional[Designator],
      name:       Text,
      own:        Boolean               = false,
      definition: Optional[Declaration] = Unset,
      template:   Optional[Declaration] = Unset,
      overloads:  List[Declaration]     = Nil,
      members:    List[Text]            = Nil,
      location:   Optional[Location]    = Unset,
      doc:        Optional[Entry]       = Unset,
      removed:    Boolean               = false,
      alias:      Optional[Designator]  = Unset,   // an export forwarder's target, whose page is this one's
      bases:      List[Text]            = Nil,
      extensions: List[Applicable]      = Nil,
      givens:     List[Text]            = Nil ):

    def key: Text = Api.key(parent, name)
    def path: Text = Api.path(parent, name)
    def url: Text = t"/api/$path"

    // The designator of this entity as a term, and as a type: a member's parent is one or the
    // other.
    def term: Designator = parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Term(parent, name) }
    def tpe: Designator = parent.lay(Designator.Top(name)) { (parent: Designator) => Designator.Type(parent, name) }

    def isPackage: Boolean = definition.let(_.kind == Kind.Package).or(false)
    def declarations: List[Declaration] = definition.let(List(_)).or(Nil) + template.let(List(_)).or(Nil) + overloads
    def hidden: Boolean = doc.let(_.hidden).or(false)

    // A parameter, which its parent's page lists apart from its members.
    def isParameter: Boolean = !declarations.nil && declarations.all(_.kind.isParameter)

  // The key of an entity: its parent's qualified name, the separator its parent's kind gives
  // (`#` after a type, `.` after a term or a package), and its name.
  def key(parent: Optional[Designator], name: Text): Text =
    parent.lay(name) { (parent: Designator) => t"${parent.qualified}${separator(parent)}$name" }

  def key(designator: Designator): Text = key(designator.parent, designator.name)

  private def separator(parent: Designator): Text = parent match
    case Designator.Type(_, _) => t"#"
    case _                     => t"."

  // The path of an entity's page, beneath `/api/`: as its key, with `~` for `#` and every name
  // URL-encoded, so that a symbolic name travels.
  def path(parent: Optional[Designator], name: Text): Text =
    parent.lay(encode(name)) { (parent: Designator) => t"${path(parent.parent, parent.name)}${if separator(parent) == t"#" then t"~" else t"."}${encode(name)}" }

  private def encode(name: Text): Text = Text(java.net.URLEncoder.encode(name.s, "UTF-8").nn)
  private def decode(name: Text): Text = Text(java.net.URLDecoder.decode(name.s, "UTF-8").nn)

  // The key a path names, back from its separators; the designator's kinds are the separators'.
  def unpath(path: Text): Designator =
    def recur(index: Int, start: Int, designator: Optional[Designator]): Designator =
      def next: Text = decode(path.s.substring(start, index).nn.tt)
      def extend(isType: Boolean): Designator =
        designator.lay(Designator.Top(next)) { (parent: Designator) => parent.child(next, isType) }

      if index >= path.s.length then extend(false)
      else path.s.charAt(index) match
        case '.'  => recur(index + 1, index + 1, extend(false))
        case '~'  => recur(index + 1, index + 1, extend(true))
        case _    => recur(index + 1, start, designator)

    recur(0, 0, Unset)

  def empty: Api = Api(sci.Map())

// The entities, by key (a standard map, which the inspection builds and the overlay amends);
// `roots` are the packages with no parent.
case class Api(entities: sci.Map[Text, Api.Entity]):
  def apply(key: Text): Optional[Api.Entity] = entities.get(key) match
    case Some(entity) => entity
    case _            => Unset

  def has(designator: Designator): Boolean = entities.contains(Api.key(designator))
  def locate(path: Text): Optional[Api.Entity] = apply(Api.key(Api.unpath(path)))
  def children(entity: Api.Entity): List[Api.Entity] = entity.members.bind { (key: Text) => apply(key).let(List(_)).or(Nil) }

  def roots: List[Api.Entity] =
    List.from(entities.values.filter(_.parent.absent).toList.sortBy(_.name.s))

  // The entity a designator names, if it exists as a term or a type member of its parent: a
  // designator written with `.` may name a type (`gossamer.Text`), so both are tried.
  def find(designator: Designator): Optional[Api.Entity] =
    apply(Api.key(designator)).or:
      designator.parent.let { (parent: Designator) => apply(Api.key(parent.companionType, designator.name)) }

  // Every entity of a simple name, for a reference to be resolved among and for search.
  lazy val byName: sci.Map[Text, List[Api.Entity]] =
    entities.values.toList.groupBy(_.name).map: (name, group) =>
      val sorted: List[Api.Entity] = List.from(group.sortBy(_.key.s))
      (name, sorted)

  def add(entity: Api.Entity): Api = Api(entities.updated(entity.key, entity))

  // Two models as one: an entity in both keeps the project's own declaration, and its members
  // are those of either; the members are relinked afterwards.
  def merge(other: Api): Api =
    val combined: sci.Map[Text, Api.Entity] = other.entities.foldLeft(entities): (acc, pair) =>
      val (key, entity) = pair
      acc.get(key) match
        case Some(existing) =>
          val merged: Api.Entity =
            if entity.own && !existing.own then entity.copy(doc = existing.doc.or(entity.doc))
            else existing.copy(definition = existing.definition.or(entity.definition), template = existing.template.or(entity.template), overloads = existing.overloads + entity.overloads)
          acc.updated(key, merged)
        case None => acc.updated(key, entity)

    Api(combined).relinked

  // Every entity's `members`, from the parents its children name, sorted by name.
  def relinked: Api =
    val children: sci.Map[Text, scala.List[Api.Entity]] =
      entities.values.toList.flatMap { e => e.parent.let { (p: Designator) => scala.List((Api.key(p), e)) }.or(scala.Nil) }.groupMap(_(0))(_(1))

    Api(entities.map { (key, entity) =>
      val known: scala.List[Api.Entity] = children.getOrElse(key, scala.Nil)
      (key, entity.copy(members = List.from(known.sortBy(_.name.s).map(_.key))))
    })

  def update(key: Text)(change: Api.Entity => Api.Entity): Api =
    apply(key).lay(this) { (entity: Api.Entity) => add(change(entity)) }
