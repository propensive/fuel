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

import soundness.{Format as _, *}

import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.sortingAlgorithms.timsort

import stenography.{Designator, Syntax}

// The API of a project and its dependencies, as read from compiled code: every package, type
// and term, by its fully-qualified name, with the signatures of its declarations and, once an
// overlay has been merged, its documentation. As Amok's model: a type and its companion object
// are one *entity* (one page), keyed by their parent and name; which of the two a member
// belongs to is what the parent's kind says, and what the URL's separator encodes — `.` for a
// member of a term, `~` for a member of a type (`#` being a URL's fragment).
object Api:
  // The kinds of declaration, each with the keyword it is declared by, and whether a declaration
  // of it is a type (a template, as Amok said) or a term. A format has kinds of its own; Scala's
  // are `Kind.Scala`'s. A parameter is an entity too, a member of the type form of what it
  // parameterises (`quux.foo#bar`, `quux.Box#T`), so that an overlay documents it as it does
  // any member.
  trait Kind:
    def keyword: Text
    def isType: Boolean
    def isParameter: Boolean = false

  object Kind:
    sealed abstract class Scala(val keyword: Text, val isType: Boolean = false) extends Kind:
      override def isParameter: Boolean = this == Param || this == TypeParam

    case object Package   extends Scala(t"package")
    case object Object    extends Scala(t"object")
    case object Class     extends Scala(t"class", true)
    case object CaseClass extends Scala(t"case class", true)
    case object Trait     extends Scala(t"trait", true)
    case object Enum      extends Scala(t"enum", true)
    case object Case      extends Scala(t"case")
    case object Type      extends Scala(t"type", true)
    case object Def       extends Scala(t"def")
    case object Val       extends Scala(t"val")
    case object Var       extends Scala(t"var")
    case object Given     extends Scala(t"given")
    case object Extension extends Scala(t"extension")
    case object Param     extends Scala(t"param")
    case object TypeParam extends Scala(t"type", true)

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

  // One declaration: its kind, its modifiers (as written, `final`, `inline`, …, with `java` and
  // `annotation`, which are not written but are said of it, as its glyph says them), its clauses
  // (type and term parameters), its result (a return type, an alias's right-hand side, a type's
  // bounds), a template's parents, and for an extension the clauses it extends; then what
  // matching reads: an extension's `receiver` (the shape of the one parameter it extends), the
  // `requires` of an extension or a given (the shape of each of its `using` parameter, a
  // context bound among them, and for an extension declared in a class or trait, an instance of
  // it), a given's `provides` (the shape of its type), and a type alias's `aliasOf` (the shape
  // it stands for, over its `aliasParams`; an opaque type stands for nothing). For search, the
  // shape of what a declaration `returns` (a term's type, a class not being a result), what it
  // `accepts` (every term parameter, `using` ones included, and for an extension the one it
  // extends) and, for a template, its `parentShapes` (its direct parents, as written, so that
  // `Doc extends Shape[Int]` is known to be a `Shape[Int]`). A template's `derives` are the type
  // classes its `derives` clause names, and its `uses` the capabilities its `uses` clause does,
  // each one used in its initialization written with `initially`.
  case class Declaration
    ( kind:        Kind,
      modifiers:   List[Text]       = Nil,
      clauses:     List[Syntax]     = Nil,
      result:      Optional[Syntax] = Unset,
      parents:     List[Syntax]     = Nil,
      group:       List[Syntax]     = Nil,
      // Whether it implements or overrides an inherited member, which documents it.
      overrides:   Boolean          = false,
      receiver:    Optional[Shape]  = Unset,
      requires:    List[Shape]      = Nil,
      provides:    Optional[Shape]  = Unset,
      aliasOf:     Optional[Shape]  = Unset,
      aliasParams: List[Text]       = Nil,
      // Whether it is an export forwarder, which what it forwards to documents.
      exported:    Boolean          = false,
      returns:     Optional[Shape]  = Unset,
      accepts:     List[Shape]      = Nil,
      parentShapes: List[Shape]     = Nil,
      derives:     List[Syntax]     = Nil,
      uses:        List[Syntax]     = Nil,
      // What follows the keyword and the name, for a format whose signatures are words.
      signature:   Optional[Signature] = Unset ):

    def written: List[Text] = modifiers.filter { m => m != t"java" && m != t"annotation" }

  // A word of a signature, for a format which writes one as words rather than as Scala's
  // syntax: its text, what it is (which is how it is coloured), and the key of the entity, in
  // the same API, which it names, where it names one.
  case class Phrase
    ( text: Text, role: Phrase.Role = Phrase.Role.Plain, target: Optional[Text] = Unset )

  object Phrase:
    enum Role:
      case Keyword, Modifier, Type, Term, Literal, Symbol, Plain

  // A declaration's signature as words: the `tail` which follows its keyword and its name on
  // their line (`String optional`, after `field title`), and the lines of its `body` beneath.
  case class Signature(tail: List[Phrase] = Nil, body: List[List[Phrase]] = Nil)

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
      line:    Optional[Int]    = Unset,
      keywords: List[Text]      = Nil )

  case class Until(version: Text, replacement: Optional[Text] = Unset)

  // A type and its companion, or a term, or a package: one page. `definition` is the term's
  // declaration and `template` the type's; `overloads` are the further term declarations of the
  // name; `members` are the keys of its children; `own` says it comes from the project's
  // sources rather than a dependency; `removed` that an overlay documents it although nothing
  // compiled declares it. A type's `bases` are the keys of every class it extends, nearest
  // first; its `extensions` and `givens` are what `Applicables` finds applies to it.
  case class Entity
    ( parent:     Optional[Locator],
      name:       Text,
      own:        Boolean               = false,
      definition: Optional[Declaration] = Unset,
      template:   Optional[Declaration] = Unset,
      overloads:  List[Declaration]     = Nil,
      members:    List[Text]            = Nil,
      location:   Optional[Location]    = Unset,
      doc:        Optional[Entry]       = Unset,
      removed:    Boolean               = false,
      // An export forwarder's target, whose page is this one's.
      alias:      Optional[Designator]  = Unset,
      bases:      List[Text]            = Nil,
      extensions: List[Applicable]      = Nil,
      givens:     List[Text]            = Nil,
      // The `id` of the format it is declared in.
      format:     Text                  = t"scala" ):

    def key: Text = Api.key(parent, name)
    def path: Text = Api.path(parent, name)

    // The entity among those of every format: its key, after its format's `id` and a `/` unless
    // it is Scala's — as its page's URL is its path after them.
    def address: Text = t"${Api.prefix(format)}$key"
    def url: Text = t"/api/${Api.prefix(format)}$path"

    // The locator of this entity as a term, and as a type: a member's parent is one or the
    // other.
    def term: Locator = Locator(parent, name, false)
    def tpe: Locator = Locator(parent, name, true)

    def isPackage: Boolean = definition.let(_.kind == Kind.Package).or(false)

    def declarations: List[Declaration] =
      definition.let(List(_)).or(Nil) + template.let(List(_)).or(Nil) + overloads

    def hidden: Boolean = doc.let(_.hidden).or(false)

    // The instance a `derives` clause generated, which the clause itself says: an instance of its
    // type class, but neither a member to list nor one to document.
    def derived: Boolean = name.starts(t"derived$$")

    // Whether this is an export: every declaration a forwarder, however many the clause made (a
    // type and a term, or one per overload), which is one member of what it forwards to.
    def exported: Boolean =
      alias.present || (!declarations.nil && !declarations.exists(!_.exported))

    // A parameter, which its parent's page lists apart from its members.
    def isParameter: Boolean = !declarations.nil && declarations.all(_.kind.isParameter)

  // What precedes an entity's key in its address, and its path in its URL: nothing for Scala's,
  // whose addresses are their keys, and for any other format its `id` and a `/`, which no path
  // holds (a name's `/` is encoded) and no package's key begins with.
  def prefix(format: Text): Text = if format == t"scala" then t"" else t"$format/"

  // The key of an entity: its parent's qualified name, the separator its parent's kind gives
  // (`#` after a type, `.` after a term or a package), and its name.
  def key(parent: Optional[Locator], name: Text): Text =
    parent.lay(name): (parent: Locator) => t"${parent.qualified}${parent.separator}$name"

  def key(locator: Locator): Text = key(locator.parent, locator.name)

  // The path of an entity's page, beneath `/api/`: as its key, with `~` for `#` and every name
  // URL-encoded, so that a symbolic name travels.
  def path(parent: Optional[Locator], name: Text): Text =
    parent.lay(encode(name)): (parent: Locator) =>
      val mark: Text = if parent.isType then t"~" else t"."
      t"${path(parent.parent, parent.name)}$mark${encode(name)}"

  private def encode(name: Text): Text = name.urlEncode

  // The key a path names, back from its separators; each name's form is its separator's: a
  // name before `~` is a type's, and one before `.`, or at the end, a term's.
  def unpath(path: Text): Locator =
    def recur(rest: Text, designator: Optional[Locator]): Locator =
      val separator: Optional[Ordinal] = rest.where { (char: Char) => char == '.' || char == '~' }

      separator.lay(Locator(designator, rest.urlDecode, false)): (index: Ordinal) =>
        val name: Text = rest.keep(index.n0).urlDecode
        val isType: Boolean = rest.skip(index.n0).starts(t"~")
        recur(rest.skip(index.n0 + 1), Locator(designator, name, isType))

    recur(path, Unset)

  def empty: Api = Api(Map())

// The entities of one format, by key (which the inspection builds and the overlay amends);
// `roots` are the packages with no parent. The `format` is what the entities are declared in,
// which says how they are drawn and what their pages show.
case class Api(entities: Map[Text, Api.Entity], format: Format = ScalaFormat):
  def apply(key: Text): Optional[Api.Entity] = entities.at(key)

  // A locator as this format has it: one whose top-level names have one form names the term.
  def normal(locator: Locator): Locator = if format.typedRoots then locator else locator.rooted

  def has(designator: Locator): Boolean = entities.at(Api.key(normal(designator))).present
  def locate(path: Text): Optional[Api.Entity] = apply(Api.key(normal(Api.unpath(path))))

  def children(entity: Api.Entity): List[Api.Entity] =
    entity.members.bind: (key: Text) => apply(key).let(List(_)).or(Nil)

  lazy val roots: List[Api.Entity] = entities.values.filter(_.parent.absent).order(_.name)

  // The roots with any of the project's own entities at or beneath them, and the others, which
  // are its dependencies': found once, since it takes every own entity for every root.
  private lazy val rooted: (List[Api.Entity], List[Api.Entity]) =
    val own: List[Text] = entities.keys.to[List].filter: (key: Text) =>
      entities.at(key).lay(false)(_.own)

    roots.partition: (root: Api.Entity) =>
      val key: Text = root.key
      val within: Text = t"$key."
      val member: Text = t"$key#"
      own.exists: (owned: Text) => owned == key || owned.starts(within) || owned.starts(member)

  def ownRoots: List[Api.Entity] = rooted(0)
  def otherRoots: List[Api.Entity] = rooted(1)

  // The navigation of the project's own entities with no page current, a node for each of
  // `ownRoots`: drawn once, and gone with this `Api`.
  lazy val navigation: List[pyrocosm.Block.TreeNode] = ApiPages.navigation(this)

  // The entity a designator names, if it exists as a term or a type member of its parent: a
  // designator written with `.` may name a type (`gossamer.Text`), so both are tried.
  def find(designator0: Locator): Optional[Api.Entity] =
    val designator: Locator = normal(designator0)

    apply(Api.key(designator)).or:
      designator.parent.let: (parent: Locator) =>
        apply(Api.key(normal(parent.tpe), designator.name))

  // The entity a designator names, as `find` finds it, else through what its prefix forwards
  // to — an export or an exported type's alias, so that `soundness.Text.s` is the member `s`
  // of `anticipation.internal.Text`, which `soundness.Text` exports — and, for a member of a
  // type, among the members it inherits, from the nearest base which declares one.
  def resolve(designator: Locator): Optional[Api.Entity] =
    def recur(designator: Locator, depth: Int): Optional[Api.Entity] =
      find(designator).or:
        if depth <= 0 then Unset else designator.parent.let: (parent: Locator) =>
          recur(parent, depth - 1).let: (owner: Api.Entity) =>
            val target: Api.Entity = through(owner)

            // A locator's parent is a type where `#` follows it, and a term where `.` does.
            val ofType: Boolean = parent.isType
            val rebased: Locator = if ofType then target.tpe else target.term

            val forwarded: Optional[Api.Entity] =
              if target.key == owner.key then Unset
              else find(designator.copy(parent = rebased))

            forwarded.or(if ofType then inherited(target, designator.name) else Unset)

    recur(designator, 16)

  // What an entity forwards to, through every export and exported type's alias, to a depth
  // which ends a cycle; the entity itself when it forwards to nothing in the model.
  def through(entity: Api.Entity, depth: Int = 8): Api.Entity =
    forwards(entity).let(apply).lay(entity): (target: Api.Entity) =>
      if depth <= 0 || target.key == entity.key then target else through(target, depth - 1)

  // A member a type inherits by its name: the first of its bases, nearest first, to declare one.
  def inherited(entity: Api.Entity, name: Text): Optional[Api.Entity] =
    entity.bases.seek { (base: Text) => apply(t"$base#$name").present }.let: (base: Text) =>
      apply(t"$base#$name")

  // Whether an extension reaches its receiver without an import: from the companion of the
  // type or of a base of it, or from an object enclosing the type, which is a prefix of it and
  // so in its implicit scope — as an opaque type's object is, which declares its extensions —
  // though a package enclosing it is not.
  def implicitly(applicable: Api.Applicable): Boolean =
    applicable.reach == 0 || applicable.reach == 1 &&
      applicable.importFrom.let(apply).let(!_.isPackage).or(false)

  // The keys of the extensions some type reaches without an import, which a bare reference to
  // an extension may name.
  lazy val unimported: Set[Text] =
    entities.values.bind(_.extensions.filter(implicitly).map(_.key)).to[Set]

  // The entities which forward to each entity, by the key of what they forward to: the
  // packages and objects a name may be reached through besides its own. A forwarder is an
  // export, or a type alias standing for another type over exactly its own parameters, as an
  // exported type is compiled to (`type Optional[value] = vacuous.Optional[value]`).
  def forwards(entity: Api.Entity): Optional[Text] =
    entity.alias.let(Api.key(_)).or:
      entity.template.let: (d: Api.Declaration) =>
        d.aliasOf.let:
          case Api.Shape.Named(key, args) if key != entity.key =>
            val params: List[Text] = args.map:
              case Api.Shape.Param(name, _) => name
              case _                        => t""

            if params == d.aliasParams then key else Unset

          case _ => Unset

  lazy val exporters: Map[Text, List[Api.Entity]] =
    entities.values.filter(forwards(_).present).group { (e: Api.Entity) => forwards(e).or(t"") }

  // Every entity of a simple name, for a reference to be resolved among and for search.
  lazy val byName: Map[Text, List[Api.Entity]] =
    entities.values.group(_.name).map { (group: List[Api.Entity]) => group.order(_.key) }

  // Every entity with a keyword, by the keyword in lower case: what a search for the keyword
  // finds, besides the entities of that name.
  lazy val byKeyword: Map[Text, List[Api.Entity]] =
    val pairs: List[(Text, Api.Entity)] = entities.values.bind: (e: Api.Entity) =>
      e.doc.lay(List[(Text, Api.Entity)]()): (doc: Api.Entry) =>
        doc.keywords.map { (keyword: Text) => (keyword.lower, e) }

    pairs.group(_(0)).map: (group: List[(Text, Api.Entity)]) =>
      group.map(_(1)).order(_.key)

  // A shape with every alias it names expanded to what the alias stands for, its parameters
  // substituted, to a depth which ends a cycle; an export forwarder is followed to its target.
  def expand(shape: Api.Shape, depth: Int = 8): Api.Shape = shape match
    case Api.Shape.Named(key, args) =>
      val expanded: List[Api.Shape] = args.map(expand(_, depth))
      val entity: Optional[Api.Entity] = apply(key)

      val alias: Optional[Api.Declaration] =
        entity.let(_.template).let: (d: Api.Declaration) => if d.aliasOf.present then d else Unset

      // An export forwarder's page is its target's, so its name is the target's.
      val exported: Optional[Text] = entity.let(_.alias).let(Api.key(_))

      if alias.absent && exported.present && depth > 0
      then expand(Api.Shape.Named(exported.or(key), expanded), depth - 1)
      else alias.lay(Api.Shape.Named(key, expanded)): (d: Api.Declaration) =>
        if depth <= 0 then Api.Shape.Other else
          val binding: Map[Text, Api.Shape] = d.aliasParams.zip(expanded).to[Map]

          def over(shape: Api.Shape): Api.Shape = shape match
            case Api.Shape.Param(name, _)   => binding.at(name).or(shape)
            case Api.Shape.Named(k, as)     => Api.Shape.Named(k, as.map(over))
            case other                      => other

          expand(over(d.aliasOf.or(Api.Shape.Other)), depth - 1)

    case other => other

  // The subtyping of every type the model names, and the search over its declarations: each
  // built once, on first use, and gone with this `Api`.
  lazy val lattice: Lattice = Lattice.build(this)
  lazy val search: Search = Search.build(this)

  def add(entity: Api.Entity): Api = copy(entities = entities.define(entity.key, entity))

  // Two models as one: an entity in both keeps the project's own declaration, and its members
  // are those of either; the members are relinked afterwards.
  def merge(other: Api): Api =
    val combined: Map[Text, Api.Entity] =
      other.entities.keys.to[List].fold(entities): (acc: Map[Text, Api.Entity], key: Text) =>
        other.entities.at(key).lay(acc): (entity: Api.Entity) =>
          val merged: Api.Entity = acc.at(key).lay(entity): (existing: Api.Entity) =>
            if entity.own && !existing.own then entity.copy(doc = existing.doc.or(entity.doc))
            else
              existing.copy
                ( definition = existing.definition.or(entity.definition),
                  template   = existing.template.or(entity.template),
                  overloads  = existing.overloads + entity.overloads )

          acc.define(key, merged)

    copy(entities = combined).relinked

  // Every entity's `members`, from the parents its children name, sorted by name.
  def relinked: Api =
    def parented(entity: Api.Entity): List[(Text, Api.Entity)] =
      entity.parent.lay(List[(Text, Api.Entity)]()): (p: Locator) =>
        List((Api.key(p), entity))

    val children: Map[Text, List[(Text, Api.Entity)]] = entities.values.bind(parented).group(_(0))

    // Each entity is replaced where it is, so the map keeps its order.
    val linked: Map[Text, Api.Entity] =
      entities.keys.to[List].fold(entities): (acc: Map[Text, Api.Entity], key: Text) =>
        acc.at(key).lay(acc): (entity: Api.Entity) =>
          val known: List[Api.Entity] = children.at(key).or(Nil).map(_(1))
          acc.define(key, entity.copy(members = known.order(_.name).map(_.key)))

    copy(entities = linked)

  def update(key: Text)(change: Api.Entity => Api.Entity): Api =
    apply(key).lay(this): (entity: Api.Entity) => add(change(entity))
