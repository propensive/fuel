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

import soundness.*

import dysasymptotics.linearSize
import pyrocosm.{Inline, Purview, Token}
import stenography.{Designator, Syntax}

// The scope a page's references resolve in: the entity it is about and those enclosing it (so
// that a member may be named by its leaf), the names imported one by one (`named`: each as it
// is written, and what it names), the packages and objects whose every member is imported
// (`imports`), and what every Scala source sees (`scala`, `scala.Predef`, `java.lang`). A name
// is tried as written, then under each enclosing entity, then as a name imported, then under
// each import; one entity found is the link, several are an ambiguity, none is plain text.
object Scope:
  val standard: List[Locator] =
    List(Locator(t"java.lang"), Locator(t"scala"), Locator(t"scala.Predef"))

  // What every source of a project sees without importing it, in the order the compiler
  // imports them, each shadowing those before it: the compiler's own three packages, or those
  // the project's `-Yimports` option names instead, less `scala.Predef`
  // under `-Yno-predef` and all of them under `-Yno-imports`. A name from one of them is
  // written by its leaf, as the project's sources write it.
  def standardOf(options: List[Text]): List[Locator] =
    // The option is written `-Yimports:a,b` or as two words, which is how the configuration
    // keeps it, since its options are cut at colons.
    def value(options: List[Text]): Optional[Text] = options match
      case option :: rest if option.starts(t"-Yimports:") => option.skip(t"-Yimports:".length)
      case t"-Yimports" :: value :: _                     => value
      case _ :: rest                                      => value(rest)
      case _                                              => Unset

    val named: Optional[List[Locator]] =
      value(options).let: (value: Text) => value.cut(t",").filter(_ != t"").map(Locator(_))

    val base: List[Locator] =
      if options.has(t"-Yno-imports") then Nil else named.or(standard)

    if options.has(t"-Yno-predef") then base.filter(_ != Locator(t"scala.Predef")) else base

  // An entity's scope: itself and its parents, with the imports its overlay gives it.
  def of(entity: Api.Entity, imports: List[Text], standard: List[Locator] = standard): Scope =
    def parents(designator: Optional[Locator]): List[Locator] =
      designator.lay(Nil: List[Locator]): (d: Locator) => List(d) + parents(d.parent)

    val enclosing: List[Locator] = List(entity.term, entity.tpe) + parents(entity.parent)
    Scope(enclosing, imports.map(Locator(_)), standard)

  // A document's scope: no enclosing entity, and the project's imports.
  def document(imports: List[Text], standard: List[Locator] = standard): Scope =
    Scope(Nil, imports.map(Locator(_)), standard)

  // The imports as a reader's starting purview: each is the page of the package or object it
  // names, which is where a renderer elides them from, and what a reader may toggle.
  def purview(imports: List[Text]): Purview =
    Purview(imports.map { (name: Text) => Purview.Entry(t"/api/$name", name) })

case class Scope
  ( enclosing: List[Locator],
    imports:   List[Locator],
    standard:  List[Locator]         = Scope.standard,
    named:     List[(Text, Locator)] = Nil ):

  def all: List[Locator] = enclosing + imports + standard

  // Whether a designator's members may be named by their leaf in this scope, which is what
  // decides how a signature elides a qualifier. The imports resolve a reference but do not
  // elide: a page is rendered once and read under each reader's purview (Pyrocosm's), which
  // begins as these imports and is what elides them, so a signature writes them in full.
  def has(designator: Locator): Boolean = (enclosing + standard).has(designator)

  // The same, of an entity's key.
  def names(key: Text): Boolean = (enclosing + standard).exists(Api.key(_) == key)

// `page` is the URL of the page the references are on, which a name the page's own fragments
// define leads to.
// A reference which begins with another format's `id` and a `/` (`tels/presentation.Slide`) is
// resolved in that format's API, which the `apis` has; one nothing in this API answers is
// tried in each of the others, as written and through those of the scope's imports which name
// that format, and is the entity one of them has, or an ambiguity when several have one.
class Resolver
  ( api: Api, scope0: Scope, page: Optional[Text] = Unset, apis: Optional[Apis] = Unset )
extends Linker:
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var scope: Scope = scope0

  // What the page's own fragments have defined, latest first: the name, its kind and the
  // anchor of the fragment which defined it.
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var defined: List[(Text, Linker.Kind, Text)] = Nil

  // The references found ambiguous, latest first, each with the keys of what it named.
  private val ambiguous: Atomic.Ref[List[Linker.Ambiguity]] =
    Atomic(Nil: List[Linker.Ambiguity])

  private def ambiguity(reference: Text, candidates: List[Text] = Nil): Unit =
    ambiguous.since(Linker.Ambiguity(reference, candidates) :: _)

  def define(anchor: Text, names: List[(Text, Linker.Kind)]): Unit =
    val latest: List[(Text, Linker.Kind, Text)] =
      names.reverse.map: (entry: (Text, Linker.Kind)) => (entry(0), entry(1), anchor)

    defined = latest + defined

  // A name the page's own fragments defined: its first segment, of the kind a sigil says if
  // one does, leads to the fragment which defined it last.
  private def fenced(name: Text, kind: Optional[Linker.Kind]): Optional[Inline.Destination] =
    page.let: (url: Text) =>
      val leaf: Text = name.keep { char => char != '.' && char != '#' }

      def matches(entry: (Text, Linker.Kind, Text)): Boolean =
        entry(0) == leaf && kind.lay(true)(_ == entry(1))

      defined.seek(matches).let: (entry: (Text, Linker.Kind, Text)) =>
        Inline.Destination.Local(url, entry(2))

  // Whether an entity is what a sigil says: for `#`, a type, or a member of a type; for `.`, a
  // term, or a member of a term. A type and its companion, one entity, answer to either; a
  // member of a package is not a member of a term, so a trait in a package is no term.
  private def suits(entity: Api.Entity, kind: Linker.Kind): Boolean =
    val ofType: Boolean = entity.parent.let(_.isType).or(false)

    kind match
      case Linker.Kind.Type =>
        ofType || entity.template.present || entity.definition.let(_.kind.isType).or(false)

      case Linker.Kind.Term =>
        val ofTerm: Boolean =
          entity.parent.lay(false): (p: Locator) => api(Api.key(p)).let(!_.isPackage).or(false)

        !ofType && (entity.definition.present || ofTerm)

  def ambiguities: List[Linker.Ambiguity] = ambiguous().reverse.distinct

  // What a fragment's imports bring into scope, as code has it: `import a.b.*` (or `a.b.given`)
  // every member of the package or object `a.b`, and `import a.b.C` the name `C` alone, so that
  // another member of `a.b` is no more in scope than it was. Of `import a.b.{C, D as E, F as _,
  // *}`, `C` is `a.b.C`, `E` is `a.b.D`, `F` is nothing, and `*` is every member; a line's
  // clauses are separated by commas outside its braces.
  def learn(imports: List[Text]): Unit =
    // The clauses of a line, cut at each comma which no brace encloses.
    def clauses(parts: List[Text], open: Optional[Text], done: List[Text]): List[Text] =
      parts match
        case part :: rest =>
          val text: Text = open.lay(part) { (open: Text) => t"$open,$part" }

          if text.contains(t"{") && !text.contains(t"}") then clauses(rest, text, done)
          else clauses(rest, Unset, text.trim :: done)

        case _ =>
          done.reverse

    // One selector of a prefix: every member, or a name and what it names.
    def selector(prefix: Text, text: Text): Optional[Locator | (Text, Locator)] =
      text.cut(t" ").filter(_ != t"") match
        case (t"*" | t"_" | t"given") :: _             => Locator(prefix)
        case _ :: (t"as" | t"=>") :: t"_" :: _         => Unset
        case name :: (t"as" | t"=>") :: renamed :: _   => (renamed, Locator(t"$prefix.$name"))
        case name :: _                                 => (name, Locator(t"$prefix.$name"))
        case _                                         => Unset

    def clause(text: Text): List[Locator | (Text, Locator)] =
      val braced: Optional[Ordinal] = text.where(_ == '{')

      val (prefix, selectors) = braced.lay((t"", List(text))): (brace: Ordinal) =>
        val inner: Text = text.skip(brace.n0 + 1).sub(t"}", t"")
        (text.keep(brace.n0).trim, inner.cut(t","))

      // Without braces, the selector is what follows the last `.`.
      val (path, picked) = if braced.present then (prefix, selectors) else
        val segments: List[Text] = text.cut(t".").map(_.trim).filter(_ != t"")
        (segments.keep(segments.size - 1).join(t"."), segments.reverse.keep(1))

      val base: Text = if path.ends(t".") then path.skip(1, Rtl) else path
      if base == t"" then Nil else picked.bind(selector(base, _).let(List(_)).or(Nil))

    val learned: List[Locator | (Text, Locator)] = imports.bind: (line: Text) =>
      val body: Text = line.trim.skip(t"import ".length).trim
      clauses(body.cut(t","), Unset, Nil).bind(clause)

    val wildcards: List[Locator] = learned.bind:
      case locator: Locator => List(locator)
      case _                => Nil

    // The latest import of a name is the one it has.
    val names: List[(Text, Locator)] =
      learned.bind:
        case (name: Text, locator: Locator) => List((name, locator))
        case _                              => Nil

      . reverse

    scope =
      scope.copy
        ( imports = (scope.imports + wildcards).distinct,
          named   = (names + scope.named).distinct )

  // The entity a designator written in prose names, through what its prefix forwards to and
  // what a type inherits, then — where it is an export, whether of a term, of a type or of a
  // type with parameters — through to what it exports, whose page is its own.
  private def entity(designator: Locator): Optional[Api.Entity] =
    api.resolve(designator).let: (found: Api.Entity) =>
      if found.exported then api.through(found) else found

  // The keys of the packages and objects whose members are in scope, which an extension
  // declared in one of them is reached through.
  private def reachable: Set[Text] = scope.all.map(Api.key(_)).to[Set]

  // The keys of what is imported by its name.
  private def chosen: Set[Text] =
    scope.named.map { (entry: (Text, Locator)) => Api.key(entry(1)) }.to[Set]

  // Whether an extension reaches its receiver here: without an import (it is in the companion
  // of the receiver or of a base, or in an object enclosing it), or through what is in scope —
  // the object or package which declares it, or one which exports it — or by its own name,
  // or an export's of it, imported.
  private def admitted(applicable: Api.Applicable): Boolean =
    val keys: Set[Text] = reachable
    val names: Set[Text] = chosen

    def exported: Boolean = api.exporters.at(applicable.key).or(Nil).exists:
      (forwarder: Api.Entity) =>
        names.has(forwarder.key) ||
          forwarder.parent.lay(false) { (p: Locator) => keys.has(Api.key(p)) }

    api.implicitly(applicable) || applicable.importFrom.lay(false)(keys.has(_)) ||
      names.has(applicable.key) || exported

  // The extensions of a name which apply to a receiver, written as its members
  // (`Angle#principal`): the receiver's own (an alias seen through) and those of every value,
  // as far as they reach it here, of the closest tier which has any.
  private def extensions(receiver: Api.Entity, name: Text): List[Api.Entity] =
    val target: Api.Entity = api.through(receiver)
    val universal: List[Api.Applicable] = api(Lattice.any).let(_.extensions).or(Nil)

    val applicable: List[Api.Applicable] = (target.extensions + universal).filter:
      (applicable: Api.Applicable) =>
        api(applicable.key).let(_.name == name).or(false) && admitted(applicable)

    applicable.map(_.tier).minimum.lay(Nil): (lowest: Int) =>
      val closest: List[Api.Applicable] = applicable.filter(_.tier == lowest)
      closest.bind { (a: Api.Applicable) => api(a.key).let(List(_)).or(Nil) }.distinct

  // An extension named by its leaf alone (`.stdlib`): every one of the name which reaches some
  // receiver without an import, or which is declared in what is in scope.
  private def bare(designator: Locator): List[Api.Entity] =
    if designator.parent.present then Nil else
      val keys: Set[Text] = reachable
      val names: Set[Text] = chosen

      api.byName.at(designator.name).or(Nil).filter: (entity: Api.Entity) =>
        val extension: Boolean = entity.declarations.exists(_.kind == Api.Kind.Extension)

        val inScope: Boolean =
          names.has(entity.key) ||
            entity.parent.lay(false) { (p: Locator) => keys.has(Api.key(p)) }

        extension && (api.unimported.has(entity.key) || inScope)

  // A locator rebased under a prefix: its first segment becomes a child of the prefix.
  private def under(prefix: Locator, designator: Locator): Locator =
    designator.copy(parent = designator.parent.lay(prefix)(under(prefix, _)))

  // A locator whose first name is one imported by name, as each such import has it: the first
  // name gives way to what it names, in the form it is written in.
  private def renamed(designator: Locator): List[Locator] = designator.parent match
    case parent: Locator =>
      renamed(parent).map { (parent: Locator) => designator.copy(parent = parent) }

    case _ =>
      scope.named.filter(_(0) == designator.name).keep(1).map: (entry: (Text, Locator)) =>
        entry(1).copy(isType = designator.isType)

  // As written first, then under the enclosing entities (innermost first), then as a name
  // imported, then under the imports of every member, then the standard scope: the first tier
  // which names anything decides, as a name in code resolves in its innermost scope and a name
  // imported shadows a wildcard's, so an outer match is no ambiguity. A sigil keeps
  // only the entities of its kind in every tier, which is how `#title` on `Doc`'s page finds
  // `Doc#title` through the enclosing tier, and `#Doc` under an import finds the type.
  //
  // A member of a type which no entity answers (`Angle#principal`) is sought among the
  // extensions which apply to the type; and last, a bare name is sought among the extensions
  // in reach (`.stdlib`), several of them on different receivers being an ambiguity.
  private def candidates(designator: Locator, kind: Optional[Linker.Kind]): List[Api.Entity] =
    def fit(entities: List[Api.Entity]): List[Api.Entity] =
      kind.lay(entities): (kind: Linker.Kind) => entities.filter(suits(_, kind))

    // A member written after `#` is a member of a type, which an extension of the type is, as
    // a reference to it is written: so a `.` sigil, which says a term, admits none.
    val typeMember: Boolean = kind.lay(true)(_ == Linker.Kind.Type)

    def lookup(designator: Locator): List[Api.Entity] =
      val found: List[Api.Entity] = fit(entity(designator).let(List(_)).or(Nil))

      // A name after `#` is a member of the type before it.
      if !found.nil then found else designator.parent match
        case parent: Locator if parent.isType && typeMember =>
          entity(parent).lay(Nil: List[Api.Entity])(extensions(_, designator.name))

        case _ =>
          Nil

    def tier(prefixes: List[Locator]): List[Api.Entity] =
      prefixes.bind { (prefix: Locator) => lookup(under(prefix, designator)) }.distinct

    // The standard imports shadow one another, a later one the earlier — `scala.Double`
    // shadows `java.lang.Double` — so each is a tier of its own, the last first.
    val standard: List[List[Api.Entity]] =
      scope.standard.reverse.map { (prefix: Locator) => tier(List(prefix)) }

    // An extension is named by its leaf alone only where a sigil says the name is a term.
    val extension: List[Api.Entity] =
      if kind == Linker.Kind.Term then fit(bare(designator)) else Nil

    val named: List[Api.Entity] = renamed(designator).bind(lookup).distinct

    val tiers: List[List[Api.Entity]] =
      List(lookup(designator), tier(scope.enclosing), named, tier(scope.imports)) + standard +
        List(extension)

    tiers.seek(!_.nil).or(Nil)

  // The page of an entity, by the name it is written with; a type member is a page of its own,
  // as a term member is.
  private def destination(entity: Api.Entity): Inline.Destination =
    Inline.Destination.Local(entity.url)

  def link(reference: Text): Optional[Inline.Destination] = resolve(reference, true)

  // A qualified name which resolves, as a token for each segment and each separator, each
  // segment linked to what the name up to it resolves to in the same scope — so a prefix
  // written through an export links where the export leads — and each token after the first
  // chained to the name before it. A sigil is dropped once the reference resolves, and the
  // name's last segment takes the accent of its kind; a reference which does not resolve is
  // one token, as written.
  override def tokens(reference: Text): List[Token] =
    val text: Text = reference.trim
    val kind: Optional[Linker.Kind] = Linker.kind(text)
    val name: Text = if kind.present then text.skip(1) else text
    val whole: Optional[Inline.Destination] = link(reference)

    val accent: Optional[Token.Accent] = kind.let:
      case Linker.Kind.Type => Token.Accent.Typal
      case Linker.Kind.Term => Token.Accent.Term

    def accented(token: Token): Token =
      accent.lay(token): (accent: Token.Accent) => token.copy(accent = accent)

    // Whether a character is in the name, after its start.
    def inner(char: Char): Boolean = name.where(_ == char).lay(false)(_.n0 > 0)
    val qualified: Boolean = inner('.') || inner('#')

    if whole.absent then List(Token.plain(reference))
    else if !qualified then List(accented(Token.plain(name).copy(link = whole)))
    else
      // The segments with the separators between them, in order: `a.b#C` is `a`, `.`, `b`,
      // `#`, `C`.
      def hashed(segment: Text): List[Text] = segment.cut(t"#").bind(List(t"#", _)).skip(1)
      val parts: List[Text] = name.cut(t".").map(hashed).bind(List(t".") + _).skip(1)

      def recur(parts: List[Text], written: Text, first: Boolean): List[Token] = parts match
        case part :: rest if part == t"." || part == t"#" =>
          Token.plain(part).copy(chained = true) :: recur(rest, written + part, false)

        case part :: rest =>
          val prefix: Text = written + part

          val token: Token =
            if rest.nil then accented(Token.plain(part).copy(link = whole, chained = !first))
            else Token.plain(part).copy(link = resolve(prefix, false), chained = !first)

          token :: recur(rest, prefix, false)

        case _ =>
          Nil

      recur(parts, t"", true)

  // A reference resolved, recording it as ambiguous when several entities answer it and
  // `record` says so: a prefix of a name already resolved is not reported again. A sigil
  // (`#Name`, `.name`) says the kind; the name after it is then resolved as any other, first
  // among what the page's own fragments defined, then through the API's tiers.
  private def resolve(reference: Text, record: Boolean): Optional[Inline.Destination] =
    val text: Text = reference.trim
    val kind: Optional[Linker.Kind] = Linker.kind(text)
    val name: Text = if kind.present then text.skip(1) else text

    val sigil: Text = if kind.present then text.keep(1) else t""

    // Every character a name may hold; a space, a bracket or a quote is none of them.
    def legal(char: Char): Boolean = char.isLetterOrDigit || t".#_$$`".contains(char)

    def plausible: Boolean = name != t"" && name.all(legal)

    // Another format's resolver: its API, and the imports which name it, without its `id`.
    def foreign(other: Api): Resolver =
      val lead: Text = t"${other.format.id}/"

      val imports: List[Locator] = scope.imports.map(_.qualified).filter(_.starts(lead)).map:
        (name: Text) => Locator(name.skip(lead.length))

      Resolver(other, Scope(Nil, imports, other.format.standard(Nil)))

    // What the other formats make of the reference, where this one makes nothing of it.
    def elsewhere: Optional[Inline.Destination] =
      val others: List[Api] = apis.lay(Nil: List[Api])(_.all.filter(_.format != api.format))

      val found: List[Inline.Destination] = others.bind: (other: Api) =>
        foreign(other).link(text).let(List(_)).or(Nil)

      found match
        case one :: Nil => one
        case Nil        => Unset
        case many =>
          if record then ambiguity(text)
          Unset

    def here: Optional[Inline.Destination] =
      if !plausible then elsewhere else fenced(name, kind).or:
        candidates(Locator(name), kind) match
          case one :: Nil => destination(one)
          case Nil        => elsewhere
          case many =>
            // The same page reached by several routes is no ambiguity.
            if many.map(_.key).distinct.size == 1 then many.head.let(destination) else
              if record then ambiguity(text, many.map(_.address).distinct)
              Unset

    apis.let(_.split(name)) match
      case (other: Api, rest: Text) =>
        if other.format == api.format then here else foreign(other).link(t"$sigil$rest")

      case _ =>
        here

  // A token whose type is exactly what it names: a type's token, a stable value's, or the
  // head of an applied type.
  def link(name: Text, tpe: Syntax): Optional[Inline.Destination] =
    def named(designator: Designator): Optional[Inline.Destination] =
      if designator.name == name then entity(designator).let(destination) else Unset

    tpe match
      case Syntax.Simple(designator)                      => named(designator)
      case Syntax.Value(designator)                       => named(designator)
      case Syntax.Application(Syntax.Simple(d), _, _)     => named(d)
      case Syntax.Capturing(Syntax.Simple(d), _)          => named(d)
      case _                                              => Unset
