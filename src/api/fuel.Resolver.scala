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

import soundness.{Language as _, Token as _, *}

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Inline, Token}
import stenography.{Designator, Syntax}

// The scope a page's references resolve in: the entity it is about and those enclosing it (so
// that a member may be named by its leaf), the packages and objects whose members are imported,
// and what every Scala source sees (`scala`, `scala.Predef`, `java.lang`). A name is tried as
// written, then under each enclosing entity, then under each import; one entity found is the
// link, several are an ambiguity, none is plain text.
object Scope:
  val standard: List[Designator] = List(Designator(t"scala"), Designator(t"scala.Predef"), Designator(t"java.lang"))

  // An entity's scope: itself and its parents, with the imports its overlay gives it.
  def of(entity: Api.Entity, imports: List[Text]): Scope =
    def parents(designator: Optional[Designator]): List[Designator] =
      designator.lay(Nil: List[Designator]) { (d: Designator) => List(d) + parents(d.parent) }

    Scope(List(entity.term, entity.tpe) + parents(entity.parent), imports.map(Designator(_)))

  // A document's scope: no enclosing entity, and the project's imports.
  def document(imports: List[Text]): Scope = Scope(Nil, imports.map(Designator(_)))

case class Scope(enclosing: List[Designator], imports: List[Designator]):
  def all: List[Designator] = enclosing + imports + Scope.standard

  // Whether a designator's members may be named by their leaf in this scope, which is what
  // decides how a signature elides a qualifier.
  def has(designator: Designator): Boolean = all.has(designator)

// `page` is the URL of the page the references are on, which a name the page's own fragments
// define leads to.
class Resolver(api: Api, scope0: Scope, page: Optional[Text] = Unset) extends Linker:
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var scope: Scope = scope0

  // What the page's own fragments have defined, latest first: the name, its kind and the
  // anchor of the fragment which defined it.
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var defined: List[(Text, Linker.Kind, Text)] = Nil

  private val ambiguous: juc.ConcurrentLinkedQueue[Text] = juc.ConcurrentLinkedQueue()

  def define(anchor: Text, names: List[(Text, Linker.Kind)]): Unit =
    val latest: List[(Text, Linker.Kind, Text)] =
      names.reverse.map { (entry: (Text, Linker.Kind)) => (entry(0), entry(1), anchor) }

    defined = latest + defined

  // A name the page's own fragments defined: its first segment, of the kind a sigil says if
  // one does, leads to the fragment which defined it last.
  private def fenced(name: Text, kind: Optional[Linker.Kind]): Optional[Inline.Destination] =
    page.let: (url: Text) =>
      val leaf: Text = Text(name.s.takeWhile { c => c != '.' && c != '#' })

      defined.seek { (entry: (Text, Linker.Kind, Text)) => entry(0) == leaf && kind.lay(true)(_ == entry(1)) }.let:
        (entry: (Text, Linker.Kind, Text)) => Inline.Destination.Local(url, entry(2))

  // Whether an entity is what a sigil says: for `#`, a type, or a member of a type; for `.`, a
  // term, or a member of a term. A type and its companion, one entity, answer to either; a
  // member of a package is not a member of a term, so a trait in a package is no term.
  private def suits(entity: Api.Entity, kind: Linker.Kind): Boolean =
    val ofType: Boolean = entity.parent match
      case Designator.Type(_, _) => true
      case _                     => false

    kind match
      case Linker.Kind.Type => ofType || entity.template.present || entity.definition.let(_.kind.isType).or(false)
      case Linker.Kind.Term =>
        val ofTerm: Boolean = entity.parent.let { (p: Designator) => api(Api.key(p)).let(!_.isPackage).or(false) }.or(false)
        !ofType && (entity.definition.present || ofTerm)

  def ambiguities: List[Text] =
    List.from(scala.jdk.CollectionConverters.IteratorHasAsScala(ambiguous.iterator.nn).asScala.toList.distinct)

  // `import a.b.*`, `import a.b.C` and `import a.b.{C, D as E}`: the package or object itself
  // for a wildcard, else the named members' parent (which is enough to resolve their leaves).
  def learn(imports: List[Text]): Unit =
    val learned: List[Designator] = imports.bind: (line: Text) =>
      val body: Text = line.trim.skip(t"import ".length).trim
      val cleaned: Text = Text(body.s.replace("{", "").nn.replace("}", "").nn)

      cleaned.cut(t",").bind: (clause: Text) =>
        val path: Text = clause.trim.cut(t" ").prim.or(t"")
        val segments: List[Text] = path.cut(t".").filter(_ != t"")
        val prefix: List[Text] = if segments.reverse.prim.let(_ == t"*").or(false) then segments.keep(segments.size - 1) else segments.keep(segments.size - 1)
        if prefix.nil then Nil else List(Designator(prefix.join(t".")))

    scope = scope.copy(imports = (scope.imports + learned).distinct)

  // The entity a designator written in prose names: through an exported alias to its target.
  private def entity(designator: Designator): Optional[Api.Entity] =
    api.find(designator).let { (found: Api.Entity) => found.alias.let(api.find).or(found) }

  // A designator rebased under a prefix: its first segment becomes a child of the prefix.
  private def under(prefix: Designator, designator: Designator): Designator = designator match
    case Designator.Top(name)          => Designator.Term(prefix, name)
    case Designator.Term(parent, name) => Designator.Term(under(prefix, parent), name)
    case Designator.Type(parent, name) => Designator.Type(under(prefix, parent), name)

  // As written first, then under the enclosing entities (innermost first), then under the
  // imports, then the standard scope: the first tier which names anything decides, as a name
  // in code resolves in its innermost scope, so an outer match is no ambiguity. A sigil keeps
  // only the entities of its kind in every tier, which is how `#title` on `Doc`'s page finds
  // `Doc#title` through the enclosing tier, and `#Doc` under an import finds the type.
  private def candidates(designator: Designator, kind: Optional[Linker.Kind]): List[Api.Entity] =
    def fit(entities: List[Api.Entity]): List[Api.Entity] =
      kind.lay(entities) { (kind: Linker.Kind) => entities.filter(suits(_, kind)) }

    def tier(prefixes: List[Designator]): List[Api.Entity] =
      fit(prefixes.bind { (prefix: Designator) => entity(under(prefix, designator)).let(List(_)).or(Nil) }.distinct)

    val tiers: List[List[Api.Entity]] =
      List(fit(entity(designator).let(List(_)).or(Nil)), tier(scope.enclosing), tier(scope.imports), tier(Scope.standard))

    tiers.seek(!_.nil).or(Nil)

  // The page of an entity, by the name it is written with; a type member is a page of its own,
  // as a term member is.
  private def destination(entity: Api.Entity): Inline.Destination = Inline.Destination.Local(entity.url)

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

    def accented(token: Token): Token = accent.lay(token) { (accent: Token.Accent) => token.copy(accent = accent) }

    val qualified: Boolean = name.s.indexOf('.') > 0 || name.s.indexOf('#') > 0

    if whole.absent then List(Token.plain(reference))
    else if !qualified then List(accented(Token.plain(name).copy(link = whole)))
    else
      // The segments with the separators between them, in order: `a.b#C` is `a`, `.`, `b`,
      // `#`, `C`.
      def hashed(segment: Text): List[Text] = segment.cut(t"#").flatMap(List(t"#", _)).skip(1)
      val parts: List[Text] = name.cut(t".").map(hashed).flatMap(List(t".") + _).skip(1)

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

    def plausible: Boolean =
      !name.s.isEmpty && !name.s.contains(" ") && !name.s.contains("(") && !name.s.contains("[") && !name.s.contains("\"")
      && name.s.forall { c => c.isLetterOrDigit || c == '.' || c == '#' || c == '_' || c == '$' || c == '`' }

    if !plausible then Unset else fenced(name, kind).or:
      candidates(Designator(name), kind) match
        case one :: Nil => destination(one)
        case Nil        => Unset
        case many       =>
          // The same page reached by several routes is no ambiguity.
          if many.map(_.key).distinct.size == 1 then many.prim.let(destination) else
            if record then ambiguous.add(text)
            Unset

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
