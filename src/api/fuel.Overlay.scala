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

import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.sortingAlgorithms.timsort

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Tone}

// The documentation of an API, in Amok's `Flux` format: a TEL file per package (`base`) — or
// per top-level name of another `format`, a schema say, which the file names by its `id` — with
// an `entry` per member — `.name` for a member of the term, `#Name` for a member of the type,
// nested as the members are — each holding a one-line `info` (its inline atoms), a Markdown
// `detail` (its source atom), whether it is `hidden`, the version it was removed `until` and
// what replaces it, the entities it `refer`s to, and the `import`s its prose resolves
// references through; the file's own `info`, `detail` and `import`s are the package's.
//
// Reading an overlay holds it to its format: a keyword the format does not have, one given
// twice where it is given once, an entry's info (or detail) given both on its line and as a
// child, two entries for the same member, and an `info` longer than `InfoLimit` characters as
// it reads, are each a failure at its line. Merging an overlay onto the model is where
// documentation and code are held to each other: an entry naming a member nothing declares is
// a failure (unless it is marked `until`, when it has a page which says what replaced it), an
// `until` on a member which still exists is a caution, and a public member of a documented
// package with no entry is a caution at the place it is declared.
object Overlay:
  case class Entry(name: Text, isType: Boolean, doc: Api.Entry, entries: List[Entry], line: Int)

  // The most an `info` may say, in characters of its text as it reads: a line, not a paragraph.
  val InfoLimit: Int = 140

  // What a file may hold, what an entry may, and which of those it holds at most once; the
  // rest hold atoms alone.
  private val fileKeywords: List[Text] =
    List
      ( t"format", t"base", t"import", t"info", t"detail", t"refer", t"hidden", t"until",
        t"entry", t"keyword" )

  private val entryKeywords: List[Text] =
    fileKeywords.filter { (keyword: Text) => keyword != t"base" && keyword != t"format" }

  private val once: List[Text] =
    List(t"format", t"base", t"info", t"detail", t"hidden", t"until")

  // A compound of the file, with its path, by which its line is found (`Compounds`).
  private type Placed = (Tel.Compound, List[Text])

  // An overlay read from its text, or the diagnostic which says why it could not be; `prose`
  // is how its prose — each `info`, `detail` and `keyword` — reads, which for an overlay's
  // translation is one of the two readings of what is written (`OverlayTranslations`).
  def read
    ( path:  Text,
      text:  Text,
      prose: Text -> Text = (text: Text) => text )
  :   Overlay | Block.Diagnostic =

    import parsing.trackPositions

    recover:
      case error: Tel.Error => Block.Diagnostic.of(error, path, text)

    . protect:
        val tel: Tel = text.read[Tel]
        val roots: List[Placed] = Compounds.paths(Nil, tel.childCompounds.to[List])

        val base: Optional[Text] =
          tel.fields(t"base").to[List].head.let(_.primaryAtom).let(_.puncture(t""))

        val missing: Block.Diagnostic =
          val message: Text = t"An overlay must name the package it documents with `base`."
          failure(path, 1, message, 1)

        base.lay[Overlay | Block.Diagnostic](missing): (base: Text) =>
          val problems: List[Block] = check(path, tel, Unset, roots, fileKeywords, prose)

          // The format of what it documents, by its `id`: Scala's, unless it says.
          val format: Text =
            tel.fields(t"format").to[List].head.let(_.primaryAtom).let(_.puncture(t""))
            . or(t"scala")

          Overlay
            ( path, base, doc(tel, 1, Nil, prose), entries(tel, tel, Nil, Nil, prose), problems,
              format )

  private def failure(path: Text, line: Int, message: Text, variant: Int): Block.Diagnostic =
    val location: Block.Location = Block.Location(path, line)
    Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(message)), code = bolt(variant))

  // The code of an overlay's problem, `↯/f.26.4.<variant>`: an overlay without a `base` (1); a
  // keyword it does not have (2); an `entry` without a name (3); a value which holds what it
  // cannot (4); a keyword given twice (5); an entry's info (6) or detail (7) given twice; an
  // entry given twice (8); info too long (9); an entry marked as removed which exists (10); an
  // entry for no member (11); a `base` the sources do not declare (12); a member nothing
  // documents (13); and a `format` which is not one an overlay documents (14).
  private[fuel] def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.api, FuelBolts.overlay, variant)

  // The atoms of a compound, inline then its source, as they are written.
  private def texts(compound: Tel.Compound): List[Text] =
    compound.atoms.to[List].bind:
      case Tel.Atom.Inline(text, _) => List(text)
      case Tel.Atom.Source(text)    => List(text)
      case _                        => Nil

  // The failures of the children of one node: the file (`entry` unset) or an entry. Each
  // compound's line is found in `tel`, the file read with its positions, by its path.
  private def check
    ( path:     Text,
      tel:      Tel,
      entry:    Optional[Placed],
      children: List[Placed],
      allowed:  List[Text],
      prose:    Text -> Text )
  :   List[Block] =

    def line(placed: Placed): Int = Compounds.line(tel, placed(1))
    def written(keywords: List[Text]): Text = keywords.map { (k: Text) => t"`$k`" }.join(t", ")
    def named(keyword: Text): List[Placed] = children.filter(_(0).keyword == keyword)

    val each: List[Block] = children.bind: (child: Placed) =>
      val keyword: Text = child(0).keyword
      val grandchildren: List[Placed] = Compounds.paths(child(1), Compounds.children(child(0)))

      if !allowed.has(keyword) then
        val where: Text = if entry.absent then t"An overlay" else t"An entry"

        val message: Text =
          t"`$keyword` is not part of an overlay: $where may hold ${written(allowed)}."

        List(failure(path, line(child), message, 2))
      else if keyword == t"entry" then
        val unnamed: Text = t"An `entry` must name the member it documents, as `.name` or `#Name`."

        val name: List[Block] =
          if Compounds.inline(child(0)).nil then List(failure(path, line(child), unnamed, 3))
          else Nil

        name + check(path, tel, child, grandchildren, entryKeywords, prose)
      else
        grandchildren.head.lay(List[Block]()): (first: Placed) =>
          val message: Text =
            t"`$keyword` holds no `${first(0).keyword}`: it takes its value alone."

          List(failure(path, line(first), message, 4))

    val repeated: List[Block] = once.bind: (keyword: Text) =>
      named(keyword).skip(1).map: (child: Placed) =>
        failure(path, line(child), t"`$keyword` is given more than once here.", 5)

    // An entry's info may follow its name, and a leaf entry's detail may follow it as a source
    // atom, but not as well as a child which says the same.
    val doubled: List[Block] = entry.lay(List[Block]()): (entry: Placed) =>
      // The first child of a keyword, as a failure, where it is given as well as on the line.
      def already(keyword: Text, twice: Boolean, message: Text, variant: Int): List[Block] =
        val found: List[Placed] = if twice then named(keyword).keep(1) else Nil
        found.map: (child: Placed) => failure(path, line(child), message, variant)

      val onLine: Text = t"This entry's info is already given on its line."
      val beneath: Text = t"This entry's detail is already given beneath its line."
      val info: List[Block] = already(t"info", Compounds.inline(entry(0)).size > 1, onLine, 6)

      val detail: List[Block] =
        already(t"detail", Compounds.source(entry(0)).present, beneath, 7)
      info + detail

    val duplicates: List[Block] =
      val entries: List[(Text, Placed)] = named(t"entry").bind: (child: Placed) =>
        Compounds.inline(child(0)).head.lay(List[(Text, Placed)]()): (name: Text) =>
          List((name, child))

      entries.indexed.bind: (pair: (Text, Placed), index: Ordinal) =>
        if !entries.keep(index.n0).exists(_(0) == pair(0)) then Nil
        else List(failure(path, line(pair(1)), t"`${pair(0)}` already has an entry here.", 8))

    // The info as it reads, which is what the limit is on, not the Markdown it is written in.
    val long: List[Block] =
      val infoChild: Optional[Placed] = named(t"info").head

      val inline: List[Text] =
        entry.lay(List[Text]()) { (entry: Placed) => Compounds.inline(entry(0)).skip(1) }

      val info: Optional[Text] =
        infoChild.let { (child: Placed) => texts(child(0)).filter(_ != t"").join(t" ") }.or:
          if inline.nil then Unset else inline.join(t" ")

      val at: Int = infoChild.let(line(_)).or(entry.let(line(_)).or(1))

      info.lay(List[Block]()): (info: Text) =>
        val size: Int = Blocks.plain(Blocks.inline(prose(info))).length

        val message: Text =
          t"This info is ${size.show} characters long, and may be at most ${InfoLimit.show}: " +
            t"say the rest in a `detail`."

        if size <= InfoLimit then Nil else List(failure(path, at, message, 9))

    each + repeated + doubled + duplicates + long

  // The text of a node's child by keyword: its atoms, inline then the indented source, joined.
  private def child(node: Tel, keyword: Text): Optional[Text] =
    node.fields(keyword).to[List].head
    . let(_.atomTexts.to[List].filter(_ != t"").join(t" "))
    . let(_.puncture(t""))

  private def atoms(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].bind(_.atomTexts.to[List].filter(_ != t""))

  private def flag(node: Tel, keyword: Text): Boolean = !node.fields(keyword).to[List].nil

  // Each child of the keyword as one text: a keyword of several words is one keyword.
  private def phrases(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].map(_.atomTexts.to[List].filter(_ != t"").join(t" "))
    . filter(_ != t"")

  // What a node says of its entity, with the imports of those enclosing it.
  private def doc(node: Tel, line: Int, inherited: List[Text], prose: Text -> Text): Api.Entry =
    val until: Optional[Api.Until] =
      node.fields(t"until").to[List].head.let: (until: Tel) =>
        val texts: List[Text] = until.atomTexts.to[List].filter(_ != t"")
        texts.head.let: (version: Text) => Api.Until(version, texts.at(Sec))

    Api.Entry
      ( info    = child(node, t"info").let(prose),
        detail  = child(node, t"detail").let(prose),
        hidden  = flag(node, t"hidden"),
        until   = until,
        refer   = atoms(node, t"refer"),
        imports = inherited + atoms(node, t"import"),
        line    = line,
        keywords = phrases(node, t"keyword").map(prose) )

  // The entries beneath `node`, which is at `at` in `file`, the file read with its positions.
  private def entries
    ( file: Tel, node: Tel, at: List[Text], inherited: List[Text], prose: Text -> Text )
  :   List[Entry] =

    val scope: List[Text] = inherited + atoms(node, t"import")

    // A node, and the compound it wraps (whose atoms say which is inline and which the source)
    // with its path, paired by order: `fields` and `childCompounds` walk the same children.
    val nodes: List[Tel] = node.fields(t"entry").to[List]

    val compounds: List[Placed] =
      Compounds.paths(at, node.childCompounds.to[List]).filter(_(0).keyword == t"entry")

    nodes.zip(compounds).bind: (tel: Tel, placed: Placed) =>
      // The name is the first inline atom; the info the rest; the detail the source atom.
      val inline: List[Text] = Compounds.inline(placed(0))
      val source: Optional[Text] = Compounds.source(placed(0))

      inline.head.lay(List[Entry]()): (name: Text) =>
        val line: Int = Compounds.line(file, placed(1))
        val isType: Boolean = name.starts(t"#")
        val plain: Text = if name.starts(t".") || name.starts(t"#") then name.skip(1) else name
        val joined: Text = inline.skip(1).join(t" ")
        val info: Optional[Text] = if joined == t"" then Unset else prose(joined)
        val base: Api.Entry = doc(tel, line, scope, prose)

        val entry: Api.Entry =
          val imports: List[Text] = scope + atoms(tel, t"import")
          val detail: Optional[Text] = base.detail.or(source.let(prose))
          base.copy(info = base.info.or(info), detail = detail, imports = imports)

        val nested: List[Entry] = entries(file, tel, placed(1), entry.imports, prose)
        List(Entry(plain, isType, entry, nested, line))

  // Every overlay merged onto the model, with the diagnostics the merging raises.
  def merge(api: Api, overlays: List[Overlay]): (Api, List[Block]) =
    @scala.caps.unsafe.untrackedCaptures
    var current: Api = api

    @scala.caps.unsafe.untrackedCaptures
    var problems: List[Block] = Nil

    def report(tone: Tone, path: Text, line: Int, message: Text, variant: Int): Unit =
      val location: Block.Location = Block.Location(path, line)
      val paragraphs: List[Block] = List(Block.paragraph(message))
      problems = Block.Diagnostic(tone, location, paragraphs, code = bolt(variant)) :: problems

    // What an overlay says of an entity, over what the entity says of itself (a schema's
    // `description`): the overlay's `info` and `detail`, each where it has one.
    def over(said: Api.Entry, own: Optional[Api.Entry]): Api.Entry =
      own.lay(said): (own: Api.Entry) =>
        said.copy(info = said.info.or(own.info), detail = said.detail.or(own.detail))

    def place(overlay: Overlay, parent: Api.Entity, entry: Entry): Unit =
      val designator: Locator = current.normal(if entry.isType then parent.tpe else parent.term)
      val key: Text = Api.key(designator, entry.name)
      val written: Text = t"${if entry.isType then t"#" else t"."}${entry.name}"

      current(key) match
        case existing: Api.Entity =>
          entry.doc.until.let: (until: Api.Until) =>
            val message: Text =
              t"`$written` is marked as removed in ${until.version}, but it still exists."

            report(Tone.Warning, overlay.file, entry.line, message, 10)

          val doc: Api.Entry = over(entry.doc.copy(file = overlay.file), existing.doc)
          current = current.add(existing.copy(doc = doc))
          entry.entries.each: (child: Entry) => place(overlay, existing.copy(doc = doc), child)

        case _ =>
          entry.doc.until match
            case until: Api.Until =>
              val doc: Api.Entry = entry.doc.copy(file = overlay.file)

              val removed: Api.Entity =
                Api.Entity(designator, entry.name, own = true, doc = doc, removed = true)

              current = current.add(removed)

              current = current.update(parent.key): (p: Api.Entity) =>
                p.copy(members = (p.members + List(key)).distinct)

              entry.entries.each: (child: Entry) => place(overlay, removed, child)

            case _ =>
              val message: Text =
                t"`$written` is documented here, but no such member of `${parent.key}` is declared."

              report(Tone.Failure, overlay.file, entry.line, message, 11)

    overlays.each: (overlay: Overlay) =>
      overlay.problems.each: (problem: Block) => problems = problem :: problems
      val base: Locator = current.normal(Locator(overlay.base))

      current(Api.key(base)) match
        case entity: Api.Entity =>
          val documented: Api.Entity =
            entity.copy(doc = over(overlay.doc.copy(file = overlay.file), entity.doc))
          current = current.add(documented)
          overlay.entries.each: (entry: Entry) => place(overlay, documented, entry)

        case _ =>
          val noun: Text = current.format.noun

          val message: Text =
            t"The $noun `${overlay.base}` this overlay documents is not declared by the sources."

          report(Tone.Failure, overlay.file, 1, message, 12)

    // A public member of a documented package, declared by the sources, with no entry, and
    // under no hidden entity: a caution where it is declared, naming the entry to write.
    val bases: List[Text] = overlays.map(_.base)

    def within(entity: Api.Entity): Boolean =
      bases.exists: (base: Text) =>
        entity.key == base || entity.key.starts(t"$base.") || entity.key.starts(t"$base#")

    def concealed(entity: Api.Entity): Boolean =
      entity.hidden || entity.parent.let(Api.key(_)).let(current(_)).let(concealed).or(false)

    // A member which only overrides is documented by what it overrides; a parameter need not be
    // documented at all.
    def overriding(entity: Api.Entity): Boolean =
      !entity.declarations.nil && entity.declarations.all: (d: Api.Declaration) =>
        d.overrides || d.modifiers.has(t"override")

    current.entities.values.order(_.key).each: (entity: Api.Entity) =>
      val undocumented: Boolean = entity.own && !entity.removed && entity.doc.absent
      val member: Boolean = current.format.documentable(entity)

      if undocumented && member && within(entity) && !concealed(entity) && !overriding(entity) then
        val overlay: Optional[Overlay] = overlays.seek: (o: Overlay) =>
          entity.key.starts(t"${o.base}.") || entity.key.starts(t"${o.base}#")

        val where: Text = entity.location.let(_.path).or(overlay.let(_.file).or(t""))
        val line: Int = entity.location.let(_.line).or(1)

        val written: Text = entity.parent.lay(entity.name): (parent: Locator) =>
          t"${parent.separator}${entity.name}"

        val parentKey: Text = entity.parent.let(Api.key(_)).or(t"")
        val file: Text = overlay.let(_.file).or(t"an overlay")

        val message: Text =
          t"`${entity.key}` is not documented: add `entry $written` under `$parentKey` in $file."

        report(Tone.Warning, where, line, message, 13)

    (current, problems.reverse)

// One overlay file: the package it documents, what it says of the package, its entries, and
// the failures of its format, which merging reports.
case class Overlay
  ( file:     Text,
    base:     Text,
    doc:      Api.Entry,
    entries:  List[Overlay.Entry],
    problems: List[Block]         = Nil,
    format:   Text                = t"scala" )
