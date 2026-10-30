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

import soundness.*

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Tone}
import stenography.Designator

// The documentation of an API, in Amok's `Flux` format: a TEL file per package (`base`), with
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
    List(t"base", t"import", t"info", t"detail", t"refer", t"hidden", t"until", t"entry")

  private val entryKeywords: List[Text] = fileKeywords.filter(_ != t"base")
  private val once: List[Text] = List(t"base", t"info", t"detail", t"hidden", t"until")

  // The lines `headed` adds before the file's own.
  private def headed(text: Text): Text =
    if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"

  private def added(text: Text): Int = if text.trim.starts(t"tel ") then 0 else 2

  // An overlay read from its text, or the diagnostic which says why it could not be.
  def read(path: Text, text: Text): Overlay | Block.Diagnostic =
    import strategies.throwUnsafely

    try
      val tel: Tel = headed(text).read[Tel]
      val lines: Lines = Lines(text, tel.childCompounds.to[List])

      val base: Optional[Text] =
        tel.fields(t"base").to[List].head.let(_.primaryAtom).let(_.puncture(t""))

      val missing: Block.Diagnostic =
        val message: Text = t"An overlay must name the package it documents with `base`."
        Block.Diagnostic(Tone.Failure, Block.Location(path, 1), List(Block.paragraph(message)))

      base.lay[Overlay | Block.Diagnostic](missing): (base: Text) =>
        val problems: List[Block] =
          check(path, lines, Unset, tel.childCompounds.to[List], fileKeywords)

        Overlay(path, base, doc(tel, 1, Nil), entries(tel, lines), problems)

    catch case error: Tel.Error =>
      val shift: Int = -added(text)
      val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))

      val location = raw.location.let: (location: Block.Location) =>
        location.copy(line = location.line.let(_ + shift))

      raw.copy(location = location)

  // The line of each compound of the file, since a node does not know its own: walking the
  // compounds and the lines together, in order, a compound is on the first line from where the
  // last one ended which begins with its keyword, and its source atom's lines follow it.
  private class Lines(text: Text, roots: List[Tel.Compound]):
    private val all: List[Text] = text.cut(t"\n")
    private val placed: java.util.IdentityHashMap[Tel.Compound, Int] = java.util.IdentityHashMap()

    walk(roots, 0)

    def apply(compound: Tel.Compound): Int = Optional(placed.get(compound)).let(_.nn.toInt).or(1)

    private def walk(compounds: List[Tel.Compound], cursor: Int): Int = compounds match
      case compound :: rest =>
        val index: Int = seek(compound.keyword.s, cursor)
        placed.put(compound, index + 1)

        val source: Int = sourceAtom(compound).lay(0): (text: Text) =>
          text.s.split("\n", -1).nn.length

        walk(rest, walk(childrenOf(compound), index + 1 + source))

      case _ => cursor

    private def seek(keyword: String, from: Int): Int =
      def begins(line: String): Boolean =
        val trimmed: String = line.trim.nn
        trimmed == keyword || trimmed.startsWith(keyword + " ") ||
          trimmed.startsWith(keyword + "\t")

      all.indexed
      . seek: (line: Text, index: Ordinal) => index.n0 >= from && begins(line.s)
      . let(_(1).n0)
      . or(from.min(all.size - 1).max(0))

  private def failure(path: Text, line: Int, message: Text): Block =
    Block.Diagnostic(Tone.Failure, Block.Location(path, line), List(Block.paragraph(message)))

  // The inline atoms of a compound, and its source atom.
  private def inlineAtoms(compound: Tel.Compound): List[Text] =
    List.from(compound.atoms.stdlib.collect { case Tel.Atom.Inline(text, _) => text }.toList)

  private def sourceAtom(compound: Tel.Compound): Optional[Text] =
    compound.atoms.stdlib.collectFirst { case Tel.Atom.Source(text) => text }.getOrElse(Unset)

  private def texts(compound: Tel.Compound): List[Text] =
    val all: scala.List[Text] = compound.atoms.stdlib.toList.collect:
      case Tel.Atom.Inline(text, _) => text
      case Tel.Atom.Source(text)    => text

    List.from(all)

  private def childrenOf(compound: Tel.Compound): List[Tel.Compound] =
    List.from(compound.children.stdlib.toList.flatMap(_.compounds.stdlib.toList))

  // The failures of the children of one node: the file (`entry` unset) or an entry.
  private def check
    ( path:     Text,
      lines:    Lines,
      entry:    Optional[Tel.Compound],
      children: List[Tel.Compound],
      allowed:  List[Text] )
  :   List[Block] =

    def written(keywords: List[Text]): Text = keywords.map { (k: Text) => t"`$k`" }.join(t", ")
    def named(keyword: Text): List[Tel.Compound] = children.filter(_.keyword == keyword)

    val each: List[Block] = children.bind: (child: Tel.Compound) =>
      val keyword: Text = child.keyword
      val grandchildren: List[Tel.Compound] = childrenOf(child)

      if !allowed.has(keyword) then
        val where: Text = if entry.absent then t"An overlay" else t"An entry"

        val message: Text =
          t"`$keyword` is not part of an overlay: $where may hold ${written(allowed)}."

        List(failure(path, lines(child), message))
      else if keyword == t"entry" then
        val unnamed: Text = t"An `entry` must name the member it documents, as `.name` or `#Name`."

        val name: List[Block] =
          if inlineAtoms(child).nil then List(failure(path, lines(child), unnamed)) else Nil

        name + check(path, lines, child, grandchildren, entryKeywords)
      else
        grandchildren.head.lay(Nil: List[Block]): (first: Tel.Compound) =>
          val message: Text = t"`$keyword` holds no `${first.keyword}`: it takes its value alone."
          List(failure(path, lines(first), message))

    val repeated: List[Block] = once.bind: (keyword: Text) =>
      named(keyword).skip(1).map: (child: Tel.Compound) =>
        failure(path, lines(child), t"`$keyword` is given more than once here.")

    // An entry's info may follow its name, and a leaf entry's detail may follow it as a source
    // atom, but not as well as a child which says the same.
    val doubled: List[Block] = entry.lay(Nil: List[Block]): (entry: Tel.Compound) =>
      // The first child of a keyword, as a failure, where it is given as well as on the line.
      def already(keyword: Text, twice: Boolean, message: Text): List[Block] =
        val found: List[Tel.Compound] = if twice then named(keyword).keep(1) else Nil
        found.map: (child: Tel.Compound) => failure(path, lines(child), message)

      val onLine: Text = t"This entry's info is already given on its line."
      val beneath: Text = t"This entry's detail is already given beneath its line."
      val info: List[Block] = already(t"info", inlineAtoms(entry).size > 1, onLine)
      val detail: List[Block] = already(t"detail", sourceAtom(entry).present, beneath)
      info + detail

    val duplicates: List[Block] =
      val entries: List[(Text, Tel.Compound)] = named(t"entry").bind: (child: Tel.Compound) =>
        inlineAtoms(child).head.lay(Nil: List[(Text, Tel.Compound)]): (name: Text) =>
          List((name, child))

      entries.indexed.bind: (pair: (Text, Tel.Compound), index: Ordinal) =>
        if !entries.keep(index.n0).exists(_(0) == pair(0)) then Nil
        else List(failure(path, lines(pair(1)), t"`${pair(0)}` already has an entry here."))

    // The info as it reads, which is what the limit is on, not the Markdown it is written in.
    val long: List[Block] =
      val infoChild: Optional[Tel.Compound] = named(t"info").head
      val inline: List[Text] = entry.lay(Nil: List[Text])(inlineAtoms(_).skip(1))

      val info: Optional[Text] =
        infoChild.let { (child: Tel.Compound) => texts(child).filter(_ != t"").join(t" ") }.or:
          if inline.nil then Unset else inline.join(t" ")

      val line: Int = infoChild.let(lines(_)).or(entry.let(lines(_)).or(1))

      info.lay(Nil: List[Block]): (info: Text) =>
        val size: Int = Blocks.plain(Blocks.inline(info)).length

        val message: Text =
          t"This info is ${size.show} characters long, and may be at most ${InfoLimit.show}: " +
            t"say the rest in a `detail`."

        if size <= InfoLimit then Nil else List(failure(path, line, message))

    each + repeated + doubled + duplicates + long

  // The text of a node's child by keyword: its atoms, inline then the indented source, joined.
  private def child(node: Tel, keyword: Text): Optional[Text] =
    node.fields(keyword).to[List].head
    . let(_.atomTexts.to[List].filter(_ != t"").join(t" "))
    . let(_.puncture(t""))

  private def atoms(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].bind(_.atomTexts.to[List].filter(_ != t""))

  private def flag(node: Tel, keyword: Text): Boolean = !node.fields(keyword).to[List].nil

  // What a node says of its entity, with the imports of those enclosing it.
  private def doc(node: Tel, line: Int, inherited: List[Text]): Api.Entry =
    val until: Optional[Api.Until] =
      node.fields(t"until").to[List].head.let: (until: Tel) =>
        val texts: List[Text] = until.atomTexts.to[List].filter(_ != t"")
        texts.head.let: (version: Text) => Api.Until(version, texts.at(Sec))

    Api.Entry
      ( info    = child(node, t"info"),
        detail  = child(node, t"detail"),
        hidden  = flag(node, t"hidden"),
        until   = until,
        refer   = atoms(node, t"refer"),
        imports = inherited + atoms(node, t"import"),
        line    = line )

  private def entries(node: Tel, lines: Lines): List[Entry] = entries(node, lines, Nil)

  private def entries(node: Tel, lines: Lines, inherited: List[Text]): List[Entry] =
    val scope: List[Text] = inherited + atoms(node, t"import")

    // A node, and the compound it wraps (whose atoms say which is inline and which the source),
    // paired by order: `fields` and `childCompounds` walk the same children.
    val nodes: List[Tel] = node.fields(t"entry").to[List]
    val compounds: List[Tel.Compound] = node.childCompounds.to[List].filter(_.keyword == t"entry")

    nodes.zip(compounds).bind: (tel: Tel, compound: Tel.Compound) =>
      // The name is the first inline atom; the info the rest; the detail the source atom.
      val inline: List[Text] = inlineAtoms(compound)
      val source: Optional[Text] = sourceAtom(compound)

      inline.head.lay(Nil: List[Entry]): (name: Text) =>
        val line: Int = lines(compound)
        val isType: Boolean = name.starts(t"#")
        val plain: Text = if name.starts(t".") || name.starts(t"#") then name.skip(1) else name
        val joined: Text = inline.skip(1).join(t" ")
        val info: Optional[Text] = if joined == t"" then Unset else joined
        val base: Api.Entry = doc(tel, line, scope)

        val entry: Api.Entry =
          val imports: List[Text] = scope + atoms(tel, t"import")
          base.copy(info = base.info.or(info), detail = base.detail.or(source), imports = imports)

        List(Entry(plain, isType, entry, entries(tel, lines, entry.imports), line))

  // Every overlay merged onto the model, with the diagnostics the merging raises.
  def merge(api: Api, overlays: List[Overlay]): (Api, List[Block]) =
    @scala.caps.unsafe.untrackedCaptures
    var current: Api = api

    @scala.caps.unsafe.untrackedCaptures
    var problems: List[Block] = Nil

    def report(tone: Tone, path: Text, line: Int, message: Text): Unit =
      val location: Block.Location = Block.Location(path, line)
      problems = Block.Diagnostic(tone, location, List(Block.paragraph(message))) :: problems

    def place(overlay: Overlay, parent: Api.Entity, entry: Entry): Unit =
      val designator: Designator = if entry.isType then parent.tpe else parent.term
      val key: Text = Api.key(designator, entry.name)
      val written: Text = t"${if entry.isType then t"#" else t"."}${entry.name}"

      current(key) match
        case existing: Api.Entity =>
          entry.doc.until.let: (until: Api.Until) =>
            val message: Text =
              t"`$written` is marked as removed in ${until.version}, but it still exists."

            report(Tone.Warning, overlay.file, entry.line, message)

          val doc: Api.Entry = entry.doc.copy(file = overlay.file)
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

              report(Tone.Failure, overlay.file, entry.line, message)

    overlays.each: (overlay: Overlay) =>
      overlay.problems.each: (problem: Block) => problems = problem :: problems
      val base: Designator = Designator(overlay.base)

      current(Api.key(base)) match
        case entity: Api.Entity =>
          val documented: Api.Entity = entity.copy(doc = overlay.doc.copy(file = overlay.file))
          current = current.add(documented)
          overlay.entries.each: (entry: Entry) => place(overlay, documented, entry)

        case _ =>
          val message: Text =
            t"The package `${overlay.base}` this overlay documents is not declared by the sources."

          report(Tone.Failure, overlay.file, 1, message)

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

    current.entities.values.toList.sortBy(_.key.s).foreach: entity =>
      val undocumented: Boolean = entity.own && !entity.removed && entity.doc.absent
      val member: Boolean = !entity.isPackage && !entity.isParameter

      if undocumented && member && within(entity) && !concealed(entity) && !overriding(entity) then
        val overlay: Optional[Overlay] = overlays.seek: (o: Overlay) =>
          entity.key.starts(t"${o.base}.") || entity.key.starts(t"${o.base}#")

        val where: Text = entity.location.let(_.path).or(overlay.let(_.file).or(t""))
        val line: Int = entity.location.let(_.line).or(1)

        val written: Text = entity.parent.lay(entity.name):
          case Designator.Type(_, _) => t"#${entity.name}"
          case _                     => t".${entity.name}"

        val parentKey: Text = entity.parent.let(Api.key(_)).or(t"")
        val file: Text = overlay.let(_.file).or(t"an overlay")

        val message: Text =
          t"`${entity.key}` is not documented: add `entry $written` under `$parentKey` in $file."

        report(Tone.Warning, where, line, message)

    (current, problems.reverse)

// One overlay file: the package it documents, what it says of the package, its entries, and
// the failures of its format, which merging reports.
case class Overlay
  ( file:     Text,
    base:     Text,
    doc:      Api.Entry,
    entries:  List[Overlay.Entry],
    problems: List[Block]         = Nil )
