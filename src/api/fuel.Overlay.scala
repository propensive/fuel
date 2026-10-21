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

import scala.collection.immutable as sci

import soundness.{Language as _, Token as _, *}

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
// Merging an overlay onto the model is where documentation and code are held to each other: an
// entry naming a member nothing declares is a failure (unless it is marked `until`, when it
// has a page which says what replaced it), an `until` on a member which still exists is a
// caution, and a public member of a documented package with no entry is a caution at the
// place it is declared.
object Overlay:
  case class Entry(name: Text, isType: Boolean, doc: Api.Entry, entries: List[Entry], line: Int)

  // The lines `headed` adds before the file's own.
  private def headed(text: Text): Text = if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"
  private def added(text: Text): Int = if text.trim.starts(t"tel ") then 0 else 2

  // An overlay read from its text, or the diagnostic which says why it could not be.
  def read(path: Text, text: Text): Overlay | Block.Diagnostic =
    import strategies.throwUnsafely

    try
      val tel: Tel = headed(text).read[Tel]
      val lines: Lines = Lines(text)

      val base: Optional[Text] = tel.fields(t"base").to[List].prim.let(_.primaryAtom).let { (b: Text) => if b == t"" then Unset else b }

      base.lay[Overlay | Block.Diagnostic](Block.Diagnostic(Tone.Failure, Block.Location(path, 1), List(Block.paragraph(t"An overlay must name the package it documents with `base`.")))):
        (base: Text) =>
          Overlay(path, base, doc(tel, lines, 1, Nil), entries(tel, lines))

    catch case error: Tel.Error =>
      val shift: Int = -added(text)
      val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))
      val location = raw.location.let { (location: Block.Location) => location.copy(line = location.line.let(_ + shift)) }
      raw.copy(location = location)

  // The lines of the file, by which an entry is placed: the n-th line beginning `entry <name>`
  // is the n-th such entry walked, since a node does not know its own line.
  private class Lines(text: Text):
    private val all: List[Text] = text.cut(t"\n")
    private val seen: juc.ConcurrentHashMap[Text, Int] = juc.ConcurrentHashMap()

    def entry(name: Text): Int =
      val occurrence: Int = seen.merge(name, 1, (a, b) => a.nn + b.nn).nn
      val matching: List[Int] = all.indexed.filter { (line: Text, index: Ordinal) => entryLine(line, name) }.map(_(1).n0 + 1)
      matching.skip(occurrence - 1).prim.or(matching.skip(matching.size - 1).prim.or(1))

    private def entryLine(line: Text, name: Text): Boolean =
      val trimmed: String = line.s.trim.nn
      trimmed.startsWith("entry ") && {
        val rest: String = trimmed.substring(6).nn.trim.nn
        rest == name.s || rest.startsWith(name.s + " ") || rest.startsWith(name.s + "\t")
      }

  // The text of a node's child by keyword: its atoms, inline then the indented source, joined.
  private def child(node: Tel, keyword: Text): Optional[Text] =
    node.fields(keyword).to[List].prim.let(_.atomTexts.to[List].filter(_ != t"").join(t" ")).let { (t: Text) => if t == t"" then Unset else t }

  private def atoms(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].bind(_.atomTexts.to[List].filter(_ != t""))

  private def flag(node: Tel, keyword: Text): Boolean = !node.fields(keyword).to[List].nil

  // What a node says of its entity, with the imports of those enclosing it.
  private def doc(node: Tel, lines: Lines, line: Int, inherited: List[Text]): Api.Entry =
    val until: Optional[Api.Until] =
      node.fields(t"until").to[List].prim.let: (until: Tel) =>
        val texts: List[Text] = until.atomTexts.to[List].filter(_ != t"")
        texts.prim.let { (version: Text) => Api.Until(version, texts.at(Sec)) }

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
      val inline: List[Text] = List.from(compound.atoms.stdlib.collect { case Tel.Atom.Inline(text, _) => text }.toList)
      val source: Optional[Text] = compound.atoms.stdlib.collectFirst { case Tel.Atom.Source(text) => text }.getOrElse(Unset)

      inline.prim.lay(Nil: List[Entry]): (name: Text) =>
        val line: Int = lines.entry(name)
        val isType: Boolean = name.starts(t"#")
        val plain: Text = if name.starts(t".") || name.starts(t"#") then name.skip(1) else name
        val joined: Text = inline.skip(1).join(t" ")
        val info: Optional[Text] = if joined == t"" then Unset else joined
        val base: Api.Entry = doc(tel, lines, line, scope)

        val entry: Api.Entry =
          base.copy(info = base.info.or(info), detail = base.detail.or(source), imports = scope + atoms(tel, t"import"))

        List(Entry(plain, isType, entry, entries(tel, lines, entry.imports), line))

  // Every overlay merged onto the model, with the diagnostics the merging raises.
  def merge(api: Api, overlays: List[Overlay]): (Api, List[Block]) =
    @scala.caps.unsafe.untrackedCaptures
    var current: Api = api

    @scala.caps.unsafe.untrackedCaptures
    var problems: List[Block] = Nil

    def report(tone: Tone, path: Text, line: Int, message: Text): Unit =
      problems = Block.Diagnostic(tone, Block.Location(path, line), List(Block.paragraph(message))) :: problems

    def place(overlay: Overlay, parent: Api.Entity, entry: Entry): Unit =
      val designator: Designator = if entry.isType then parent.tpe else parent.term
      val key: Text = Api.key(designator, entry.name)
      val written: Text = t"${if entry.isType then t"#" else t"."}${entry.name}"

      current(key) match
        case existing: Api.Entity =>
          entry.doc.until.let: (until: Api.Until) =>
            report(Tone.Warning, overlay.file, entry.line, t"`$written` is marked as removed in ${until.version}, but it still exists.")

          val doc: Api.Entry = entry.doc.copy(file = overlay.file)
          current = current.add(existing.copy(doc = doc))
          entry.entries.each { (child: Entry) => place(overlay, existing.copy(doc = doc), child) }

        case _ =>
          entry.doc.until match
            case until: Api.Until =>
              val removed: Api.Entity =
                Api.Entity(designator, entry.name, own = true, doc = entry.doc.copy(file = overlay.file), removed = true)

              current = current.add(removed)
              current = current.update(parent.key) { (p: Api.Entity) => p.copy(members = (p.members + List(key)).distinct) }
              entry.entries.each { (child: Entry) => place(overlay, removed, child) }

            case _ =>
              report(Tone.Failure, overlay.file, entry.line, t"`$written` is documented here, but no such member of `${parent.key}` is declared.")

    overlays.each: (overlay: Overlay) =>
      val base: Designator = Designator(overlay.base)

      current(Api.key(base)) match
        case entity: Api.Entity =>
          val documented: Api.Entity = entity.copy(doc = overlay.doc.copy(file = overlay.file))
          current = current.add(documented)
          overlay.entries.each { (entry: Entry) => place(overlay, documented, entry) }

        case _ =>
          report(Tone.Failure, overlay.file, 1, t"The package `${overlay.base}` this overlay documents is not declared by the sources.")

    // A public member of a documented package, declared by the sources, with no entry, and
    // under no hidden entity: a caution where it is declared, naming the entry to write.
    val bases: List[Text] = overlays.map(_.base)

    def within(entity: Api.Entity): Boolean =
      bases.exists { (base: Text) => entity.key == base || entity.key.starts(t"$base.") || entity.key.starts(t"$base#") }

    def concealed(entity: Api.Entity): Boolean =
      entity.hidden || entity.parent.let { (parent: Designator) => current(Api.key(parent)).let(concealed).or(false) }.or(false)

    // A member which only overrides is documented by what it overrides.
    def overriding(entity: Api.Entity): Boolean =
      !entity.declarations.nil && entity.declarations.all { (d: Api.Declaration) => d.overrides || d.modifiers.has(t"override") }

    current.entities.values.toList.sortBy(_.key.s).foreach: entity =>
      if entity.own && !entity.removed && !entity.isPackage && entity.doc.absent && within(entity) && !concealed(entity) && !overriding(entity) then
        val overlay: Optional[Overlay] = overlays.seek { (o: Overlay) => entity.key.starts(t"${o.base}.") || entity.key.starts(t"${o.base}#") }
        val where: Text = entity.location.let(_.path).or(overlay.let(_.file).or(t""))
        val line: Int = entity.location.let(_.line).or(1)
        val written: Text = entity.parent.let { (p: Designator) => if p.isInstanceOf[Designator.Type] then t"#${entity.name}" else t".${entity.name}" }.or(entity.name)
        val parentKey: Text = entity.parent.let(Api.key(_)).or(t"")
        report(Tone.Warning, where, line, t"`${entity.key}` is not documented: add `entry $written` under `$parentKey` in ${overlay.let(_.file).or(t"an overlay")}.")

    (current, problems.reverse)

// One overlay file: the package it documents, what it says of the package, and its entries.
case class Overlay(file: Text, base: Text, doc: Api.Entry, entries: List[Overlay.Entry])
