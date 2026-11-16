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

import soundness.sortingAlgorithms.timsort

import codepages.utf8Codepage
import dysasymptotics.linearSize
import pyrocosm.{Block, Tone}

// The translation of an overlay: an overlay of the same entries, which TEL reads as it reads
// any other, whose prose — each `info` and `detail` — is a `Translation` of the definitive
// overlay's, written as the source atom of its keyword:
//
//   entry .Text
//     info
//         - a one-line summary
//         + un résumé d'une ligne
//     detail
//         - Markdown about `Text`.
//         + Du Markdown sur `Text`.
//
// An `info` the definitive overlay gives on an entry's line is an `info` beneath the entry
// here, since a translation of it is two lines at least. Everything else — the entries, and
// each one's `hidden`, `until`, `refer` and `import` — is the definitive overlay's, and is the
// same in every language. A translation is made from the definitive overlay (`generate`), and
// made again from it when it changes, each run of prose brought up to date as
// `Translation.update` does it; `check` holds one to the other.
object OverlayTranslations:
  // The code of what is wrong with an overlay's translation, `↯/f.26.5.<variant>`: prose which
  // is not translated (1); prose whose text has gone (2); an entry it lacks (3); an entry whose
  // `hidden`, `until`, `refer` or `import` is not its original's (4); an entry the overlay it
  // translates lacks (5); and a translation of another package (6).
  private def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.api, FuelBolts.overlayTranslation, variant)

  // A run of prose of an overlay: the entry it is of, by the names which lead to it from the
  // file (none, for the file's own), each with its sigil; whether it is its `info` or its
  // `detail`; its text as written; the line of the file (from 1) its text begins on; the line
  // of the compound which holds it, whether that is the entry itself rather than an `info` or
  // a `detail`, and whether the text is on that line rather than beneath it.
  case class Prose
    ( entry:   List[Text],
      keyword: Text,
      text:    Text,
      line:    Int,
      site:    Int,
      own:     Boolean,
      inline:  Boolean ):

    def key: (List[Text], Text) = (entry, keyword)

    // How it is named to a reader: `the info of .Text#length`.
    def title: Text =
      if entry.nil then t"the overlay's `$keyword`" else t"the `$keyword` of `${entry.join}`"

  private def sigilled(name: Text): Text =
    if name.starts(t".") || name.starts(t"#") then name else t".$name"

  // Every run of prose of an overlay, in the order of the file; none if it is not TEL.
  def prose(text: Text): List[Prose] =
    import parsing.trackPositions

    safely(text.read[Tel]).lay(List[Prose]()): (tel: Tel) =>
      def walk(compounds: List[(Tel.Compound, List[Text])], path: List[Text]): List[Prose] =
        compounds.bind: (compound: Tel.Compound, at: List[Text]) =>
          val keyword: Text = compound.keyword
          val line: Int = Compounds.line(tel, at)
          val inline: List[Text] = Compounds.inline(compound)
          val source: Optional[Text] = Compounds.source(compound)

          if keyword == t"info" || keyword == t"detail" then
            val run: List[Prose] =
              if inline.nil then Nil
              else List(Prose(path, keyword, inline.join(t" "), line, line, true, true))

            source.lay(run): (text: Text) =>
              List(Prose(path, keyword, text, line + 1, line, true, false))
          else if keyword == t"entry" then
            inline.head.lay(List[Prose]()): (name: Text) =>
              val here: List[Text] = path + List(sigilled(name))

              val info: List[Prose] =
                if inline.size < 2 then Nil
                else List(Prose(here, t"info", inline.skip(1).join(t" "), line, line, false, true))

              val detail: List[Prose] = source.lay(List[Prose]()): (text: Text) =>
                List(Prose(here, t"detail", text, line + 1, line, false, false))

              info + detail + walk(Compounds.paths(at, Compounds.children(compound)), here)
          else Nil

      walk(Compounds.paths(Nil, tel.childCompounds.to[List]), Nil)

  // The translation of a definitive overlay: the overlay as TEL reads it, each run of its prose
  // a `Translation` of it — that of the translation `old`, brought up to date, where it has the
  // run, and otherwise one nobody has translated — written as the source atom of its keyword,
  // and the rest as the definitive overlay has it. An `info` on an entry's line moves beneath
  // the entry, and the entry's detail with it, beneath `detail`, since nothing may follow a
  // source atom in its compound; a detail alone stays the entry's source atom. `Unset` if the
  // overlay is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    val carried: Map[(List[Text], Text), Text] =
      old.lay(List[Prose]())(prose(_)).map { (run: Prose) => run.key -> run.text }.to[Map]

    def translated(entry: List[Text], keyword: Text, text: Text): Tel.Atom =
      val translation: Translation =
        carried.at((entry, keyword)).lay(Translation.skeleton(text)): (before: Text) =>
          Translation.update(Translation.parse(before), text)

      Tel.Atom.Source(translation.serialize)

    def detail(entry: List[Text], text: Text): List[Tel.Atom] =
      List(translated(entry, t"detail", text))

    def beneath(keyword: Text, atom: Tel.Atom): Tel.Compound =
      Tel.Compound(keyword, Array(atom), Unset, Array.empty)

    // A compound as the translation writes it, within the entry `path`.
    def written(path: List[Text])(compound: Tel.Compound): Tel.Compound =
      val inline: List[Text] = Compounds.inline(compound)
      val source: Optional[Text] = Compounds.source(compound)

      compound.keyword match
        case t"info" | t"detail" =>
          val text: Optional[Text] = source.or(if inline.nil then Unset else inline.join(t" "))

          text.lay(compound): (text: Text) =>
            compound.copy(atoms = Array(translated(path, compound.keyword, text)))

        case t"entry" =>
          inline.head.lay(compound): (name: Text) =>
            val here: List[Text] = path + List(sigilled(name))
            val info: Optional[Text] = if inline.size < 2 then Unset else inline.skip(1).join(t" ")

            val named: List[Tel.Atom] =
              compound.atoms.to[List].filter:
                case Tel.Atom.Inline(_, _) => true
                case _                     => false

              . keep(1)

            val children: Array[Tel.Block] = compound.children.map: (block: Tel.Block) =>
              block.copy(compounds = block.compounds.map(written(here)))

            info.lay
              ( compound.copy
                  ( atoms    = (named + source.lay(List[Tel.Atom]())(detail(here, _))).to[Array],
                    children = children ) ):
              (info: Text) =>
                val moved: List[Tel.Compound] =
                  List(beneath(t"info", translated(here, t"info", info)))
                  + source.lay(List[Tel.Compound]()): (detail: Text) =>
                      List(beneath(t"detail", translated(here, t"detail", detail)))

                val block: Tel.Block = Tel.Block(Array.empty, Unset, moved.to[Array], 0)

                val all: List[Tel.Block] = List(block) + children.to[List]
                compound.copy(atoms = named.to[Array], children = all.to[Array])

        case _ =>
          compound

    safely:
      val tel: Tel = definitive.read[Tel]

      val replacements: List[Tel.Mutation.Op] =
        tel.childCompounds.to[List].indexed.map: (compound: Tel.Compound, index: Ordinal) =>
          val occurrence: Int =
            tel.childCompounds.to[List].keep(index.n0).count(_.keyword == compound.keyword)

          val pointer: Tel.Pointer = Tel.Pointer.Empty / (compound.keyword, occurrence)
          Tel.Mutation.Op.Replace(pointer, written(Nil)(compound))

      Tel.Mutation(tel, replacements).show

  // The runs of a translation which someone has translated and the definitive overlay no
  // longer has: what making the translation again leaves out.
  def lost(definitive: Text, old: Text): List[Prose] =
    val kept: List[(List[Text], Text)] = prose(definitive).map(_.key)

    prose(old).filter: (run: Prose) =>
      !kept.has(run.key) && !Translation.parse(run.text).passages.filter(!_.foreign.nil).nil

  // An overlay's prose as its translation reads: what `Overlay.read` is given to read a
  // translation as the overlay it is in its own language.
  def foreign(text: Text): Text = Translation.parse(text).foreign.text

  // What is wrong with the translation of an overlay, read from `text` at `path`, of the
  // definitive overlay `definitive`: what is wrong with it as an overlay, as its translation
  // reads; each run of prose which is not a translation of the definitive overlay's, as
  // `Translation` and `Mirror` find; a run or an entry which one has and the other lacks; and
  // an entry which says other than the one it translates of what is the same in every language.
  def check(path: Text, text: Text, definitive: Text): List[Block] =
    Overlay.read(path, text, foreign) match
      case diagnostic: Block.Diagnostic =>
        List(diagnostic)

      case overlay: Overlay =>
        val theirs: List[Prose] = prose(text)
        val ours: List[Prose] = prose(definitive)
        val remedy: Text = t"`fuel translate --update` makes the translation again"

        type Path = List[Text]

        def paths(entries: List[Overlay.Entry], prefix: Path): List[(Path, Overlay.Entry)] =
          entries.bind: (entry: Overlay.Entry) =>
            val here: Path = prefix + List(t"${if entry.isType then t"#" else t"."}${entry.name}")
            List((here, entry)) + paths(entry.entries, here)

        // Each entry by its path, a later one of a path over an earlier; and the entries so
        // kept, in the order of their lines.
        def byPath(entries: List[Overlay.Entry]): Map[Path, Overlay.Entry] =
          paths(entries, Nil).to[Map]

        def inOrder(entries: Map[Path, Overlay.Entry]): List[(Path, Overlay.Entry)] =
          entries.keys.to[List].bind: (path: Path) =>
            entries.at(path).lay(List[(Path, Overlay.Entry)]()) { entry => List((path, entry)) }

          . order(_(1).line)

        val translated: Map[Path, Overlay.Entry] = byPath(overlay.entries)

        val original: Optional[Overlay] = Overlay.read(path, definitive) match
          case overlay: Overlay => overlay
          case _                => Unset

        val defined: Map[Path, Overlay.Entry] =
          original.lay(Map[Path, Overlay.Entry]()) { (overlay: Overlay) => byPath(overlay.entries) }

        def written(path: Path): Text = path.join

        val runs: List[Translation.Finding] = ours.bind: (run: Prose) =>
          theirs.seek(_.key == run.key) match
            case found: Prose =>
              (Translation.findings(found.text, run.text) + Mirror.findings(found.text))
              . map(_.shifted(found.line - 1))

            case _ =>
              val entry: Path = run.entry

              if !entry.nil && translated.at(entry).absent then Nil else
                val line: Int = translated.at(entry).lay(1)(_.line)
                val message: Text = t"This lacks ${run.title}, which is not translated: $remedy."
                List(Translation.Finding(Tone.Warning, line, message, bolt(1)))

        val gone: List[Translation.Finding] = theirs.bind: (run: Prose) =>
          val entry: Path = run.entry

          if ours.exists(_.key == run.key) || (!entry.nil && defined.at(entry).absent) then Nil
          else
            val message: Text = t"The text this translates has gone: $remedy."
            List(Translation.Finding(Tone.Warning, run.line, message, bolt(2)))

        val entries: List[Translation.Finding] =
          if original.absent then Nil else
            val absent: List[Translation.Finding] =
              inOrder(defined).bind: (path: Path, entry: Overlay.Entry) =>
                if translated.at(path).present then Nil else
                  val line: Int = translated.at(path.skip(1, Rtl)).lay(1)(_.line)
                  val message: Text = t"This has no entry for `${written(path)}`: $remedy."
                  List(Translation.Finding(Tone.Warning, line, message, bolt(3)))

            val extra: List[Translation.Finding] =
              inOrder(translated).bind: (path: Path, entry: Overlay.Entry) =>
                defined.at(path) match
                  case other: Overlay.Entry =>
                    val same: Boolean =
                      entry.doc.hidden == other.doc.hidden && entry.doc.until == other.doc.until &&
                        entry.doc.refer == other.doc.refer && entry.doc.imports == other.doc.imports

                    if same then Nil else
                      val message: Text =
                        t"This entry's `hidden`, `until`, `refer` and `import` are not those of " +
                          t"the entry it translates, and they are the same in every language: " +
                          t"$remedy."

                      List(Translation.Finding(Tone.Failure, entry.line, message, bolt(4)))

                  case _ =>
                    val message: Text =
                      t"The overlay this translates has no entry for `${written(path)}`: $remedy."

                    List(Translation.Finding(Tone.Warning, entry.line, message, bolt(5)))

            val heading: List[Translation.Finding] =
              original.lay(List[Translation.Finding]()): (other: Overlay) =>
                if other.base == overlay.base && other.format == overlay.format then Nil else
                  val message: Text =
                    t"This is of `${overlay.base}`, and the overlay it translates of " +
                      t"`${other.base}`."

                  List(Translation.Finding(Tone.Failure, 1, message, bolt(6)))

            heading + absent + extra

        val diagnostics: List[Block.Diagnostic] =
          Translation.report(path, text, runs + gone + entries)

        overlay.problems + diagnostics.map { (diagnostic: Block.Diagnostic) => diagnostic: Block }

  // How much of an overlay's translation is done: the sum over its runs of prose, a run the
  // translation lacks being one more change to make.
  def progress(text: Text, definitive: Text): Translation.Progress =
    val theirs: List[Prose] = prose(text)

    prose(definitive).fold(Translation.Progress(0, 0, 0, 0, 0)):
      (total: Translation.Progress, run: Prose) =>
        theirs.seek(_.key == run.key).lay(total.copy(stale = total.stale + 1)):
          (found: Prose) =>
            val progress: Translation.Progress =
              Translation.progress(Translation.parse(found.text), run.text)

            Translation.Progress
              ( total.translated + progress.translated, total.untranslated + progress.untranslated,
                total.pending + progress.pending, total.orphaned + progress.orphaned,
                total.stale + progress.stale )

  // An overlay at `path` held to its translations, each a language and a path, as
  // `Translations.check` holds a document.
  def verify
    ( path:         Text,
      text:         Text,
      translations: List[(Text, Text)],
      read:         Text -> Optional[Text],
      partial:      List[Text] = Nil )
  :   List[Block] =

    translations.bind: (tongue: Text, target: Text) =>
      val missing: List[Block] =
        if partial.has(tongue) then Nil else List(Translations.absent(path, tongue, target))

      read(target).lay(missing): (source: Text) =>
        check(target, source, text)
