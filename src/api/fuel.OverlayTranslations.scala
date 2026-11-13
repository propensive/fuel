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

import scala.collection.immutable as sci

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
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

  private def parsed(text: Text): Optional[(Tel, Overlay.Lines)] =
    import strategies.throwUnsafely

    try
      val tel: Tel = Overlay.headed(text).read[Tel]
      (tel, Overlay.Lines(text, tel.childCompounds.to[List]))
    catch case error: Tel.Error => Unset

  // Every run of prose of an overlay, in the order of the file; none if it is not TEL.
  def prose(text: Text): List[Prose] =
    parsed(text).lay(List[Prose]()): (tel: Tel, lines: Overlay.Lines) =>
      def walk(compounds: sci.List[Tel.Compound], path: sci.List[Text]): sci.List[Prose] =
        compounds.flatMap: (compound: Tel.Compound) =>
          val keyword: Text = compound.keyword
          val line: Int = lines(compound)
          val inline: sci.List[Text] = Overlay.inlineAtoms(compound).stdlib
          val source: Optional[Text] = Overlay.sourceAtom(compound)
          val entry: List[Text] = List.from(path)

          if keyword == t"info" || keyword == t"detail" then
            val joined: Text = Text(inline.map(_.s).mkString(" "))

            source.lay
              ( if inline.isEmpty then sci.Nil
                else sci.List(Prose(entry, keyword, joined, line, line, true, true)) ):
              (text: Text) => sci.List(Prose(entry, keyword, text, line + 1, line, true, false))
          else if keyword == t"entry" then
            inline.headOption.toList.flatMap: (name: Text) =>
              val here: sci.List[Text] = path :+ sigilled(name)
              val entry: List[Text] = List.from(here)

              val info: sci.List[Prose] =
                if inline.size < 2 then sci.Nil
                else
                  val text: Text = Text(inline.drop(1).map(_.s).mkString(" "))
                  sci.List(Prose(entry, t"info", text, line, line, false, true))

              val detail: sci.List[Prose] = source.lay(sci.List[Prose]()): (text: Text) =>
                sci.List(Prose(entry, t"detail", text, line + 1, line, false, false))

              info ++ detail ++ walk(Overlay.childrenOf(compound).stdlib, here)
          else sci.Nil

      List.from(walk(tel.childCompounds.to[List].stdlib, sci.Nil))

  private def indent(line: String): Int = line.takeWhile(_ == ' ').length
  private def spaces(count: Int): String = " ".repeat(count).nn

  private def set(lines: sci.Vector[String], indent: Int): sci.Vector[String] =
    lines.map { (line: String) => if line == "" then "" else spaces(indent) + line }

  // The translation of a definitive overlay: each run of its prose a `Translation` of it —
  // that of the translation `old`, brought up to date, where it has the run, and otherwise one
  // nobody has translated — and the rest as the definitive overlay has it. `Unset` if the
  // overlay is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    if parsed(definitive).absent then Unset else
      val lines: sci.Vector[String] = Translation.split(definitive)
      val runs: sci.List[Prose] = prose(definitive).stdlib

      val carried: sci.Map[(List[Text], Text), Text] =
        sci.Map.from:
          old.lay(sci.List[Prose]())(prose(_).stdlib).map { (run: Prose) => run.key -> run.text }

      def marked(run: Prose): sci.Vector[String] =
        val translation: Translation = carried.get(run.key) match
          case Some(text) => Translation.update(Translation.parse(text), run.text)
          case None       => Translation.skeleton(run.text)

        Translation.split(translation.serialize)

      // What stands for each line (from zero) which does not stand for itself.
      val replaced: sci.Map[Int, sci.Vector[String]] =
        val written: sci.List[(Int, sci.Vector[String])] =
          runs.groupBy(_.site).toList.flatMap: (site: Int, runs: sci.List[Prose]) =>
            val head: String = lines.lift(site - 1).getOrElse("")
            val depth: Int = indent(head)
            val pad: String = spaces(depth)

            def beneath(run: Prose, first: sci.Vector[String], last: sci.Vector[String])
            :   sci.List[(Int, sci.Vector[String])] =

              val count: Int = Translation.split(run.text).size
              val body: sci.Vector[String] = first ++ set(marked(run), depth + 4) ++ last

              (site -> body) :: (1 until count).toList.map: (offset: Int) =>
                (site + offset) -> sci.Vector[String]()

            runs.find(_.own) match
              case Some(run) =>
                if !run.inline then beneath(run, sci.Vector(), sci.Vector())
                else
                  sci.List
                    ( (site - 1) ->
                        (sci.Vector(s"$pad${run.keyword.s}") ++ set(marked(run), depth + 4)) )

              case None =>
                val info: Option[Prose] = runs.find(_.keyword == t"info")
                val detail: Option[Prose] = runs.find(_.keyword == t"detail")
                val name: String = runs.head.entry.stdlib.lastOption.map(_.s).getOrElse("")

                val block: sci.Vector[String] = info.toVector.flatMap: (run: Prose) =>
                  sci.Vector(s"$pad  info") ++ set(marked(run), depth + 6)

                val entry: sci.Vector[String] =
                  if info.isEmpty then sci.Vector(head) else sci.Vector(s"${pad}entry $name")

                detail match
                  case Some(run) => ((site - 1) -> entry) :: beneath(run, sci.Vector(), block)
                  case None      => sci.List((site - 1) -> (entry ++ block))

        sci.Map.from(written)

      val result: sci.Vector[String] = lines.zipWithIndex.flatMap: (line: String, index: Int) =>
        replaced.getOrElse(index, sci.Vector(line))

      Text(result.mkString("\n"))

  // The runs of a translation which someone has translated and the definitive overlay no
  // longer has: what making the translation again leaves out.
  def lost(definitive: Text, old: Text): List[Prose] =
    val kept: sci.Set[(List[Text], Text)] = prose(definitive).stdlib.map(_.key).toSet

    prose(old).filter: (run: Prose) =>
      !kept.contains(run.key) && !Translation.parse(run.text).passages.filter(!_.foreign.nil).nil

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
        val theirs: sci.List[Prose] = prose(text).stdlib
        val ours: sci.List[Prose] = prose(definitive).stdlib
        val remedy: Text = t"`fuel translate --update` makes the translation again"

        type Path = sci.List[Text]

        def paths(entries: List[Overlay.Entry], prefix: Path): sci.Map[Path, Overlay.Entry] =
          sci.Map.from:
            entries.stdlib.flatMap: (entry: Overlay.Entry) =>
              val here: Path = prefix :+ t"${if entry.isType then t"#" else t"."}${entry.name}"
              (here -> entry) :: paths(entry.entries, here).toList

        val translated: sci.Map[Path, Overlay.Entry] = paths(overlay.entries, sci.Nil)

        val original: Optional[Overlay] = Overlay.read(path, definitive) match
          case overlay: Overlay => overlay
          case _                => Unset

        val defined: sci.Map[Path, Overlay.Entry] =
          original.lay(sci.Map[Path, Overlay.Entry]()): (overlay: Overlay) =>
            paths(overlay.entries, sci.Nil)

        def written(path: Path): Text = Text(path.map(_.s).mkString)

        val runs: sci.List[Translation.Finding] = ours.flatMap: (run: Prose) =>
          theirs.find(_.key == run.key) match
            case Some(found) =>
              (Translation.findings(found.text, run.text) + Mirror.findings(found.text)).stdlib
              . map(_.shifted(found.line - 1))

            case None =>
              val entry: Path = run.entry.stdlib

              if !entry.isEmpty && !translated.contains(entry) then sci.Nil else
                val line: Int = translated.get(entry).map(_.line).getOrElse(1)
                val message: Text = t"This lacks ${run.title}, which is not translated: $remedy."
                sci.List(Translation.Finding(Tone.Warning, line, message))

        val gone: sci.List[Translation.Finding] = theirs.flatMap: (run: Prose) =>
          val entry: Path = run.entry.stdlib

          if ours.exists(_.key == run.key) || (!entry.isEmpty && !defined.contains(entry))
          then sci.Nil
          else
            val message: Text = t"The text this translates has gone: $remedy."
            sci.List(Translation.Finding(Tone.Warning, run.line, message))

        val entries: sci.List[Translation.Finding] =
          if original.absent then sci.Nil else
            val absent: sci.List[Translation.Finding] =
              defined.toList.sortBy(_(1).line).flatMap: (path, entry) =>
                if translated.contains(path) then sci.Nil else
                  val line: Int = translated.get(path.dropRight(1)).map(_.line).getOrElse(1)
                  val message: Text = t"This has no entry for `${written(path)}`: $remedy."
                  sci.List(Translation.Finding(Tone.Warning, line, message))

            val extra: sci.List[Translation.Finding] =
              translated.toList.sortBy(_(1).line).flatMap: (path, entry) =>
                defined.get(path) match
                  case None =>
                    val message: Text =
                      t"The overlay this translates has no entry for `${written(path)}`: $remedy."

                    sci.List(Translation.Finding(Tone.Warning, entry.line, message))

                  case Some(other) =>
                    val same: Boolean =
                      entry.doc.hidden == other.doc.hidden && entry.doc.until == other.doc.until &&
                        entry.doc.refer == other.doc.refer && entry.doc.imports == other.doc.imports

                    if same then sci.Nil else
                      val message: Text =
                        t"This entry's `hidden`, `until`, `refer` and `import` are not those of " +
                          t"the entry it translates, and they are the same in every language: " +
                          t"$remedy."

                      sci.List(Translation.Finding(Tone.Failure, entry.line, message))

            val heading: sci.List[Translation.Finding] =
              original.lay(sci.List[Translation.Finding]()): (other: Overlay) =>
                if other.base == overlay.base && other.format == overlay.format then sci.Nil else
                  val message: Text =
                    t"This is of `${overlay.base}`, and the overlay it translates of " +
                      t"`${other.base}`."

                  sci.List(Translation.Finding(Tone.Failure, 1, message))

            heading ++ absent ++ extra

        val diagnostics: List[Block.Diagnostic] =
          Translation.report(path, text, List.from(runs ++ gone ++ entries))

        overlay.problems + diagnostics.map { (diagnostic: Block.Diagnostic) => diagnostic: Block }

  // How much of an overlay's translation is done: the sum over its runs of prose, a run the
  // translation lacks being one more change to make.
  def progress(text: Text, definitive: Text): Translation.Progress =
    val theirs: sci.List[Prose] = prose(text).stdlib

    prose(definitive).stdlib.foldLeft(Translation.Progress(0, 0, 0, 0, 0)):
      (total: Translation.Progress, run: Prose) =>
        theirs.find(_.key == run.key) match
          case None =>
            total.copy(stale = total.stale + 1)

          case Some(found) =>
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
