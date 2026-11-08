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

import soundness.*

import codepages.utf8Codepage
import dynamicAccess.dynamicJson
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics

// A forced alignment of a reading of a presentation's transcript, as Montreal Forced Aligner
// writes it: each word spoken, with the time it starts and ends. The output is read in any of
// MFA's formats — a Praat TextGrid (its default), JSON, or CSV — and the words of its `words`
// tier are matched, in order, to the words of the slides' transcripts, so that the time the
// first word of a slide's transcript is spoken is the time that slide should be shown: the
// deck's schedule.
object ForcedAlignment:
  case class Word(label: Text, start: Double, end: Double)

  // The labels MFA gives silence, pauses and noise, which are not words of the transcript.
  private val silent: sci.Set[Text] = sci.Set(t"", t"sil", t"sp", t"spn", t"<eps>", t"<sil>")

  private def number(text: Text): Optional[Double] = safely(text.trim.as[Double])

  // A word as it is compared: lower-case, without punctuation but for the apostrophes and
  // hyphens within it, as MFA normalises a transcript's words.
  def normal(word: Text): Text =
    val lower: String = word.s.toLowerCase.nn
    Text(lower.replaceAll("[^\\p{L}\\p{N}'’\\-]", "").nn.replace('’', '\'').nn)

  // A transcript's words, normalised, the empty ones gone.
  def words(transcript: Text): List[Text] =
    val spaced: Text = Text(transcript.s.replaceAll("\\s+", " ").nn)
    spaced.cut(t" ").map(normal).filter(_ != t"")

  // The spoken words of an alignment, in order, by the format its text is in; or what is wrong
  // with it.
  def read(text: Text): List[Word] | Text =
    val trimmed: Text = text.trim

    if trimmed.starts(t"File type = \"ooTextFile\"") then textGrid(text)
    else if trimmed.starts(t"{") then json(text)
    else if trimmed.lower.starts(t"begin,end,label") then csv(text)
    else t"not a TextGrid, JSON or CSV alignment"

  private def spoken(words: List[Word]): List[Word] | Text =
    val heard: List[Word] = words.filter { (word: Word) => !silent.contains(word.label) }
    if heard.nil then t"the alignment has no words in its `words` tier" else heard

  // A long-form TextGrid: the intervals of the item named `words`, each an `xmin`, an `xmax`
  // and a `text`.
  private def textGrid(text: Text): List[Word] | Text =
    val lines: List[Text] = text.cut(t"\n").map(_.trim)
    val start: Optional[Int] = lines.indexed.filter(_(0) == t"name = \"words\"").head.let(_(1).n0)

    start.lay(t"the TextGrid has no tier named `words`"): (start: Int) =>
      val rest: List[Text] = lines.skip(start + 1)
      val end: Int = rest.indexed.filter(_(0).starts(t"item [")).head.let(_(1).n0).or(rest.size)

      var xmin: Optional[Double] = Unset
      var xmax: Optional[Double] = Unset
      var found: List[Word] = Nil

      rest.keep(end).each: (line: Text) =>
        if line.starts(t"xmin = ") then xmin = number(line.skip(7))
        else if line.starts(t"xmax = ") then xmax = number(line.skip(7))
        else if line.starts(t"text = ") then
          val label: Text = line.skip(7).trim
          val unquoted: Text =
            if label.starts(t"\"") && label.ends(t"\"") && label.length >= 2
            then label.skip(1).keep(label.length - 2) else label

          xmin.let: (start: Double) =>
            xmax.let: (stop: Double) =>
              found = Word(unquoted.sub(t"\"\"", t"\""), start, stop) :: found

      spoken(found.reverse)

  // MFA's JSON: `tiers.words.entries`, each `[start, end, label]`.
  private def json(text: Text): List[Word] | Text =
    import strategies.throwUnsafely

    try
      val document: Json = text.read[Json]
      val entries: List[Json] = document.tiers.words.entries.as[List[Json]]

      val found: List[Word] = entries.map: (entry: Json) =>
        Word(entry(2).as[Text], entry(0).as[Double], entry(1).as[Double])

      spoken(found)
    catch case error: Exception => t"the JSON has no `tiers.words.entries` of `[start, end, word]`"

  // MFA's CSV: `Begin,End,Label,Type,Speaker`, the rows whose type is `words`.
  private def csv(text: Text): List[Word] | Text =
    val rows: List[List[Text]] = text.cut(t"\n").skip(1).map(_.trim).filter(_ != t"").map: (row: Text) =>
      row.cut(t",").map(_.trim).map: (cell: Text) =>
        if cell.starts(t"\"") && cell.ends(t"\"") && cell.length >= 2
        then cell.skip(1).keep(cell.length - 2) else cell

    val found: List[Word] = rows.bind: (cells: List[Text]) =>
      cells match
        case begin :: end :: label :: kind :: _ if kind == t"words" =>
          number(begin).let { start => number(end).let { stop => List(Word(label, start, stop)) } }.or(Nil)

        case _ =>
          Nil

    spoken(found)

  // The word MFA writes for one it does not know, which stands for any one word.
  private val unknown: Text = t"<unk>"

  // The transcript's words beside the alignment's: the edits which turn the one into the
  // other, a word the aligner did not know pairing with whichever word it stands in for. The
  // two correspond closely enough to time a presentation by when no more than one word in
  // ten, and never fewer than one word, differs — is in the one and not the other.
  case class Comparison(expected: List[Text], actual: List[Text], edits: List[Edit[Text]]):
    def differing: Int = edits.filter { edit => !edit.isInstanceOf[Par[?]] }.size
    def close: Boolean = differing <= (expected.size/10).max(1)

    // The place in the alignment of each word of the transcript, by its index, where it is.
    def places: sci.Map[Int, Int] =
      sci.Map.from(edits.stdlib.collect { case Par(left, right, _) => left -> right })

    // The differences, each shown with up to three words either side: a word of the
    // transcript not spoken as `-word`, one spoken which the transcript lacks as `+word`.
    def shown: List[Text] =
      val marked: List[(Text, Boolean)] = edits.map:
        case Par(left, _, _) => (expected.at(Ordinal.zerary(left)).or(t""), false)
        case Del(left, _)    => (t"-${expected.at(Ordinal.zerary(left)).or(t"")}", true)
        case Ins(right, _)   => (t"+${actual.at(Ordinal.zerary(right)).or(t"")}", true)

      val changed: List[Int] = marked.indexed.filter(_(0)(1)).map(_(1).n0)

      // Runs of changes, those within six words of each other shown together.
      def runs(indices: List[Int], current: List[Int]): List[List[Int]] = indices match
        case head :: tail =>
          current match
            case last :: _ if head - last <= 6 => runs(tail, head :: current)
            case Nil                           => runs(tail, List(head))
            case _                             => current.reverse :: runs(tail, List(head))

        case Nil =>
          if current.nil then Nil else List(current.reverse)

      runs(changed, Nil).map: (run: List[Int]) =>
        val first: Int = (run.head.or(0) - 3).max(0)
        val last: Int = (run.reverse.head.or(0) + 3).min(marked.size - 1)
        val words: Text = marked.skip(first).keep(last - first + 1).map(_(0)).join(t" ")
        val before: Text = if first > 0 then t"… " else t""
        val after: Text = if last < marked.size - 1 then t" …" else t""
        t"$before$words$after"

  def compare(expected: List[Text], aligned: List[Word]): Comparison =
    val actual: List[Text] = aligned.map(_.label)
    val same: (Text, Text) => Boolean = (left, right) => left == right || right == unknown
    val edits: List[Edit[Text]] = diff(expected.to[Sequence], actual.to[Sequence], same).edits.to(List)
    Comparison(expected, actual, edits)

  enum Outcome:
    case Timed(schedule: List[Double], comparison: Comparison)
    case Failed(messages: List[Text])

  // The deck's schedule from the alignment: a time for each slide after the first, at which
  // its step lands, being the time the first word of its transcript is spoken — or the first
  // word of it the alignment has, if the aligner missed that one. Every slide must have a
  // transcript, and the transcripts and the alignment must correspond closely, else the
  // outcome says what is wrong, and shows where they differ.
  def schedule(presentation: Presentation, aligned: List[Word]): Outcome =
    val transcripts: List[List[Text]] = presentation.slides.map(_.transcript.let(words).or(Nil))

    val missing: List[Text] = presentation.slides.indexed.bind: (slide: Presentation.Slide, index: Ordinal) =>
      if transcripts.at(index).or(Nil).nil
      then List(t"slide ${(index.n0 + 1).toString.tt} (line ${slide.line.toString.tt}) has no transcript, which timing it by an alignment needs")
      else Nil

    if !missing.nil then Outcome.Failed(missing) else
      val expected: List[Text] = transcripts.bind(identity(_))
      val comparison: Comparison = compare(expected, aligned)

      if !comparison.close then
        val count: Text = comparison.differing.toString.tt
        val total: Text = expected.size.toString.tt
        val heading: Text = t"the transcript and the alignment differ in $count of $total words, which is too many to time the presentation by:"
        Outcome.Failed(heading :: comparison.shown.map { hunk => t"  $hunk" })
      else
        val places: sci.Map[Int, Int] = comparison.places

        // Each slide's time: that of the first of its words the alignment has.
        val times: List[Optional[Double]] = transcripts.indexed.map: (words: List[Text], index: Ordinal) =>
          val offset: Int = transcripts.keep(index.n0).map(_.size).stdlib.sum
          val found: List[Int] = List.range(offset, offset + words.size).bind(places.get(_).toList.pipe(List.from(_)))
          found.head.let { (place: Int) => aligned.at(Ordinal.zerary(place)) }.let(_.start)

        val unplaced: List[Text] = times.indexed.bind: (time: Optional[Double], index: Ordinal) =>
          if time.present then Nil else
            val line: Int = presentation.slides.at(index).let(_.line).or(1)
            List(t"slide ${(index.n0 + 1).toString.tt} (line ${line.toString.tt}) has a transcript none of whose words the alignment has")

        // Times to the millisecond: an aligner's floats carry noise beyond it.
        def rounded(time: Double): Double = java.lang.Math.round(time*1000.0)/1000.0

        if !unplaced.nil then Outcome.Failed(unplaced)
        else Outcome.Timed(times.skip(1).map(_.or(0.0)).map(rounded), comparison)
