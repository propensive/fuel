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

  // When each word of a transcript is spoken: the words are matched in order to the aligned
  // words, each to the next aligned word with its label within a few of where the count puts
  // it — so that a word the aligner did not know (`<unk>`), or split or joined, puts the match
  // off by no more than itself — else to the aligned word at that place; `Unset` once the
  // alignment runs out.
  def times(transcript: List[Text], aligned: List[Word]): List[Optional[Double]] =
    var cursor: Int = 0

    transcript.map: (word: Text) =>
      if cursor >= aligned.size then Unset else
        val window: List[(Word, Ordinal)] = aligned.skip(cursor).keep(4).indexed
        val hit: Optional[Int] = window.filter(_(0).label == word).head.let(_(1).n0)

        hit.lay(aligned.at(Ordinal.zerary(cursor)).let(_.start).also { cursor += 1 }):
          (offset: Int) =>
            val found: Optional[Double] = aligned.at(Ordinal.zerary(cursor + offset)).let(_.start)
            cursor += offset + 1
            found

  // The deck's schedule and what had to be decided: a time for each slide after the first, at
  // which its step lands, being the time the first word of its transcript is spoken. A slide
  // without a transcript, or whose transcript the alignment does not reach, has no time of
  // its own: one between two timed slides is given a time between theirs, evenly, and one
  // after the last timed slide follows the last spoken word by a second, each a second apart.
  def schedule(presentation: Presentation, aligned: List[Word]): (List[Double], List[Text]) =
    val transcripts: List[List[Text]] =
      presentation.slides.map(_.transcript.let(words).or(Nil))

    val spoken: List[Optional[Double]] = times(transcripts.bind(identity(_)), aligned)

    // Each slide's own time: that of its first word.
    val own: List[Optional[Double]] =
      transcripts.indexed.map: (words: List[Text], index: Ordinal) =>
        if words.nil then Unset
        else spoken.at(Ordinal.zerary(transcripts.keep(index.n0).map(_.size).stdlib.sum)).or(Unset)

    val lastWord: Double = aligned.reverse.head.let(_.end).or(0.0)
    val count: Int = own.size

    def timed(index: Int): Optional[Double] = own.at(Ordinal.zerary(index)).or(Unset)

    // The nearest timed slide before (the first slide counting as shown at its own time, or
    // zero) and after an untimed one.
    def before(index: Int): (Int, Double) =
      var i: Int = index - 1
      while i > 0 && timed(i).absent do i -= 1
      (i, timed(i).or(0.0))

    def after(index: Int): Optional[(Int, Double)] =
      var i: Int = index + 1
      while i < count && timed(i).absent do i += 1
      if i < count then timed(i).let((i, _)) else Unset

    var notes: List[Text] = Nil

    val all: List[Double] = List.range(0, count).map: (index: Int) =>
      timed(index).or:
        val slide: Int = index + 1
        val line: Int = presentation.slides.at(Ordinal.zerary(index)).let(_.line).or(1)
        val why: Text =
          if transcripts.at(Ordinal.zerary(index)).or(Nil).nil then t"has no transcript"
          else t"has a transcript the alignment does not reach"

        if index == 0 then 0.0 else
          val (from, start) = before(index)
          val time: Double = after(index) match
            case (to: Int, stop: Double) => start + (stop - start)*(index - from)/(to - from)
            case _                       => lastWord + (index - from)*1.0

          val boxed: java.lang.Double = java.lang.Double.valueOf(time).nn
          val said: Text = Text(String.format(java.util.Locale.ROOT, "%.2f", boxed).nn)
          notes = notes :+ t"slide $slide (line $line) $why, so its step is placed at $said s"
          time

    (all.skip(1), notes)
