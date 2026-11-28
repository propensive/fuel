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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}

// A forced alignment of a reading of a presentation's transcript, as Montreal Forced Aligner
// writes it: each word spoken, with the time it starts and ends. The output is read in any of
// MFA's formats — a Praat TextGrid (its default), JSON, or CSV — and the words of its `words`
// tier are matched, in order, to the words of the transcript, so that the time the first word
// of a state's cue is spoken is the time the presentation should come to that state: the deck's
// schedule.
object ForcedAlignment:
  case class Word(label: Text, start: Double, end: Double)

  // The labels MFA gives silence, pauses and noise, which are not words of the transcript.
  private val silent: Set[Text] = Set(t"", t"sil", t"sp", t"spn", t"<eps>", t"<sil>")

  private def number(text: Text): Optional[Double] = safely(text.trim.as[Double])

  // A word as it is compared: lower-case, without punctuation but for the apostrophes and
  // hyphens within it, as MFA normalises a transcript's words.
  def normal(word: Text): Text =
    word.lower.sub(r"[^\p{L}\p{N}'’\-]", t"").sub(t"’", t"'")

  // A transcript's words, normalised, the empty ones gone.
  def words(transcript: Text): List[Text] =
    val spaced: Text = transcript.sub(r"\s+", t" ")
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
    val heard: List[Word] = words.filter { (word: Word) => !silent.has(word.label) }
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
    val found: Optional[List[Word]] = safely:
      val document: Json = text.read[Json]
      val entries: List[Json] = dynamically[Json](document.tiers.words.entries.as[List[Json]])

      entries.map: (entry: Json) =>
        Word(entry(2).as[Text], entry(0).as[Double], entry(1).as[Double])

    found.lay(t"the JSON has no `tiers.words.entries` of `[start, end, word]`")(spoken(_))

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
  case class Comparison(expected: List[Text], actual: List[Text], edits: List[Diff.Edit[Text]]):
    def differing: Int = edits.filter { edit => !edit.isInstanceOf[Diff.Par[?]] }.size
    def close: Boolean = differing <= (expected.size/10).max(1)

    // The place in the alignment of each word of the transcript, by its index, where it is.
    def places: Map[Int, Int] =
      edits.bind:
        case Diff.Par(left, right, _) => List(left -> right)
        case _                        => Nil

      . to[Map]

    // The differences, each shown with up to three words either side: a word of the
    // transcript not spoken as `-word`, one spoken which the transcript lacks as `+word`.
    def shown: List[Text] =
      val marked: List[(Text, Boolean)] = edits.map:
        case Diff.Par(left, _, _) => (expected.at(Ordinal.zerary(left)).or(t""), false)
        case Diff.Del(left, _)    => (t"-${expected.at(Ordinal.zerary(left)).or(t"")}", true)
        case Diff.Ins(right, _)   => (t"+${actual.at(Ordinal.zerary(right)).or(t"")}", true)

      val changed: List[Int] = marked.indexed.filter(_(0)(1)).map(_(1).n0)

      // Runs of changes, those within six words of each other shown together.
      def runs(indices: List[Int], current: List[Int]): List[List[Int]] = indices match
        case head :: tail =>
          current match
            case last :: _ if head - last <= 6 => runs(tail, head :: current)
            case Nil                           => runs(tail, List(head))
            case _                             => current.reverse :: runs(tail, List(head))

        case _ =>
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
    val edits: List[Diff.Edit[Text]] =
      diff(expected.to[Sequence], actual.to[Sequence], same).edits.to(List)
    Comparison(expected, actual, edits)

  enum Outcome:
    case Timed(schedule: List[Double], comparison: Comparison)
    case Failed(code: fulminate.Bolt, messages: List[Text])

  // The code of why a presentation cannot be timed by an alignment, `↯/f.27.1.<variant>`: a
  // file to align by which cannot be read (1); an aligner which could not be installed (2), or
  // which failed (3); no cache directory to install one in (4); an alignment which cannot be
  // read (5); a presentation given no transcript, or a state after the first without an id
  // (6); a transcript and an alignment which differ too much (7); a state none of whose cue's
  // words the alignment has (8); and a language there is no model to align a recording in
  // with (9).
  private[fuel] def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.client, FuelBolts.alignment, variant)

  // The deck's schedule from the alignment: a time for each state after the first, at which
  // its step lands, being the time the first word of its cue is spoken — or, if the aligner
  // missed that one, the first word the alignment has from there to the next cue. Every state
  // after the first must have an id, which a transcript cohering with the presentation cues,
  // and the transcript and the alignment must correspond closely, else the outcome says what
  // is wrong, and shows where they differ.
  def schedule(states: List[Presentation.State], transcript: Transcript, aligned: List[Word])
  :   Outcome =

    val later: List[Presentation.State] = states.skip(1)

    val unnamed: List[Text] = later.bind: (state: Presentation.State) =>
      if state.id.present then Nil
      else List(t"${state.described} has no id, which timing it by an alignment needs")

    if !unnamed.nil then Outcome.Failed(bolt(6), unnamed) else
      val expected: List[Text] = words(transcript.text)
      val comparison: Comparison = compare(expected, aligned)

      if !comparison.close then
        val count: Text = comparison.differing.show
        val total: Text = expected.size.show
        val heading: Text = t"the transcript and the alignment differ in $count of $total words, which is too many to time the presentation by:"
        Outcome.Failed(bolt(7), heading :: comparison.shown.map { hunk => t"  $hunk" })
      else
        val places: Map[Int, Int] = comparison.places

        // Each cue's place among the transcript's words: how many words are before it.
        val cued: List[(Text, Int)] = transcript.cues.map: (cue: Transcript.Cue) =>
          (cue.id, words(transcript.text.keep(cue.offset)).size)

        // A state's time: that of the first word the alignment has from its cue's first to
        // the next cue's.
        def time(id: Text): Optional[Double] =
          cued.filter(_(0) == id).head.let(_(1)).let: (from: Int) =>
            val to: Int = cued.map(_(1)).filter(_ > from).head.or(expected.size)
            val found: List[Int] = List.range(from, to).bind(places.at(_).lay(Nil)(List(_)))
            found.head.let { (place: Int) => aligned.at(Ordinal.zerary(place)) }.let(_.start)

        val times: List[Optional[Double]] = later.map(_.id.let(time))

        val unplaced: List[Text] = later.zip(times).bind: (state: Presentation.State, time: Optional[Double]) =>
          if time.present then Nil else
            val id: Text = state.id.or(t"")
            List(t"the state `$id`, ${state.described}, is cued by words none of which the alignment has")

        // Times to the millisecond: an aligner's floats carry noise beyond it.
        def rounded(time: Double): Double = (time*1000.0).round/1000.0

        if !unplaced.nil then Outcome.Failed(bolt(8), unplaced)
        else Outcome.Timed(times.map(_.or(0.0)).map(rounded), comparison)
