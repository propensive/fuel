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

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Tone}

// A translation of a text, kept in one file with the text it translates, so that the file is
// consistent by itself and can be held to the definitive text. Each line is one of four kinds,
// by how it begins:
//
//   - text      a line of the definitive text
//   + text      a line of its translation
//   ! text      a line of the definitive text as it was when the `+` lines were written
//   text        a line common to both
//
// A sigil alone is an empty line of its kind. A *passage* is a run of `!` lines, then `-` lines,
// then `+` lines, each run perhaps empty: its `+` lines translate its `-` lines as a whole, so
// a paragraph may be reflowed. A common line never begins with a sigil and a space, so a line
// which does — an item of a Markdown list, say — and is the same in both languages is written
// as a passage whose `+` lines repeat its `-` lines.
//
// The file has two readings: its *definitive* projection, the common lines and the `-` lines,
// which should be the definitive text; and its *foreign* projection, the common lines and each
// passage's `+` lines, or its `-` lines where it has none. Each knows the line of the file each
// of its lines came from, so that whatever is found in a projection is reported where the
// translator will change it.
//
// Nothing here knows what the text is: `Mirror` holds the two projections of a Markdown text
// to the same structure.
object Translation:
  enum Mark:
    case Common, Definitive, Foreign, Former

  case class Line(mark: Mark, text: Text):
    def serialize: Text = sigil(mark).lay(text): (sigil: Text) =>
      if text == t"" then sigil else t"$sigil $text"

  private def sigil(mark: Mark): Optional[Text] = mark match
    case Mark.Common     => Unset
    case Mark.Definitive => t"-"
    case Mark.Foreign    => t"+"
    case Mark.Former     => t"!"

  enum State:
    case Translated, Untranslated, Pending, Orphaned, Unpaired

  // A passage: the line of the file it begins on (from 1), and its runs. It is translated when
  // it has `-` and `+` lines alone; untranslated without `+` lines; pending while it keeps `!`
  // lines, which say its translation was written for other text; orphaned when it has no `-`
  // lines, the text it translated having gone; and unpaired when it is `+` lines alone, which
  // translate nothing.
  case class Passage(line: Int, former: List[Text], definitive: List[Text], foreign: List[Text]):
    def state: State =
      if definitive.nil then (if former.nil then State.Unpaired else State.Orphaned)
      else if !former.nil then State.Pending
      else if foreign.nil then State.Untranslated
      else State.Translated

    // Whether the translation says what the definitive text does: the line is one both
    // languages share, written as a passage because of how it begins.
    def shared: Boolean = former.nil && !definitive.nil && definitive == foreign

    def definitiveLine: Int = line + former.size
    def foreignLine: Int = line + former.size + definitive.size

  // One reading of the file: its text, and for each of its lines the line of the file it is.
  case class Projection(text: Text, origins: List[Int]):
    // The line of the file for a line of the projection, both from 1; the last line's, for one
    // beyond the end.
    def origin(line: Int): Int = origins.at((line - 1).z).or(origins.last).or(1)

  private[fuel] enum Item:
    case Shared(text: Text)
    case Group(passage: Passage)

  // A run of the edits which turn one sequence of lines into another: a line kept, by its
  // indices in both; or a change, the indices it removes from the first, those it adds from
  // the second, and how many lines of the first come before it.
  private[fuel] enum Run:
    case Kept(left: Int, right: Int)
    case Changed(removed: List[Int], added: List[Int], position: Int)

  // New lines, as `update` gives them out: a line which divides paragraphs (a blank line, or
  // one within a fence), or a paragraph.
  private enum Token:
    case Divider(line: Text)
    case Paragraph(lines: List[Text])

  // A text's lines, an empty one at the end kept.
  private[fuel] def split(text: Text): List[Text] = text.cut(t"\n")

  private def classify(line: Text): Line =
    def marked(sigil: Text, mark: Mark): Optional[Line] =
      if line == sigil then Line(mark, t"")
      else if line.starts(t"$sigil ") then Line(mark, line.skip(2))
      else Unset

    marked(t"-", Mark.Definitive).or(marked(t"+", Mark.Foreign)).or(marked(t"!", Mark.Former))
    . or(Line(Mark.Common, line))

  def parse(text: Text): Translation = Translation(split(text).map(classify))

  // A line both languages share: common, unless it would be read as a marked line, when it is
  // a passage which says the same twice.
  private def shared(line: Text): List[Line] =
    if classify(line).mark == Mark.Common then List(Line(Mark.Common, line))
    else List(Line(Mark.Definitive, line), Line(Mark.Foreign, line))

  // The marker and the info string of a line which opens a fence.
  private def opening(line: Text): Optional[(Text, Text)] = line match
    case r"\s*$marker(`{3,}|~{3,})$info(.*)" => (marker, info.trim)
    case _                                   => Unset

  // Which lines of a text are not prose to translate: the lines which open and close a fence,
  // and what is between them, unless the fence is a table, whose content is prose.
  private[fuel] def verbatim(lines: List[Text]): List[Boolean] =
    // `fence` is the marker of the open fence, empty outside one, and `prose` whether its
    // content is prose; `done` is what is found, latest first.
    def recur(lines: List[Text], fence: Text, prose: Boolean, done: List[Boolean])
    :   List[Boolean] =

      lines match
        case line :: rest =>
          if fence == t"" then opening(line) match
            case (marker: Text, info: Text) =>
              if marker.starts(t"`") && info.contains(t"`") then
                recur(rest, fence, prose, false :: done)
              else
                val table: Boolean = info.cut(r"\s+").head == Tables.language
                recur(rest, marker, table, true :: done)

            case _ =>
              recur(rest, fence, prose, false :: done)
          else
            val trimmed: Text = line.trim
            // A line closes the fence with as many of its marker's sign at least, and no other.
            val closes: Boolean =
              trimmed.length >= fence.length && trimmed.sub(fence.keep(1), t"") == t""

            if closes then recur(rest, t"", prose, true :: done)
            else recur(rest, fence, prose, !prose :: done)

        case _ =>
          done.reverse

    recur(lines, t"", false, Nil)

  // The translation of a text which nobody has translated yet: every line of prose a `-` line,
  // and the rest common.
  def skeleton(definitive: Text): Translation =
    val lines: List[Text] = split(definitive)

    Translation:
      lines.zip(verbatim(lines)).bind: (line: Text, fixed: Boolean) =>
        if fixed || line.trim == t"" then shared(line) else List(Line(Mark.Definitive, line))

  // The edits which turn one sequence of lines into another, as runs.
  private[fuel] def hunks(left: List[Text], right: List[Text]): List[Run] =
    // dissonance's `Diff` holds its edits as varargs, a standard `Seq`, read by `List.from`.
    val edits: List[Edit[Text]] = List.from(diff(left.to[Sequence], right.to[Sequence]).edits)

    // `removed` and `added` are the change being gathered, and `done` the runs before it, each
    // latest first; `consumed` is how many lines of the first sequence the edits have passed.
    def recur
      ( edits:    List[Edit[Text]],
        removed:  List[Int],
        added:    List[Int],
        consumed: Int,
        done:     List[Run] )
    :   List[Run] =

      val flushed: List[Run] =
        if removed.nil && added.nil then done
        else Run.Changed(removed.reverse, added.reverse, consumed - removed.size) :: done

      edits match
        case Par(left, right, _) :: rest =>
          recur(rest, Nil, Nil, consumed + 1, Run.Kept(left, right) :: flushed)

        case Del(left, _) :: rest =>
          recur(rest, left :: removed, added, consumed + 1, done)

        case Ins(right, _) :: rest =>
          recur(rest, removed, right :: added, consumed, done)

        case _ =>
          flushed.reverse

    recur(edits, Nil, Nil, 0, Nil)

  // A translation brought up to a definitive text which has changed. Each passage a change
  // touches keeps its `+` lines, takes the new text as its `-` lines, and keeps the text its
  // translation was written for as `!` lines — the first such text, if it has changed before —
  // so that the translator can see what the `+` lines say and what they should. New text is a
  // passage nobody has translated, or common lines where it is blank or within a fence; text
  // which has gone leaves its translation with `!` lines alone, to be deleted. Several new
  // paragraphs replacing several passages are given to them in order.
  def update(translation: Translation, definitive: Text): Translation =
    val items: List[Item] = translation.items
    val count: Int = items.size
    val lines: List[Text] = split(definitive)

    // The edits name lines by their indices, so the lines are read by index, within the bounds
    // the diff keeps them in.
    val target: Array[Text] = lines.to[Array]
    val fixed: Array[Boolean] = verbatim(lines).to[Array]

    // A passage, and the lines of the new text it is given, latest first.
    final class Draft(val passage: Passage):
      @scala.caps.unsafe.untrackedCaptures
      var lines: List[Text] = Nil

      def give(more: List[Text]): Unit = lines = more.reverse + lines

    val drafts: Map[Int, Draft] =
      items.indexed.bind: (item: Item, index: Ordinal) =>
        item match
          case Item.Group(passage) => List(index.n0 -> Draft(passage))
          case _                   => Nil

      . to[Map]

    // Each line of the definitive projection, with the index of the item it is of.
    val source: Array[(Text, Int)] =
      items.indexed.bind: (item: Item, index: Ordinal) =>
        item match
          case Item.Shared(text)   => List((text, index.n0))
          case Item.Group(passage) => passage.definitive.map((_, index.n0))

      . to[Array]

    // The item of a line of the definitive projection.
    def of(line: Int): Int = source.readUnchecked(line)(1)

    // What is written so far, latest first; and how many items are in it.
    var out: List[Draft | Line] = Nil
    var emitted: Int = 0

    // Every passage up to an item is in the output from here on.
    def reach(index: Int): Unit =
      while emitted <= index && emitted < count do
        drafts.at(emitted).let { (draft: Draft) => out = draft :: out }
        emitted += 1

    def draft(index: Int): Optional[Draft] = if index < emitted then Unset else drafts.at(index)

    def latest(draft: Draft): Boolean = out match
      case (last: Draft) :: _ => last eq draft
      case _                  => false

    hunks(source.to[List].map(_(0)), lines).each:
      case Run.Kept(left, right) =>
        val index: Int = of(left)
        val line: Text = target.readUnchecked(right)
        reach(index)

        drafts.at(index) match
          case draft: Draft => draft.give(List(line))
          case _            => out = shared(line).reverse + out

      case Run.Changed(removed, added, position) =>
        var touched: List[Int] = removed.map(of(_)).distinct.filter(drafts.at(_).present)

        // The lines of the projection either side of the change.
        val before: Optional[Draft] =
          if position == 0 then Unset
          else drafts.at(of(position - 1)).let { draft => if latest(draft) then draft else Unset }

        val following: Int = position + removed.size

        val after: Optional[Int] =
          if following >= source.length then Unset
          else
            val index: Int = of(following)
            if draft(index).present then index else Unset

        // The new lines, as paragraphs and what divides them.
        val tokens: List[Token] =
          added.fold(List[Token]()): (tokens: List[Token], index: Int) =>
            val line: Text = target.readUnchecked(index)

            if fixed.readUnchecked(index) || line.trim == t"" then Token.Divider(line) :: tokens
            else tokens match
              case Token.Paragraph(lines) :: rest => Token.Paragraph(line :: lines) :: rest
              case _                              => Token.Paragraph(List(line)) :: tokens

          . reverse

        val last: Int = tokens.size - 1

        tokens.indexed.each: (token: Token, index: Ordinal) =>
          token match
            case Token.Divider(line) =>
              out = shared(line).reverse + out

            // A paragraph's lines are latest first.
            case Token.Paragraph(lines) =>
              val receiver: Draft = touched match
                case item :: rest =>
                  touched = rest
                  reach(item)
                  drafts.at(item).or(panic(m"a touched item is a passage"))

                case _ =>
                  if index.n0 == 0 && before.present
                  then before.or(panic(m"the passage before is present"))
                  else if index.n0 == last && after.present then
                    val item: Int = after.or(panic(m"the passage after is present"))
                    reach(item)
                    drafts.at(item).or(panic(m"the passage after is a passage"))
                  else
                    val fresh: Draft = Draft(Passage(0, Nil, Nil, Nil))
                    out = fresh :: out
                    fresh

              receiver.give(lines.reverse)

        removed.last.let { (last: Int) => reach(of(last)) }

    reach(count - 1)

    Translation:
      out.reverse.bind:
        case line: Line =>
          List(line)

        case draft: Draft =>
          val passage: Passage = draft.passage
          val current: List[Text] = draft.lines.reverse

          val revised: Passage =
            if current == passage.definitive then passage
            else if passage.shared then passage.copy(definitive = current, foreign = current)
            else if !passage.former.nil || passage.foreign.nil
            then passage.copy(definitive = current)
            else passage.copy(former = passage.definitive, definitive = current)

          revised.former.map(Line(Mark.Former, _)) +
            revised.definitive.map(Line(Mark.Definitive, _)) +
            revised.foreign.map(Line(Mark.Foreign, _))

  // A change to some words, as it reads: `[-these-]` gone and `{+those+}` come, among the
  // words which stay.
  private def reworded(before: List[Text], after: List[Text]): Text =
    def words(lines: List[Text]): List[Text] = lines.bind(_.cut(r"\s+")).filter(_ != t"")

    val old: List[Text] = words(before)
    val current: List[Text] = words(after)

    // The edits name words by their indices.
    val olds: Array[Text] = old.to[Array]
    val currents: Array[Text] = current.to[Array]

    val shown: List[Text] = hunks(old, current).map:
      case Run.Kept(left, _) =>
        olds.readUnchecked(left)

      case Run.Changed(removed, added, _) =>
        val gone: Text =
          if removed.nil then t"" else t"[-${removed.map(olds.readUnchecked(_)).join(t" ")}-]"

        val come: Text =
          if added.nil then t"" else t"{+${added.map(currents.readUnchecked(_)).join(t" ")}+}"

        t"$gone$come"

    shown.join(t" ")

  // Something wrong with a translation, at a line of its text (from 1): what `findings` and
  // `Mirror` find, before it is known which file the text is in, or where in it.
  case class Finding
    ( tone:         Tone,
      line:         Int,
      message:      Text,
      detail:       Optional[Text] = Unset,
      untranslated: Boolean        = false ):

    def shifted(lines: Int): Finding = copy(line = line + lines)

  // Findings as diagnostics at their lines of the file at `path`, whose text is `file`: the
  // failures first, and the passages nobody has translated together, as one caution at the
  // first which lists the rest.
  def report(path: Text, file: Text, findings: List[Finding]): List[Block.Diagnostic] =
    import sortingAlgorithms.timsort

    def diagnostic(finding: Finding, message: Text): Block.Diagnostic =
      val style: Block.Note.Style =
        if finding.tone == Tone.Failure then Block.Note.Style.Erroneous
        else Block.Note.Style.Caution

      val excerpt: List[Block] = Excerpts.plain(file, finding.line, style)

      val detail: List[Block] =
        finding.detail.lay(List[Block]()) { text => List(Block.paragraph(text)) }

      val location: Block.Location = Block.Location(path, finding.line)
      Block.Diagnostic(finding.tone, location, List(Block.paragraph(message)), excerpt, detail)

    val untranslated: List[Finding] = findings.filter(_.untranslated).order(_.line)
    val rest: List[Finding] = findings.filter(!_.untranslated)

    val missing: List[Block.Diagnostic] =
      untranslated.head.lay(List[Block.Diagnostic]()): (first: Finding) =>
        val count: Int = untranslated.size
        val places: Text = untranslated.keep(12).map(_.line.show).join(t", ")
        val more: Text = if count > 12 then t", …" else t""

        val message: Text =
          if count == 1 then first.message
          else
            t"${count.show} passages are not translated, the first here " +
              t"(lines $places$more)."

        List(diagnostic(first, message))

    val failures: List[Block.Diagnostic] =
      rest.filter(_.tone == Tone.Failure).map { finding => diagnostic(finding, finding.message) }

    val cautions: List[Block.Diagnostic] =
      rest.filter(_.tone != Tone.Failure).map { finding => diagnostic(finding, finding.message) }

    failures + cautions + missing

  // What is wrong with a translation, as its text `source` has it, of a definitive text: each
  // place where the text it says it translates is no longer the definitive text, with the
  // change; each passage left pending or orphaned by an update; each passage nobody has
  // translated; and `+` lines which translate nothing.
  def findings(source: Text, definitive: Text): List[Finding] =
    val translation: Translation = parse(source)
    val projection: Projection = translation.definitive
    val before: List[Text] = split(projection.text)
    val after: List[Text] = split(definitive)

    // The edits name lines by their indices.
    val befores: Array[Text] = before.to[Array]
    val afters: Array[Text] = after.to[Array]

    val stale: List[Finding] = hunks(before, after).bind:
      case Run.Changed(removed, added, position) =>
        val line: Int = projection.origin(if !removed.nil then position + 1 else position.max(1))

        val message: Text =
          if removed.nil then
            t"The text this translates has gained lines after this one, which are not " +
              t"translated."
          else if added.nil then t"The text this translates has lost these lines."
          else t"The text this translates has changed here."

        val change: Text =
          reworded(removed.map(befores.readUnchecked(_)), added.map(afters.readUnchecked(_)))

        List(Finding(Tone.Warning, line, message, change))

      case _ =>
        Nil

    val states: List[Finding] =
      translation.passages.bind: (passage: Passage) =>
        passage.state match
          case State.Unpaired =>
            val message: Text =
              t"These lines translate nothing: `+` lines follow the `-` lines they translate."

            List(Finding(Tone.Failure, passage.line, message))

          case State.Pending =>
            val message: Text =
              t"The text this translates has changed, from the `!` lines to the `-` lines: " +
                t"revise the `+` lines, then delete the `!` lines."

            List(Finding(Tone.Warning, passage.line, message))

          case State.Orphaned =>
            val message: Text = t"The text this translates has gone: delete these lines."
            List(Finding(Tone.Warning, passage.line, message))

          case State.Untranslated =>
            List:
              Finding(Tone.Warning, passage.line, t"This is not translated.", untranslated = true)

          case State.Translated =>
            Nil

    stale + states

  def check(path: Text, source: Text, definitive: Text): List[Block.Diagnostic] =
    report(path, source, findings(source, definitive))

  // How much of a translation is done: its passages by state, those which say the same as the
  // definitive text aside, and the places where it differs from the definitive text.
  case class Progress(translated: Int, untranslated: Int, pending: Int, orphaned: Int, stale: Int):
    def complete: Boolean = untranslated == 0 && pending == 0 && orphaned == 0 && stale == 0

  def progress(translation: Translation, definitive: Text): Progress =
    val passages: List[Passage] = translation.passages.filter(!_.shared)
    def count(state: State): Int = passages.count(_.state == state)

    val changes: Int =
      hunks(split(translation.definitive.text), split(definitive)).count:
        case Run.Changed(_, _, _) => true
        case _                    => false

    Progress
      ( count(State.Translated), count(State.Untranslated), count(State.Pending),
        count(State.Orphaned), changes )

case class Translation(lines: List[Translation.Line]):
  import Translation.{Item, Line, Mark, Passage, Projection}

  def serialize: Text = lines.map(_.serialize).join(t"\n")

  // The file as its common lines and its passages, in order: each passage is the run of `!`
  // lines, then of `-` lines, then of `+` lines, which begins at its first marked line.
  private[fuel] lazy val items: List[Item] =
    // `index` is the line of the file (from 0) of the first of `lines`; `done` is what is
    // found, latest first.
    def recur(lines: List[Line], index: Int, done: List[Item]): List[Item] = lines match
      case line :: rest =>
        if line.mark == Mark.Common then recur(rest, index + 1, Item.Shared(line.text) :: done)
        else
          def run(mark: Mark, lines: List[Line]): List[Text] =
            lines.keep(_.mark == mark).map(_.text)

          val former: List[Text] = run(Mark.Former, lines)
          val definitive: List[Text] = run(Mark.Definitive, lines.skip(former.size))
          val foreign: List[Text] = run(Mark.Foreign, lines.skip(former.size + definitive.size))
          val size: Int = former.size + definitive.size + foreign.size
          val passage: Passage = Passage(index + 1, former, definitive, foreign)

          recur(lines.skip(size), index + size, Item.Group(passage) :: done)

      case _ =>
        done.reverse

    recur(lines, 0, Nil)

  def passages: List[Passage] = items.bind:
    case Item.Group(passage) => List(passage)
    case _                   => Nil

  private def projection(lines: List[(Text, Int)]): Projection =
    Projection(lines.map(_(0)).join(t"\n"), lines.map(_(1)))

  private def numbered(lines: List[Text], first: Int): List[(Text, Int)] =
    lines.indexed.map { (line: Text, index: Ordinal) => (line, first + index.n0) }

  // A projection's lines, each with the line of the file it is: a common line's is counted
  // from the line after the passage before it.
  private def projected(passage: Passage => List[(Text, Int)]): Projection =
    def recur(items: List[Item], line: Int, done: List[(Text, Int)]): List[(Text, Int)] =
      items match
        case Item.Shared(text) :: rest =>
          recur(rest, line + 1, (text, line + 1) :: done)

        case Item.Group(group) :: rest =>
          val last: Int =
            group.line + group.former.size + group.definitive.size + group.foreign.size - 1

          recur(rest, last, passage(group).reverse + done)

        case _ =>
          done.reverse

    projection(recur(items, 0, Nil))

  // The definitive text, as the translation has it.
  def definitive: Projection = projected: (passage: Passage) =>
    numbered(passage.definitive, passage.definitiveLine)

  // The translated text: a passage nobody has translated is as the definitive text has it, and
  // one which translates nothing is left out.
  def foreign: Projection = projected: (passage: Passage) =>
    if passage.definitive.nil then Nil
    else if passage.foreign.nil then numbered(passage.definitive, passage.definitiveLine)
    else numbered(passage.foreign, passage.foreignLine)
