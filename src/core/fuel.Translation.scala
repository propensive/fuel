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
import scala.collection.mutable as scm

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
    def origin(line: Int): Int =
      val lines: sci.List[Int] = origins.stdlib
      lines.lift(line - 1).orElse(lines.lastOption).getOrElse(1)

  private[fuel] enum Item:
    case Shared(text: Text)
    case Group(passage: Passage)

  // A string cut at each match of a pattern, empty strings at the end kept.
  private def pieces(string: String, pattern: String): sci.Vector[String] =
    sci.ArraySeq.unsafeWrapArray(string.split(pattern, -1).nn).toVector.map(_.nn)

  private[fuel] def split(text: Text): sci.Vector[String] = pieces(text.s, "\n")

  private def classify(line: String): Line =
    def marked(sigil: Char, mark: Mark): Optional[Line] =
      if line.length == 1 && line.charAt(0) == sigil then Line(mark, t"")
      else if line.length >= 2 && line.charAt(0) == sigil && line.charAt(1) == ' '
      then Line(mark, Text(line.substring(2).nn))
      else Unset

    marked('-', Mark.Definitive).or(marked('+', Mark.Foreign)).or(marked('!', Mark.Former))
    . or(Line(Mark.Common, Text(line)))

  def parse(text: Text): Translation = Translation(List.from(split(text).map(classify).toList))

  // A line both languages share: common, unless it would be read as a marked line, when it is
  // a passage which says the same twice.
  private def shared(line: String): sci.List[Line] =
    val text: Text = Text(line)

    if classify(line).mark == Mark.Common then sci.List(Line(Mark.Common, text))
    else sci.List(Line(Mark.Definitive, text), Line(Mark.Foreign, text))

  private val opening: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("^\\s*(`{3,}|~{3,})(.*)$").nn

  // Which lines of a text are not prose to translate: the lines which open and close a fence,
  // and what is between them, unless the fence is a table, whose content is prose.
  private[fuel] def verbatim(lines: sci.Vector[String]): sci.Vector[Boolean] =
    var fence: String = ""
    var prose: Boolean = false

    lines.map: (line: String) =>
      if fence == "" then
        val matcher: java.util.regex.Matcher = opening.matcher(line).nn

        if !matcher.matches then false else
          val marker: String = matcher.group(1).nn
          val info: String = matcher.group(2).nn.trim.nn

          if marker.charAt(0) == '`' && info.contains("`") then false else
            fence = marker
            prose = pieces(info, "\\s+").headOption.contains(Tables.language.s)
            true
      else
        val trimmed: String = line.trim.nn

        val closes: Boolean =
          trimmed.length >= fence.length && trimmed.forall(_ == fence.charAt(0))

        if closes then
          fence = ""
          true
        else !prose

  // The translation of a text which nobody has translated yet: every line of prose a `-` line,
  // and the rest common.
  def skeleton(definitive: Text): Translation =
    val lines: sci.Vector[String] = split(definitive)
    val fixed: sci.Vector[Boolean] = verbatim(lines)

    val marked: sci.Vector[Line] = lines.zip(fixed).flatMap: (line: String, fixed: Boolean) =>
      if fixed || line.trim.nn == "" then shared(line)
      else sci.List(Line(Mark.Definitive, Text(line)))

    Translation(List.from(marked.toList))

  // The edits which turn one sequence of lines into another, as runs: each `Right` a line
  // kept, by its indices in both, and each `Left` a change, the indices it removes from the
  // first, those it adds from the second, and how many lines of the first come before it.
  private[fuel] def hunks(left: sci.Vector[String], right: sci.Vector[String])
  :   sci.Vector[Either[(sci.Vector[Int], sci.Vector[Int], Int), (Int, Int)]] =

    val before: Sequence[Text] = Sequence.from(left.map(Text(_)))
    val after: Sequence[Text] = Sequence.from(right.map(Text(_)))
    val edits: sci.Vector[Edit[Text]] = diff(before, after).edits.to(List).stdlib.toVector
    val out = sci.Vector.newBuilder[Either[(sci.Vector[Int], sci.Vector[Int], Int), (Int, Int)]]
    var removed: sci.Vector[Int] = sci.Vector()
    var added: sci.Vector[Int] = sci.Vector()
    var consumed: Int = 0

    def flush(): Unit =
      if removed.nonEmpty || added.nonEmpty then
        out += Left((removed, added, consumed - removed.size))
        removed = sci.Vector()
        added = sci.Vector()

    edits.foreach:
      case Par(left, right, _) =>
        flush()
        consumed += 1
        out += Right((left, right))

      case Del(left, _) =>
        consumed += 1
        removed = removed :+ left

      case Ins(right, _) =>
        added = added :+ right

    flush()
    out.result()

  // A translation brought up to a definitive text which has changed. Each passage a change
  // touches keeps its `+` lines, takes the new text as its `-` lines, and keeps the text its
  // translation was written for as `!` lines — the first such text, if it has changed before —
  // so that the translator can see what the `+` lines say and what they should. New text is a
  // passage nobody has translated, or common lines where it is blank or within a fence; text
  // which has gone leaves its translation with `!` lines alone, to be deleted. Several new
  // paragraphs replacing several passages are given to them in order.
  def update(translation: Translation, definitive: Text): Translation =
    val items: sci.Vector[Item] = translation.items
    val target: sci.Vector[String] = split(definitive)
    val fixed: sci.Vector[Boolean] = verbatim(target)

    final class Draft(val passage: Passage):
      val lines: scm.ArrayBuffer[String] = scm.ArrayBuffer()

    val drafts: sci.Map[Int, Draft] =
      sci.Map.from:
        items.zipWithIndex.collect { case (Item.Group(passage), index) => index -> Draft(passage) }

    // Each line of the definitive projection, with the item it is of.
    val source: sci.Vector[(String, Int)] = items.zipWithIndex.flatMap:
      case (Item.Shared(text), index) =>
        sci.Vector((text.s, index))

      case (Item.Group(passage), index) =>
        passage.definitive.stdlib.toVector.map { line => (line.s, index) }

    val out: scm.ArrayBuffer[Draft | Line] = scm.ArrayBuffer()
    var emitted: Int = 0

    // Every passage up to an item is in the output from here on.
    def reach(index: Int): Unit =
      while emitted <= index && emitted < items.size do
        drafts.get(emitted).foreach(out += _)
        emitted += 1

    def draft(index: Int): Option[Draft] = if index < emitted then None else drafts.get(index)

    hunks(source.map(_(0)), target).foreach:
      case Right((left, right)) =>
        val index: Int = source(left)(1)
        reach(index)

        drafts.get(index) match
          case Some(draft) => draft.lines += target(right)
          case None        => out ++= shared(target(right))

      case Left((removed, added, position)) =>
        val touched: scm.Queue[Int] =
          scm.Queue.from(removed.map(source(_)(1)).distinct.filter(drafts.contains))

        // The lines of the projection either side of the change.
        val before: Option[Draft] =
          if position == 0 then None
          else
            drafts.get(source(position - 1)(1)).filter: draft =>
              out.lastOption.exists(_ eq draft)

        val following: Int = position + removed.size

        val after: Option[Int] =
          if following >= source.size then None
          else Some(source(following)(1)).filter { index => draft(index).isDefined }

        // The new lines, as paragraphs and what divides them.
        val tokens: sci.Vector[Either[String, sci.Vector[String]]] =
          added.foldLeft(sci.Vector[Either[String, sci.Vector[String]]]()): (tokens, index) =>
            val line: String = target(index)

            if fixed(index) || line.trim.nn == "" then tokens :+ Left(line)
            else tokens.lastOption match
              case Some(Right(lines)) => tokens.init :+ Right(lines :+ line)
              case _                  => tokens :+ Right(sci.Vector(line))

        tokens.zipWithIndex.foreach:
          case (Left(line), _) =>
            out ++= shared(line)

          case (Right(lines), index) =>
            val receiver: Draft =
              if touched.nonEmpty then
                val item: Int = touched.dequeue()
                reach(item)
                drafts(item)
              else if index == 0 && before.isDefined then before.get
              else if index == tokens.size - 1 && after.isDefined then
                reach(after.get)
                drafts(after.get)
              else
                val fresh: Draft = Draft(Passage(0, Nil, Nil, Nil))
                out += fresh
                fresh

            receiver.lines ++= lines

        removed.lastOption.foreach { last => reach(source(last)(1)) }

    reach(items.size - 1)

    val lines: sci.Vector[Line] = out.toVector.flatMap:
      case line: Line =>
        sci.Vector(line)

      case draft: Draft =>
        val passage: Passage = draft.passage
        val current: List[Text] = List.from(draft.lines.toList.map(Text(_)))

        val revised: Passage =
          if current == passage.definitive then passage
          else if passage.shared then passage.copy(definitive = current, foreign = current)
          else if !passage.former.nil || passage.foreign.nil then passage.copy(definitive = current)
          else passage.copy(former = passage.definitive, definitive = current)

        revised.former.stdlib.toVector.map(Line(Mark.Former, _)) ++
          revised.definitive.stdlib.toVector.map(Line(Mark.Definitive, _)) ++
          revised.foreign.stdlib.toVector.map(Line(Mark.Foreign, _))

    Translation(List.from(lines.toList))

  // A change to some words, as it reads: `[-these-]` gone and `{+those+}` come, among the
  // words which stay.
  private def reworded(before: sci.Vector[String], after: sci.Vector[String]): Text =
    def words(lines: sci.Vector[String]): sci.Vector[String] =
      lines.flatMap(pieces(_, "\\s+")).filter(_ != "")

    val old: sci.Vector[String] = words(before)
    val current: sci.Vector[String] = words(after)

    val shown: sci.Vector[String] = hunks(old, current).map:
      case Right((left, _)) =>
        old(left)

      case Left((removed, added, _)) =>
        val gone: String =
          if removed.isEmpty then "" else removed.map(old(_)).mkString("[-", " ", "-]")

        val come: String =
          if added.isEmpty then "" else added.map(current(_)).mkString("{+", " ", "+}")

        gone + come

    Text(shown.mkString(" "))

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
    def diagnostic(finding: Finding, message: Text): Block.Diagnostic =
      val style: Block.Note.Style =
        if finding.tone == Tone.Failure then Block.Note.Style.Erroneous
        else Block.Note.Style.Caution

      val excerpt: List[Block] = Excerpts.plain(file, finding.line, style)

      val detail: List[Block] =
        finding.detail.lay(List[Block]()) { text => List(Block.paragraph(text)) }

      val location: Block.Location = Block.Location(path, finding.line)
      Block.Diagnostic(finding.tone, location, List(Block.paragraph(message)), excerpt, detail)

    val all: sci.Vector[Finding] = findings.stdlib.toVector
    val untranslated: sci.Vector[Finding] = all.filter(_.untranslated).sortBy(_.line)
    val rest: sci.Vector[Finding] = all.filter(!_.untranslated)

    val missing: sci.Vector[Block.Diagnostic] =
      untranslated.headOption.toVector.map: (first: Finding) =>
        val count: Int = untranslated.size
        val places: String = untranslated.take(12).map(_.line.toString).mkString(", ")
        val more: String = if count > 12 then ", …" else ""

        val message: Text =
          if count == 1 then first.message
          else
            t"${count.show} passages are not translated, the first here " +
              t"(lines ${Text(places + more)})."

        diagnostic(first, message)

    val failures: sci.Vector[Block.Diagnostic] =
      rest.filter(_.tone == Tone.Failure).map { finding => diagnostic(finding, finding.message) }

    val cautions: sci.Vector[Block.Diagnostic] =
      rest.filter(_.tone != Tone.Failure).map { finding => diagnostic(finding, finding.message) }

    List.from((failures ++ cautions ++ missing).toList)

  // What is wrong with a translation, as its text `source` has it, of a definitive text: each
  // place where the text it says it translates is no longer the definitive text, with the
  // change; each passage left pending or orphaned by an update; each passage nobody has
  // translated; and `+` lines which translate nothing.
  def findings(source: Text, definitive: Text): List[Finding] =
    val translation: Translation = parse(source)
    val projection: Projection = translation.definitive
    val before: sci.Vector[String] = split(projection.text)
    val after: sci.Vector[String] = split(definitive)

    val stale: sci.Vector[Finding] = hunks(before, after).collect:
      case Left((removed, added, position)) =>
        val line: Int =
          projection.origin(if removed.nonEmpty then position + 1 else position.max(1))

        val message: Text =
          if removed.isEmpty then
            t"The text this translates has gained lines after this one, which are not " +
              t"translated."
          else if added.isEmpty then t"The text this translates has lost these lines."
          else t"The text this translates has changed here."

        Finding(Tone.Warning, line, message, reworded(removed.map(before(_)), added.map(after(_))))

    val states: sci.Vector[Finding] =
      translation.passages.stdlib.toVector.flatMap: (passage: Passage) =>
        passage.state match
          case State.Unpaired =>
            val message: Text =
              t"These lines translate nothing: `+` lines follow the `-` lines they translate."

            sci.Vector(Finding(Tone.Failure, passage.line, message))

          case State.Pending =>
            val message: Text =
              t"The text this translates has changed, from the `!` lines to the `-` lines: " +
                t"revise the `+` lines, then delete the `!` lines."

            sci.Vector(Finding(Tone.Warning, passage.line, message))

          case State.Orphaned =>
            val message: Text = t"The text this translates has gone: delete these lines."
            sci.Vector(Finding(Tone.Warning, passage.line, message))

          case State.Untranslated =>
            sci.Vector:
              Finding(Tone.Warning, passage.line, t"This is not translated.", untranslated = true)

          case State.Translated =>
            sci.Vector()

    List.from((stale ++ states).toList)

  def check(path: Text, source: Text, definitive: Text): List[Block.Diagnostic] =
    report(path, source, findings(source, definitive))

  // How much of a translation is done: its passages by state, those which say the same as the
  // definitive text aside, and the places where it differs from the definitive text.
  case class Progress(translated: Int, untranslated: Int, pending: Int, orphaned: Int, stale: Int):
    def complete: Boolean = untranslated == 0 && pending == 0 && orphaned == 0 && stale == 0

  def progress(translation: Translation, definitive: Text): Progress =
    val passages: sci.Vector[Passage] = translation.passages.stdlib.toVector.filter(!_.shared)
    def count(state: State): Int = passages.count(_.state == state)
    val changes: Int = hunks(split(translation.definitive.text), split(definitive)).count(_.isLeft)

    Progress
      ( count(State.Translated), count(State.Untranslated), count(State.Pending),
        count(State.Orphaned), changes )

case class Translation(lines: List[Translation.Line]):
  import Translation.{Item, Line, Mark, Passage, Projection}

  def serialize: Text = lines.map(_.serialize).join(t"\n")

  // The file as its common lines and its passages, in order.
  private[fuel] lazy val items: sci.Vector[Item] =
    val all: sci.Vector[Line] = lines.stdlib.toVector
    val out = sci.Vector.newBuilder[Item]
    var index: Int = 0

    def run(mark: Mark): List[Text] =
      val start: Int = index
      while index < all.size && all(index).mark == mark do index += 1
      List.from(all.slice(start, index).map(_.text).toList)

    while index < all.size do
      if all(index).mark == Mark.Common then
        out += Item.Shared(all(index).text)
        index += 1
      else
        val start: Int = index
        val former: List[Text] = run(Mark.Former)
        val definitive: List[Text] = run(Mark.Definitive)
        val foreign: List[Text] = run(Mark.Foreign)
        out += Item.Group(Passage(start + 1, former, definitive, foreign))

    out.result()

  def passages: List[Passage] =
    List.from(items.collect { case Item.Group(passage) => passage }.toList)

  private def projection(lines: sci.Vector[(Text, Int)]): Projection =
    Projection(List.from(lines.map(_(0)).toList).join(t"\n"), List.from(lines.map(_(1)).toList))

  private def numbered(lines: List[Text], first: Int): sci.Vector[(Text, Int)] =
    lines.stdlib.toVector.zipWithIndex.map { (line, index) => (line, first + index) }

  // The definitive text, as the translation has it.
  def definitive: Projection =
    var line: Int = 0

    projection:
      items.flatMap:
        case Item.Shared(text) =>
          line += 1
          sci.Vector((text, line))

        case Item.Group(passage) =>
          line =
            passage.line + passage.former.size + passage.definitive.size + passage.foreign.size - 1

          numbered(passage.definitive, passage.definitiveLine)

  // The translated text: a passage nobody has translated is as the definitive text has it, and
  // one which translates nothing is left out.
  def foreign: Projection =
    var line: Int = 0

    projection:
      items.flatMap:
        case Item.Shared(text) =>
          line += 1
          sci.Vector((text, line))

        case Item.Group(passage) =>
          line =
            passage.line + passage.former.size + passage.definitive.size + passage.foreign.size - 1

          if passage.definitive.nil then sci.Vector()
          else if passage.foreign.nil then numbered(passage.definitive, passage.definitiveLine)
          else numbered(passage.foreign, passage.foreignLine)
