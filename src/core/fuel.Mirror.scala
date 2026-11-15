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

// The check that a translation of a Markdown text has the structure of the text it translates:
// the same blocks in the same order — a heading of the same level for each heading, a list of
// as many items for each list, a quotation for each quotation — and every fence the same,
// since code is not translated; a table's fence, whose content is prose, is held to having one.
// A paragraph which names different code, or leads to different places, than the one it
// translates is a caution. Each is found between the two projections of the translation's file
// and reported at its line of that file, once no passage of it awaits revision.
object Mirror:
  // A difference, by the lines (from 1) of the foreign and the definitive projections.
  private case class Divergence
    ( foreign:    Int,
      definitive: Int,
      message:    Text,
      tone:       Tone = Tone.Failure )

  private def name(node: Layout): Text = node match
    case _: Layout.BlockQuote    => t"a quotation"
    case _: Layout.OrderedList   => t"a numbered list"
    case _: Layout.BulletList    => t"a list"
    case _: Layout.CodeBlock     => t"a fence"
    case _: Layout.Paragraph     => t"a paragraph"
    case _: Layout.Heading       => t"a heading"
    case _: Layout.ThematicBreak => t"a rule"
    case _: Layout.HtmlBlock     => t"a block of HTML"

  // A list without one occurrence of each element of another, as a multiset's difference.
  private def without[element](left: List[element], right: List[element]): List[element] =
    right.fold(left): (rest: List[element], element: element) =>
      rest.where(_ == element).lay(rest): (index: Ordinal) =>
        rest.keep(index.n0) + rest.skip(index.n0 + 1)

  // The code a run of prose names and the places it leads to, in order of their text.
  private def references(prose: List[Prose]): List[Text] =
    import sortingAlgorithms.timsort
    import soundness.collationComparable
    import collations.codepointCollation

    // The Markdown tree holds a node's children as varargs, a standard `Seq`, which only
    // `List.from` reads as a Soundness list; so too in `pair`.
    def recur(node: Prose): List[Text] = node match
      case Prose.Code(code)                       => List(t"`$code`")
      case Prose.Emphasis(children*)              => List.from(children).bind(recur)
      case Prose.Strong(children*)                => List.from(children).bind(recur)
      case Prose.Link(destination, _, children*)  => destination :: List.from(children).bind(recur)
      case Prose.Image(destination, _, children*) => destination :: List.from(children).bind(recur)
      case _                                      => Nil

    prose.bind(recur).sort

  private def cited
    ( foreign:    Layout,
      definitive: Layout,
      theirs:     List[Prose],
      ours:       List[Prose] )
  :   List[Divergence] =

    val expected: List[Text] = references(ours)
    val found: List[Text] = references(theirs)

    if expected == found then Nil else
      val missing: List[Text] = without(expected, found)
      val extra: List[Text] = without(found, expected)

      val lacks: Text = if missing.nil then t"" else t" It lacks ${missing.join(t", ")}."

      val adds: Text =
        if extra.nil then t"" else t" It has ${extra.join(t", ")}, which the other does not."

      val message: Text =
        t"This does not name the code, or lead to the places, that the text it translates " +
          t"does.$lacks$adds"

      List(Divergence(foreign.line.n1, definitive.line.n1, message, Tone.Warning))

  private def items
    ( foreign:    Layout,
      definitive: Layout,
      theirs:     List[List[Layout]],
      ours:       List[List[Layout]] )
  :   List[Divergence] =

    if theirs.size != ours.size then
      val message: Text =
        t"This list has ${theirs.size.show} items, and the one it translates has ${ours.size.show}."

      List(Divergence(foreign.line.n1, definitive.line.n1, message))
    else
      theirs.zip(ours).bind: (theirs, ours) =>
        sequence(theirs, ours, foreign.line.n1, definitive.line.n1)

  private def pair(foreign: Layout, definitive: Layout): List[Divergence] =
    (foreign, definitive) match
      case (Layout.Heading(_, level, theirs*), Layout.Heading(_, level2, ours*)) =>
        if level == level2 then cited(foreign, definitive, List.from(theirs), List.from(ours)) else
          val message: Text =
            t"This heading is of level ${level.show}, and the one it translates of level " +
              t"${level2.show}."

          List(Divergence(foreign.line.n1, definitive.line.n1, message))

      case (Layout.Paragraph(_, theirs*), Layout.Paragraph(_, ours*)) =>
        cited(foreign, definitive, List.from(theirs), List.from(ours))

      case (Layout.BlockQuote(_, theirs*), Layout.BlockQuote(_, ours*)) =>
        sequence(List.from(theirs), List.from(ours), foreign.line.n1, definitive.line.n1)

      case (Layout.BulletList(_, _, theirs*), Layout.BulletList(_, _, ours*)) =>
        items(foreign, definitive, List.from(theirs), List.from(ours))

      case (Layout.OrderedList(_, _, _, _, theirs*), Layout.OrderedList(_, _, _, _, ours*)) =>
        items(foreign, definitive, List.from(theirs), List.from(ours))

      case (Layout.CodeBlock(_, info, content), Layout.CodeBlock(_, info2, content2)) =>
        val table: Boolean = info.head == Tables.language

        if info == info2 && (table || content == content2) then Nil else
          val message: Text =
            t"This fence is not the one it translates: a fence is the same in every language."

          List(Divergence(foreign.line.n1, definitive.line.n1, message))

      case _ =>
        if foreign.ordinal == definitive.ordinal then Nil else
          val message: Text =
            t"This is ${name(foreign)}, where the text it translates has ${name(definitive)}."

          List(Divergence(foreign.line.n1, definitive.line.n1, message))

  // The differences between two runs of blocks: those of each pair, as far as the first pair
  // of different kinds, after which nothing corresponds; and a difference in how many there
  // are, where every pair corresponds.
  private def sequence
    ( theirs:     List[Layout],
      ours:       List[Layout],
      foreign:    Int,
      definitive: Int )
  :   List[Divergence] =

    val pairs: List[(Layout, Layout)] = theirs.zip(ours)

    val matching: List[(Layout, Layout)] =
      pairs.keep { (pair: (Layout, Layout)) => pair(0).ordinal == pair(1).ordinal }

    val found: List[Divergence] = matching.bind(pair(_, _))

    if matching.size < pairs.size then
      found + pairs.skip(matching.size).head.lay(List[Divergence]())(pair(_, _))
    else if theirs.size == ours.size then found
    else
      val line: Int = theirs.at(ours.size.z).or(theirs.last).lay(foreign)(_.line.n1)
      val other: Int = ours.at(theirs.size.z).or(ours.last).lay(definitive)(_.line.n1)

      val message: Text =
        if theirs.size > ours.size then
          val extra: Text = theirs.at(ours.size.z).lay(t"")(name)
          t"This has $extra which the text it translates does not."
        else
          val lacking: Text = ours.at(theirs.size.z).lay(t"")(name)
          t"The text this translates goes on with $lacking, which this lacks."

      found + List(Divergence(line, other, message))

  // What differs in structure between a translation, as its text `source` has it, and the text
  // it says it translates: nothing, while a passage of it is pending or orphaned, since its
  // translation is of other text, and the translator has been told to revise it.
  def findings(source: Text): List[Translation.Finding] =
    val translation: Translation = Translation.parse(source)

    val unsettled: Boolean = translation.passages.exists: (passage: Translation.Passage) =>
      passage.state == Translation.State.Pending || passage.state == Translation.State.Orphaned

    if unsettled then Nil else compared(translation)

  private def compared(translation: Translation): List[Translation.Finding] =
    val foreign: Translation.Projection = translation.foreign
    val definitive: Translation.Projection = translation.definitive
    val theirs: List[Layout] = Parser.parse(foreign.text).children
    val ours: List[Layout] = Parser.parse(definitive.text).children

    sequence(theirs, ours, 1, 1).map: (divergence: Divergence) =>
      Translation.Finding(divergence.tone, foreign.origin(divergence.foreign), divergence.message)

  def check(path: Text, source: Text): List[Block.Diagnostic] =
    Translation.report(path, source, findings(source))
