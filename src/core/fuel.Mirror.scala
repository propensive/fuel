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

import scala.collection.immutable as sci

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

  // The code a run of prose names and the places it leads to, in order of their text.
  private def references(prose: sci.Seq[Prose]): sci.List[String] =
    def recur(node: Prose): sci.List[String] = node match
      case Prose.Code(code)                  => sci.List(s"`${code.s}`")
      case Prose.Emphasis(children*)         => children.toList.flatMap(recur)
      case Prose.Strong(children*)           => children.toList.flatMap(recur)
      case Prose.Link(destination, _, children*)  => destination.s :: children.toList.flatMap(recur)
      case Prose.Image(destination, _, children*) => destination.s :: children.toList.flatMap(recur)
      case _                                 => sci.Nil

    prose.toList.flatMap(recur).sorted

  private def cited
    ( foreign:    Layout,
      definitive: Layout,
      theirs:     sci.Seq[Prose],
      ours:       sci.Seq[Prose] )
  :   sci.List[Divergence] =

    val expected: sci.List[String] = references(ours)
    val found: sci.List[String] = references(theirs)

    if expected == found then sci.Nil else
      val missing: sci.List[String] = expected.diff(found)
      val extra: sci.List[String] = found.diff(expected)

      val lacks: String =
        if missing.isEmpty then "" else s" It lacks ${missing.mkString(", ")}."

      val adds: String =
        if extra.isEmpty then "" else s" It has ${extra.mkString(", ")}, which the other does not."

      val message: Text =
        Text(s"This does not name the code, or lead to the places, that the text it " +
          s"translates does.$lacks$adds")

      sci.List(Divergence(foreign.line.n1, definitive.line.n1, message, Tone.Warning))

  private def items
    ( foreign:    Layout,
      definitive: Layout,
      theirs:     sci.Seq[List[Layout]],
      ours:       sci.Seq[List[Layout]] )
  :   sci.List[Divergence] =

    if theirs.size != ours.size then
      val message: Text =
        t"This list has ${theirs.size.show} items, and the one it translates has ${ours.size.show}."

      sci.List(Divergence(foreign.line.n1, definitive.line.n1, message))
    else
      theirs.toList.zip(ours.toList).flatMap: (theirs, ours) =>
        sequence(theirs.stdlib, ours.stdlib, foreign.line.n1, definitive.line.n1)

  private def pair(foreign: Layout, definitive: Layout): sci.List[Divergence] =
    (foreign, definitive) match
      case (Layout.Heading(_, level, theirs*), Layout.Heading(_, level2, ours*)) =>
        if level == level2 then cited(foreign, definitive, theirs, ours) else
          val message: Text =
            t"This heading is of level ${level.show}, and the one it translates of level " +
              t"${level2.show}."

          sci.List(Divergence(foreign.line.n1, definitive.line.n1, message))

      case (Layout.Paragraph(_, theirs*), Layout.Paragraph(_, ours*)) =>
        cited(foreign, definitive, theirs, ours)

      case (Layout.BlockQuote(_, theirs*), Layout.BlockQuote(_, ours*)) =>
        sequence(theirs.toList, ours.toList, foreign.line.n1, definitive.line.n1)

      case (Layout.BulletList(_, _, theirs*), Layout.BulletList(_, _, ours*)) =>
        items(foreign, definitive, theirs, ours)

      case (Layout.OrderedList(_, _, _, _, theirs*), Layout.OrderedList(_, _, _, _, ours*)) =>
        items(foreign, definitive, theirs, ours)

      case (Layout.CodeBlock(_, info, content), Layout.CodeBlock(_, info2, content2)) =>
        val table: Boolean = info.head == Tables.language

        if info == info2 && (table || content == content2) then sci.Nil else
          val message: Text =
            t"This fence is not the one it translates: a fence is the same in every language."

          sci.List(Divergence(foreign.line.n1, definitive.line.n1, message))

      case _ =>
        if foreign.ordinal == definitive.ordinal then sci.Nil else
          val message: Text =
            t"This is ${name(foreign)}, where the text it translates has ${name(definitive)}."

          sci.List(Divergence(foreign.line.n1, definitive.line.n1, message))

  // The differences between two runs of blocks: those of each pair, as far as the first pair
  // of different kinds, after which nothing corresponds; and a difference in how many there
  // are, where every pair corresponds.
  private def sequence
    ( theirs:     sci.List[Layout],
      ours:       sci.List[Layout],
      foreign:    Int,
      definitive: Int )
  :   sci.List[Divergence] =

    val pairs: sci.List[(Layout, Layout)] = theirs.zip(ours)

    val matching: sci.List[(Layout, Layout)] =
      pairs.takeWhile { (left, right) => left.ordinal == right.ordinal }

    val found: sci.List[Divergence] = matching.flatMap(pair(_, _))

    if matching.size < pairs.size then
      val (left, right) = pairs(matching.size)
      found ++ pair(left, right)
    else if theirs.size == ours.size then found
    else
      val line: Int =
        theirs.lift(ours.size).orElse(theirs.lastOption).map(_.line.n1).getOrElse(foreign)

      val other: Int =
        ours.lift(theirs.size).orElse(ours.lastOption).map(_.line.n1).getOrElse(definitive)

      val message: Text =
        if theirs.size > ours.size
        then t"This has ${name(theirs(ours.size))} which the text it translates does not."
        else t"The text this translates goes on with ${name(ours(theirs.size))}, which this lacks."

      found :+ Divergence(line, other, message)

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
    val theirs: sci.List[Layout] = Parser.parse(foreign.text).children.stdlib
    val ours: sci.List[Layout] = Parser.parse(definitive.text).children.stdlib

    List.from:
      sequence(theirs, ours, 1, 1).map: (divergence: Divergence) =>
        Translation.Finding(divergence.tone, foreign.origin(divergence.foreign), divergence.message)

  def check(path: Text, source: Text): List[Block.Diagnostic] =
    Translation.report(path, source, findings(source))
