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

import soundness.{Language as _, Token as _, *}

import pyrocosm.{Block, Inline, Language, Token}
import pyrocosm.Notices

// The lines around a problem, for its diagnostic to show: the line it is on and one either
// side, numbered as the file numbers them, with the problem's span underlined — a Scala source
// highlighted, any other text plain, since nothing here tokenises Markdown. Spans are given as
// the model has them: a line (from 1) with a column (from 1) and a length where they are known,
// else the whole line; or, for Scala, as character offsets.
object Excerpts:
  private def plainLines(text: Text): List[Block.Line] =
    text.cut(t"\n").map { (line: Text) => Block.Line(List(Token.plain(line))) }

  private def scalaLines(text: Text): List[Block.Line] =
    safely(Notices.lines(text)).or(plainLines(text))

  private def excerpt(language: Language, lines: List[Block.Line], note: Block.Note): List[Block] =
    if lines.nil || note.line < 0 || note.line >= lines.stdlib.size then Nil
    else List(Block.Code.excerpt(language, lines, List(note), 1))

  // A line of any text, with a span of it underlined, or all of it where no column is known.
  def plain
    ( text:    Text,
      line:    Int,
      style:   Block.Note.Style,
      column:  Optional[Int] = Unset,
      length:  Optional[Int] = Unset,
      caption: List[Inline] = Nil )
  :   List[Block] =

    val lines: List[Block.Line] = plainLines(text)
    val index: Int = line - 1
    val width: Int = text.cut(t"\n").stdlib.lift(index).map(_.length).getOrElse(0)
    val start: Int = column.let(_ - 1).or(0).max(0).min(width)
    val end: Int = length.let(start + _).or(width).max(start).min(width)
    excerpt(Language.Markdown, lines, Block.Note(index, start, end, style, caption))

  // A line of a text in which `target` appears, that occurrence underlined; the whole line
  // where it does not.
  def plainAt(text: Text, line: Int, target: Text, style: Block.Note.Style): List[Block] =
    val at: Int = text.cut(t"\n").stdlib.lift(line - 1).map(_.s.indexOf(target.s)).getOrElse(-1)
    if at < 0 || target == t"" then plain(text, line, style) else plain(text, line, style, at + 1, target.length)

  // A Scala source, highlighted, with the characters from one offset to another underlined; the
  // location of the span's start is given back with the excerpt, for the diagnostic's heading.
  def scala(text: Text, from: Int, to: Int, style: Block.Note.Style): (Optional[(Int, Int)], List[Block]) =
    val note: Block.Note = Notes.of(text, from, to, style)
    ((note.line + 1, note.start + 1), excerpt(Language.Scala, scalaLines(text), note))
