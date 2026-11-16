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

// A range of a fragment's text named by its first and last few characters — `Mi..es` for a
// run beginning `Mi` and ending `es` — or by a literal run of text, as Amok named them: short
// enough to write in a preamble, stable under edits elsewhere in the fragment; refined, when
// the run occurs more than once, to a `line` of the fragment (from 1) and to the `occurrence`
// wanted (from 1). This is the one place a mark's selection is read and found, so that other
// ways of naming a range can be tried here alone.
object Selection:
  def parse(text: Text): Selection =
    val parts: List[Text] = text.cut(t"..")

    parts match
      case start :: end :: _ => Selection(start, end)
      case _                 => Selection(text, Unset)

  // A mark's selection from its TEL node: its first atom, refined by the `line` and
  // `occurrence` children beneath it.
  def read(node: Tel): Selection =
    val base: Selection = parse(node.atomTexts.to[List].filter(_ != t"").head.or(t""))

    def number(keyword: Text): Optional[Int] =
      node.fields(keyword).to[List].head.let: (child: Tel) => safely(child.primaryAtom.as[Int])

    base.copy(line = number(t"line"), occurrence = number(t"occurrence").or(1))

case class Selection
  ( start: Text, end: Optional[Text], line: Optional[Int] = Unset, occurrence: Int = 1 ):

  // The offsets of the match: where `start` occurs for the `occurrence`th time (on the line,
  // when one is named), to the end of the first `end` after it; or the literal alone.
  def findIn(code: Text): Optional[(Int, Int)] =
    // The offset of the first `part` at or after `from`.
    def seek(part: Text, from: Int): Optional[Int] =
      code.skip(from).offsetOf(part).let(_.n0 + from)

    // The region searched: the whole text, or the named line of it.
    val region: Optional[(Int, Int)] = line.lay((0, code.length)): (line: Int) =>
      val lines: List[Text] = code.cut(t"\n")

      if line < 1 then Unset else lines.at((line - 1).z).let: (text: Text) =>
        val offset: Int = lines.keep(line - 1).fold(0)(_ + _.length + 1)
        (offset, offset + text.length)

    region.let: (region: (Int, Int)) =>
      def nth(remaining: Int, from: Int): Optional[Int] =
        seek(start, from).let: (index: Int) =>
          if index + start.length > region(1) then Unset
          else if remaining <= 1 then index
          else nth(remaining - 1, index + 1)

      nth(occurrence, region(0)).let: (from: Int) =>
        end.lay((from, from + start.length): Optional[(Int, Int)]): (end: Text) =>
          seek(end, from + start.length).let: (to: Int) =>
            if to + end.length > region(1) then Unset else (from, to + end.length)
