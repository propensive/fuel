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

import pyrocosm.{Block, Token}

// Harlequin leaves a comment as it leaves the spaces between tokens, unparsed, so the comments
// are found here, in a fragment's lines: from a `//` to the end of its line, and from a `/*`
// to the `*/` which closes it, on that line or a later one. Only an unparsed token is looked
// in, so a `//` within a string is not a comment. A comment's tokens take the `Comment`
// accent, and a token a comment begins or ends within is cut at the boundary.
object Comments:
  def mark(lines: List[Block.Line]): List[Block.Line] =
    var open: Boolean = false

    lines.map: (line: Block.Line) =>
      // The line's tokens, each cut where a comment begins or ends within it.
      def recur(tokens: scala.List[Token], acc: scala.List[Token]): scala.List[Token] =
        tokens match
          case scala.Nil => acc.reverse

          case scala.::(token, rest) =>
            val text: String = token.text.s

            if open then
              val close: Int = text.indexOf("*/")

              if close < 0 then recur(rest, comment(token) :: acc)
              else
                open = false
                val (inside, after) = (text.substring(0, close + 2).nn.tt, text.substring(close + 2).nn.tt)
                val remaining: scala.List[Token] = if after == t"" then rest else token.copy(text = after) :: rest
                recur(remaining, comment(token.copy(text = inside)) :: acc)

            else if token.accent != Token.Accent.Unparsed then recur(rest, token :: acc)
            else
              val line: Int = text.indexOf("//")
              val block: Int = text.indexOf("/*")

              if line >= 0 && (block < 0 || line < block) then
                // The rest of the line is the comment, whatever its tokens.
                val before: scala.List[Token] =
                  if line == 0 then scala.Nil else scala.List(token.copy(text = text.substring(0, line).nn.tt))

                val from: Token = comment(token.copy(text = text.substring(line).nn.tt))
                (acc.reverse ++ before ++ (from :: rest.map(comment)))
              else if block >= 0 then
                val before: scala.List[Token] =
                  if block == 0 then scala.Nil else scala.List(token.copy(text = text.substring(0, block).nn.tt))

                open = true
                val opened: Token = token.copy(text = text.substring(block).nn.tt)
                recur(opened :: rest, before.reverse ++ acc)
              else recur(rest, token :: acc)

      Block.Line(List.from(recur(line.tokens.stdlib.toList, scala.Nil)))

  private def comment(token: Token): Token =
    token.copy(accent = Token.Accent.Comment, link = Unset, chained = false)

// A substitution replaces a range of a fragment's text, found as a mark's is, with other text
// shown in its place, in the `Substitute` accent: the code compiled as written, and reads as the
// author means it to. A range across lines collapses them to one; a mark on the range is lost.
object Substitutions:
  def apply(lines: List[Block.Line], code: Text, substitutions: List[Preamble.Substitution])
  :   List[Block.Line] =

    substitutions.stdlib.foldLeft(lines): (lines, substitution) =>
      substitution.selection.findIn(code).lay(lines): (range: (Int, Int)) =>
        replace(lines, range(0), range(1), substitution.text)

  // The newline between lines, as a token, so that the lines are one sequence of tokens whose
  // offsets are the text's.
  private val newline: Token = Token.plain(t"\n")

  private def replace(lines: List[Block.Line], from: Int, to: Int, text: Text): List[Block.Line] =
    val flat: scala.List[Token] =
      lines.stdlib.toList.map(_.tokens.stdlib.toList)
      . reduceOption { (left, right) => left ++ (newline :: right) }.getOrElse(scala.Nil)

    val substitute: Token = Token(text, Token.Accent.Substitute)

    // Each token kept, cut, or dropped, by where it lies against the range; the substitute is
    // put where the range begins.
    def recur(tokens: scala.List[Token], offset: Int, placed: Boolean, acc: scala.List[Token])
    :   scala.List[Token] =

      tokens match
        case scala.Nil => acc.reverse

        case scala.::(token, rest) =>
          val end: Int = offset + token.text.length

          if end <= from || offset >= to then recur(rest, end, placed, token :: acc)
          else
            val before: scala.List[Token] =
              if offset < from then scala.List(token.copy(text = token.text.s.substring(0, from - offset).nn.tt))
              else scala.Nil

            val after: scala.List[Token] =
              if end > to then scala.List(token.copy(text = token.text.s.substring(to - offset).nn.tt))
              else scala.Nil

            val middle: scala.List[Token] = if placed then scala.Nil else scala.List(substitute)
            recur(rest, end, true, (before ++ middle ++ after).reverse ++ acc)

    val replaced: scala.List[Token] = recur(flat, 0, false, scala.Nil)

    // Back into lines, at the newlines which survived.
    def split(tokens: scala.List[Token], current: scala.List[Token], acc: scala.List[Block.Line])
    :   scala.List[Block.Line] =

      tokens match
        case scala.Nil => (Block.Line(List.from(current.reverse)) :: acc).reverse
        case scala.::(token, rest) if token eq newline => split(rest, scala.Nil, Block.Line(List.from(current.reverse)) :: acc)
        case scala.::(token, rest) => split(rest, token :: current, acc)

    List.from(split(replaced, scala.Nil, scala.Nil))
