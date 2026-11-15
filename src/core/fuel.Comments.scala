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
      def recur(tokens: List[Token], acc: List[Token]): List[Token] =
        tokens match
          case token :: rest =>
            val text: Text = token.text

            if open then
              text.offsetOf(t"*/").lay(recur(rest, comment(token) :: acc)): (close: Ordinal) =>
                open = false
                val (inside, after) = (text.keep(close.n0 + 2), text.skip(close.n0 + 2))
                val remaining: List[Token] =
                  if after == t"" then rest else token.copy(text = after) :: rest

                recur(remaining, comment(token.copy(text = inside)) :: acc)

            else if token.accent != Token.Accent.Unparsed then recur(rest, token :: acc)
            else
              val line: Int = text.offsetOf(t"//").lay(-1)(_.n0)
              val block: Int = text.offsetOf(t"/*").lay(-1)(_.n0)

              if line >= 0 && (block < 0 || line < block) then
                // The rest of the line is the comment, whatever its tokens.
                val before: List[Token] =
                  if line == 0 then Nil else List(token.copy(text = text.keep(line)))

                val from: Token = comment(token.copy(text = text.skip(line)))
                acc.reverse + before + (from :: rest.map(comment))
              else if block >= 0 then
                val before: List[Token] =
                  if block == 0 then Nil else List(token.copy(text = text.keep(block)))

                open = true
                val opened: Token = token.copy(text = text.skip(block))
                recur(opened :: rest, before.reverse + acc)
              else recur(rest, token :: acc)

          case _ =>
            acc.reverse

      Block.Line(recur(line.tokens, Nil))

  private def comment(token: Token): Token =
    token.copy(accent = Token.Accent.Comment, link = Unset, chained = false)

// A substitution replaces a range of a fragment's text, found as a mark's is, with other text
// shown in its place, in the `Substitute` accent: the code compiled as written, and reads as the
// author means it to. A range across lines collapses them to one; a mark on the range is lost.
object Substitutions:
  def apply(lines: List[Block.Line], code: Text, substitutions: List[Preamble.Substitution])
  :   List[Block.Line] =

    substitutions.fold(lines): (lines: List[Block.Line], substitution: Preamble.Substitution) =>
      substitution.selection.findIn(code).lay(lines): (range: (Int, Int)) =>
        replace(lines, range(0), range(1), substitution.text)

  // The newline between lines, as a token, so that the lines are one sequence of tokens whose
  // offsets are the text's.
  private val newline: Token = Token.plain(t"\n")

  private def replace(lines: List[Block.Line], from: Int, to: Int, text: Text): List[Block.Line] =
    val flat: List[Token] =
      lines.indexed.bind: (line: Block.Line, index: Ordinal) =>
        if index.n0 == 0 then line.tokens else newline :: line.tokens

    val substitute: Token = Token(text, Token.Accent.Substitute)

    // Each token kept, cut, or dropped, by where it lies against the range; the substitute is
    // put where the range begins.
    def recur(tokens: List[Token], offset: Int, placed: Boolean, acc: List[Token]): List[Token] =
      tokens match
        case token :: rest =>
          val end: Int = offset + token.text.length

          if end <= from || offset >= to then recur(rest, end, placed, token :: acc)
          else
            val before: List[Token] =
              if offset < from then List(token.copy(text = token.text.keep(from - offset)))
              else Nil

            val after: List[Token] =
              if end > to then List(token.copy(text = token.text.skip(to - offset))) else Nil

            val middle: List[Token] = if placed then Nil else List(substitute)
            recur(rest, end, true, (before + middle + after).reverse + acc)

        case _ =>
          acc.reverse

    val replaced: List[Token] = recur(flat, 0, false, Nil)

    // Back into lines, at the newlines which survived.
    def split(tokens: List[Token], current: List[Token], acc: List[Block.Line])
    :   List[Block.Line] =

      tokens match
        case token :: rest =>
          if token eq newline then split(rest, Nil, Block.Line(current.reverse) :: acc)
          else split(rest, token :: current, acc)

        case _ =>
          (Block.Line(current.reverse) :: acc).reverse

    split(replaced, Nil, Nil)
