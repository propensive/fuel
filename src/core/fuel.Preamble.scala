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

import codepages.utf8Codepage
import dysasymptotics.linearSize
import pyrocosm.Block

// The TEL preamble a fenced block may begin with, before a line holding `##` alone, when its
// info string marks it (` ```scala fuel `). Amok's schema, walked by hand (as flair reads its
// configuration), so that no derived codec is needed:
//
//   context term                  # top (the default), term or type: how the fragment is wrapped
//   id setup                      # a name other fragments may follow
//   follows setup                 # the fragments whose text precedes this one when it compiles
//   errors show                   # fail (the default): an error fails the compile;
//                                 # show: errors are expected and shown; ignore: not compiled
//   highlight Mi..es  the caption # a range to mark, with its caption in the remaining atoms,
//   error     Lit..ond            # as `highlight`, `error`, `caution` or `param`
//     caption  or beneath it
object Preamble:
  case class Mark(style: Block.Note.Style, selection: Selection, caption: Optional[Text])

  enum Errors:
    case Fail, Show, Ignore

  val empty: Preamble = Preamble(Unset, Unset, Nil, Errors.Fail, Nil)

  // The fence's content as its preamble and its code: the preamble is everything before the
  // first line holding `##` alone, and `Unset` when there is no such line.
  def split(content: Text): (Optional[Text], Text) =
    val lines: List[Text] = content.cut(t"\n")
    val index: Optional[Ordinal] = lines.where(_ == t"##")

    index.lay((Unset, content)): (index: Ordinal) =>
      (lines.keep(index.n0).join(t"\n"), lines.skip(index.n0 + 1).join(t"\n"))

  // A preamble parsed from its text, or `Unset` if it is not TEL. The `tel 1.0` line is
  // supplied if absent, so a fence need not repeat it.
  def parse(text: Text): Optional[Preamble] =
    val headed: Text = if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"

    safely(headed.read[Tel]).let: (tel: Tel) =>
      def atoms(keyword: Text): List[Text] =
        tel.fields(keyword).to[List].map(_.primaryAtom).filter(_ != t"")

      def marks(keyword: Text, style: Block.Note.Style): List[Mark] =
        tel.fields(keyword).to[List].bind[List[Mark], Mark, List[Mark]]: (node: Tel) =>
          val texts: List[Text] = node.atomTexts.to[List].filter(_ != t"")

          texts match
            case selection :: rest =>
              val inline: Optional[Text] = if rest.nil then Unset else rest.join(t" ")

              val child: Optional[Text] =
                node.fields(t"caption").to[List].prim.let(_.atomTexts.to[List].join(t" "))

              List(Mark(style, Selection.parse(selection), child.or(inline)))

            case _ =>
              Nil

      val errors: Errors = atoms(t"errors").prim match
        case t"show"   => Errors.Show
        case t"ignore" => Errors.Ignore
        case _         => Errors.Fail

      Preamble
        ( atoms(t"context").prim,
          atoms(t"id").prim,
          atoms(t"follows"),
          errors,
          marks(t"highlight", Block.Note.Style.Highlight) +
            marks(t"error", Block.Note.Style.Erroneous) +
            marks(t"caution", Block.Note.Style.Caution) +
            marks(t"param", Block.Note.Style.Param) )

case class Preamble
  ( context: Optional[Text],
    id:      Optional[Text],
    follows: List[Text],
    errors:  Preamble.Errors,
    marks:   List[Preamble.Mark] ):

  def scalaContext: Optional[harlequin.Scala.Context] = context match
    case t"term" => harlequin.Scala.Context.Term
    case t"type" => harlequin.Scala.Context.Type
    case _       => Unset
