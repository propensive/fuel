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
//   frame                         # a frame of an animation, its code indented beneath (or
//       val x = 1                 # on the line, for a one-liner); several in order, each
//   frame                         # compiled on its own, with the code after `##` the last
//       val x = 2                 # frame if there is any
//   autoplay                      # play when shown; `loop` to start again at the end
object Preamble:
  case class Mark(style: Block.Note.Style, selection: Selection, caption: Optional[Text])

  enum Errors:
    case Fail, Show, Ignore

  val empty: Preamble = Preamble(Unset, Unset, Nil, Errors.Fail, Nil, Nil, false, false)

  // The fence's content as its preamble and its code: the preamble is everything before the
  // first line holding `##` alone, and `Unset` when there is no such line.
  def split(content: Text): (Optional[Text], Text) =
    val lines: List[Text] = content.cut(t"\n")
    val index: Optional[Ordinal] = lines.where(_ == t"##")

    index.lay((Unset, content)): (index: Ordinal) =>
      (lines.keep(index.n0).join(t"\n"), lines.skip(index.n0 + 1).join(t"\n"))

  // The `tel 1.0` line a preamble need not repeat, supplied when absent.
  private def headed(text: Text): Text =
    if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"

  // The lines `headed` adds before the preamble's own.
  private def added(text: Text): Int = if text.trim.starts(t"tel ") then 0 else 2

  // A preamble parsed from its text, or the TEL error if it is not TEL.
  def parse(text: Text): Preamble | Tel.Error =
    import strategies.throwUnsafely
    try read(text) catch case error: Tel.Error => error

  // A preamble's TEL error as a diagnostic at its place in the file: the preamble's text is
  // the excerpt, and its line is counted from `line`, where it begins in the file (from zero).
  def diagnostic(error: Tel.Error, path: Text, text: Text, line: Int): Block.Diagnostic =
    val shift: Int = line - added(text)
    val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))

    val location: Optional[Block.Location] =
      raw.location.let { (location: Block.Location) => location.copy(line = location.line.let(_ + shift)) }

    val excerpt: List[Block] = raw.excerpt.map:
      case code: Block.Code => code.copy(start = code.start.let(_ + shift))
      case other            => other

    raw.copy(location = location, excerpt = excerpt)

  private def read(text: Text): Preamble raises Tel.Error =
    val tel: Tel = headed(text).read[Tel]
    locally:
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

      // A frame's code: its indented literal, which follows any inline atoms in the atom
      // sequence, so joining all of them serves a one-line frame too.
      val frames: List[Text] =
        tel.fields(t"frame").to[List].map { (node: Tel) => node.atomTexts.to[List].join(t" ") }

      def flag(keyword: Text): Boolean = !tel.fields(keyword).to[List].nil

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
            marks(t"param", Block.Note.Style.Param),
          frames,
          flag(t"autoplay"),
          flag(t"loop") )

case class Preamble
  ( context:  Optional[Text],
    id:       Optional[Text],
    follows:  List[Text],
    errors:   Preamble.Errors,
    marks:    List[Preamble.Mark],
    frames:   List[Text]    = Nil,
    autoplay: Boolean       = false,
    loop:     Boolean       = false ):

  def scalaContext: Optional[harlequin.Scala.Context] = context match
    case t"term" => harlequin.Scala.Context.Term
    case t"type" => harlequin.Scala.Context.Type
    case _       => Unset
