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

import codepages.utf8Codepage
import dysasymptotics.linearSize
import pyrocosm.{Block, Inline}

// The TEL preamble a fenced block may begin with, before a line holding `##` alone, when its
// info string marks it (` ```scala fuel `). Amok's schema, walked by hand (as flare reads its
// configuration), so that no derived codec is needed:
//
//   context term                  # top (the default), term or type: how the fragment is wrapped
//   id setup                      # a name other fragments may follow
//   follows setup                 # the fragments whose text precedes this one when it compiles
//   errors show                   # fail (the default): an error fails the compile;
//                                 # show: errors are expected and shown; ignore: not compiled
//   language java                 # how the fragment is highlighted: the fence's language
//                                 # unless said
//   highlight Mi..es  the caption # a range to mark, with its caption in the remaining atoms,
//   error     Lit..ond            # as `highlight`, `error`, `caution` or `param`
//     caption  or beneath it
//     line 3                      # the range sought on one line of the fragment (from 1)
//     occurrence 2                # and the nth time it occurs
//   frame                         # a frame of an animation: several in order, each compiled
//     code                        # on its own, with the code after `##` the last frame if
//         val x = 1               # there is any; `code` holds the frame's text, indented
//     label the *first* step      # beneath it (or on the line, for a one-liner), and must be
//     hold 2                      # there; the rest may be: a label (Markdown phrasing), the
//     at 12.5                     # seconds the frame holds when played, its time on the
//     language java               # page's clock, its own language, and marks of its own
//     highlight x  the variable
//   autoplay                      # play when shown; `loop` to start again at the end
//   restart                       # in a session, a fresh one begins at this fragment
//   typecheck                     # how far the compiler goes: `parse` (syntax alone),
//                                 # `typecheck` (the default when none is said) or `compile`
//                                 # (every phase, which is where capture checking reports)
//   feature experimental.captureChecking   # a language import, this fragment onward
//
// A fragment which says a mode is checked by fuel but never run by a checker which runs
// fragments, so `typecheck` on its own marks one which opens a socket or a browser.
object Preamble:
  // How far the compiler takes a fragment.
  enum Mode:
    case Parse, Typecheck, Compile

  case class Mark(style: Block.Note.Style, selection: Selection, caption: Optional[Text])

  // A frame of an animation: its code, absent when the `code` child is missing (which is a
  // problem, reported where the frame is), and what else is said of it.
  case class Frame
    ( code:     Optional[Text],
      label:    List[Inline]     = Nil,
      hold:     Optional[Double] = Unset,
      at:       Optional[Double] = Unset,
      language: Optional[Text]   = Unset,
      marks:    List[Mark]       = Nil )

  enum Errors:
    case Fail, Show, Ignore

  val empty: Preamble = Preamble(Unset, Unset, Nil, Errors.Fail, Nil, Nil, false, false, Unset)

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
      raw.location.let: (location: Block.Location) =>
        location.copy(line = location.line.let(_ + shift))

    val excerpt: List[Block] = raw.excerpt.map:
      case code: Block.Code => code.copy(start = code.start.let(_ + shift))
      case other            => other

    raw.copy(location = location, excerpt = excerpt)

  private def read(text: Text): Preamble raises Tel.Error = of(headed(text).read[Tel])

  // A preamble read from a TEL node whose children are its keywords: the document above a
  // fence's `##`, or anything else which says what a fragment's preamble says — a slide's code.
  // A keyword the preamble does not know is left for the node's owner to read.
  def of(tel: Tel): Preamble =
    locally:
      def atoms(keyword: Text): List[Text] =
        tel.fields(keyword).to[List].map(_.primaryAtom).filter(_ != t"")

      // The text of a node's first child by a keyword: its atoms, inline then the indented
      // source, joined, so that a one-liner serves as well as an indented block.
      def child(node: Tel, keyword: Text): Optional[Text] =
        node.fields(keyword).to[List].head.let(_.atomTexts.to[List].filter(_ != t"").join(t" "))

      def number(text: Optional[Text]): Optional[Double] =
        text.let: (text: Text) => safely(text.as[Double])

      // The marks beneath a node — the preamble's own, or a frame's — each with its caption
      // in its remaining atoms or a `caption` beneath it, and its selection read whole.
      def marks(node: Tel): List[Mark] =
        def of(keyword: Text, style: Block.Note.Style): List[Mark] =
          node.fields(keyword).to[List].bind[List[Mark], Mark, List[Mark]]: (mark: Tel) =>
            val texts: List[Text] = mark.atomTexts.to[List].filter(_ != t"")

            texts match
              case _ :: rest =>
                val inline: Optional[Text] = if rest.nil then Unset else rest.join(t" ")

                val beneath: Optional[Text] =
                  mark.fields(t"caption").to[List].head.let(_.atomTexts.to[List].join(t" "))

                List(Mark(style, Selection.read(mark), beneath.or(inline)))

              case _ =>
                Nil

        of(t"highlight", Block.Note.Style.Highlight) +
          of(t"error", Block.Note.Style.Erroneous) +
          of(t"caution", Block.Note.Style.Caution) +
          of(t"param", Block.Note.Style.Param)

      // A frame's code is its `code` child's text; what else is said of it is beneath it.
      val frames: List[Frame] =
        tel.fields(t"frame").to[List].map: (node: Tel) =>
          Frame
            ( node.fields(t"code").to[List].head.let(_.atomTexts.to[List].join(t" ")),
              child(node, t"label").lay(Nil)(Blocks.inline(_)),
              number(child(node, t"hold")),
              number(child(node, t"at")),
              child(node, t"language"),
              marks(node) )

      def flag(keyword: Text): Boolean = !tel.fields(keyword).to[List].nil

      val mode: Optional[Mode] =
        if flag(t"parse") then Mode.Parse
        else if flag(t"compile") then Mode.Compile
        else if flag(t"typecheck") then Mode.Typecheck
        else Unset

      val errors: Errors = atoms(t"errors").head match
        case t"show"   => Errors.Show
        case t"ignore" => Errors.Ignore
        case _         => Errors.Fail

      Preamble
        ( atoms(t"context").head,
          atoms(t"id").head,
          atoms(t"follows"),
          errors,
          marks(tel),
          frames,
          flag(t"autoplay"),
          flag(t"loop"),
          atoms(t"language").head,
          mode,
          flag(t"restart"),
          atoms(t"feature") )

case class Preamble
  ( context:  Optional[Text],
    id:       Optional[Text],
    follows:  List[Text],
    errors:   Preamble.Errors,
    marks:    List[Preamble.Mark],
    frames:   List[Preamble.Frame] = Nil,
    autoplay: Boolean              = false,
    loop:     Boolean              = false,
    language: Optional[Text]       = Unset,
    mode:     Optional[Preamble.Mode] = Unset,
    restart:  Boolean              = false,
    features: List[Text]           = Nil ):

  def scalaContext: Optional[harlequin.Scala.Context] = context match
    case t"term" => harlequin.Scala.Context.Term
    case t"type" => harlequin.Scala.Context.Type
    case _       => Unset
