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
import pyrocosm.{Block, Inline, Markdowns}

// A presentation's transcript: what is said while it is shown, a Markdown document of its own,
// given with the presentation when it is shown, so that there may be one for each language it
// is read in. Where the words which bring a state of the presentation are
// said, they are a link to the state's `id`, behind the sigil a cue is known by:
//
//   …and the compiler will work out that the return type is [this](@reveal-inferred-type).
//
// The state arrives as the first of the link's words is spoken. What is said is the
// transcript's prose — its paragraphs, and those of its lists and quotations — and not its
// headings, which are the author's, to find a way about it by; a cue is in what is said.
object Transcript:
  val sigil: Text = t"@"

  // A cue: the state it names, where its words begin in the text of what is said, and the
  // line of the transcript its paragraph begins on.
  case class Cue(id: Text, offset: Int, line: Int)

  // A link's destination as a cue's: `@id`, with the cue's line kept beside it.
  private def marked(destination: Text, line: Int): Inline.Destination =
    Inline.Destination.Local(destination, line.show)

  private def cue(destination: Inline.Destination, offset: Int): Optional[Cue] = destination match
    case Inline.Destination.Local(path, line: Text) if path.starts(sigil) =>
      Cue(path.skip(sigil.length), offset, safely(line.as[Int]).or(1))

    case _ =>
      Unset

  // The runs of phrasing which are said, in order.
  private def said(blocks: List[Block]): List[List[Inline]] = blocks.bind:
    case Block.Paragraph(content) => List(content)
    case Block.Listing(_, items)  => items.bind { (item: Block.Item) => said(item.content) }
    case Block.Quotation(content) => said(content)
    case Block.Group(content)     => said(content)
    case _                        => Nil

  // A run of phrasing after `text`, as text, with the cues met in it before `cues`.
  private def phrasing(inlines: List[Inline], text: Text, cues: List[Cue]): (Text, List[Cue]) =
    inlines match
      case head :: tail =>
        val (text2, cues2) = head match
          case Inline.Link(destination, content) =>
            phrasing(content, text, cue(destination, text.length).lay(cues)(_ :: cues))

          case Inline.Phrase(content)       => phrasing(content, text, cues)
          case Inline.Emphasis(content)     => phrasing(content, text, cues)
          case Inline.Strong(content)       => phrasing(content, text, cues)
          case Inline.Toned(_, content)     => phrasing(content, text, cues)
          case Inline.Annotated(content, _) => phrasing(content, text, cues)
          case other                        => (t"$text${other.plain}", cues)

        phrasing(tail, text2, cues2)

      case _ =>
        (text, cues)

  // A transcript read from its Markdown: what is shown of it, a cue's words as any others; the
  // text of what is said, a blank line between its paragraphs; the cues in it, in order; and
  // those which are not in what is said — in a heading, say — and so cue nothing.
  def read(markdown: Text): Transcript =
    val parsed: Markdown of Layout = Parser.parse(markdown)

    @scala.caps.unsafe.untrackedCaptures
    var met: List[Cue] = Nil

    val marking: Markdowns.Hooks = new Markdowns.Hooks:
      override def link(destination: Text, content: List[Inline], line: Int): Inline =
        if !destination.starts(sigil) then super.link(destination, content, line) else
          met = Cue(destination.skip(sigil.length), 0, line) :: met
          Inline.Link(marked(destination, line), content)

    val showing: Markdowns.Hooks = new Markdowns.Hooks:
      override def link(destination: Text, content: List[Inline], line: Int): Inline =
        if !destination.starts(sigil) then super.link(destination, content, line)
        else Inline.Phrase(content)

    def recur(runs: List[List[Inline]], text: Text, cues: List[Cue]): (Text, List[Cue]) =
      runs match
        case head :: tail =>
          val (text2, cues2) = phrasing(head, text, cues)
          recur(tail, if tail.nil then text2 else t"$text2\n\n", cues2)

        case _ =>
          (text, cues.reverse)

    val (text, cues) = recur(said(Markdowns.convert(parsed, marking)), t"", Nil)

    // The cues met which are not among those said, each once for each time it is not.
    def unsaid(met: List[Cue], said: List[Cue]): List[Cue] = met match
      case head :: tail =>
        val place: Optional[Ordinal] = said.where { (cue: Cue) => cue.id == head.id && cue.line == head.line }

        place.lay(head :: unsaid(tail, said)): (place: Ordinal) =>
          unsaid(tail, said.keep(place.n0) + said.skip(place.n0 + 1))

      case _ =>
        Nil

    Transcript(Markdowns.convert(parsed, showing), text, cues, unsaid(met.reverse, cues))

case class Transcript
  ( blocks: List[Block],
    text:   Text,
    cues:   List[Transcript.Cue],
    stray:  List[Transcript.Cue] = Nil )
