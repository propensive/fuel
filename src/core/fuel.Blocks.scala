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

// A Markdown document as Pyrocosm blocks, node for node: Pyrocosm's own conversion
// (`Presentable`'s `markdown`), with the fenced code blocks dispatched to the embeddings in
// scope. Inline HTML is dropped, as the parser already comments it out; an inline image becomes
// a link to it. To be offered upstream once it has settled here.
object Blocks:
  def of(markdown: Markdown of Layout, embeddings: List[Embedding])
  :   List[Block] =

    all(markdown.children, embeddings)

  // The first level-one heading's text, which is what a document is called.
  def title(blocks: List[Block]): Optional[Text] = blocks match
    case Block.Heading(1, content) :: _ => plain(content)
    case _ :: tail                      => title(tail)
    case _                              => Unset

  // The text of a run of phrasing, with no styling.
  def plain(inlines: List[Inline]): Text = inlines.map(plainInline).join(t"")

  private def plainInline(inline: Inline): Text = inline match
    case Inline.Textual(text)       => text
    case Inline.Phrase(content)     => plain(content)
    case Inline.Emphasis(content)   => plain(content)
    case Inline.Strong(content)     => plain(content)
    case Inline.Toned(_, content)   => plain(content)
    case Inline.Link(_, content)    => plain(content)
    case Inline.Code(_, tokens)     => tokens.map(_.text).join(t"")
    case _                          => t""

  private def all(nodes: List[Layout], embeddings: List[Embedding]): List[Block] = nodes match
    case head :: tail => layout(head, embeddings) + all(tail, embeddings)
    case _            => Nil

  private def phrasing(nodes: List[Prose]): List[Inline] = nodes.map(prose)

  // A run of Markdown phrasing — a frame's label, say — as inlines: the first paragraph the
  // text parses to, or the text itself when it has none.
  def inline(text: Text): List[Inline] =
    def first(nodes: List[Layout]): List[Inline] = nodes match
      case Layout.Paragraph(_, prose*) :: _ => phrasing(List.from(prose))
      case _ :: tail                        => first(tail)
      case _                                => Inline.text(text)

    first(Parser.parse(text).children)

  private def prose(node: Prose): Inline = node match
    case Prose.Textual(text)       => Inline.Textual(text)
    case Prose.Emphasis(children*) => Inline.Emphasis(phrasing(List.from(children)))
    case Prose.Strong(children*)   => Inline.Strong(phrasing(List.from(children)))
    case Prose.Code(code)          => Inline.Code(Language.Plain, List(Token.plain(code)))
    case Prose.Softbreak           => Inline.Textual(t" ")
    case Prose.Linebreak           => Inline.Break()
    case Prose.HtmlInline(_)       => Inline.Textual(t"")

    case Prose.Link(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(List.from(content)))

    case Prose.Image(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(List.from(content)))

  // The first embedding to answer a fence, else the plain rendering.
  private def fence(info: List[Text], content: Text, line: Int, embeddings: List[Embedding])
  :   List[Block] =

    embeddings match
      case head :: tail => head.embed(info, content, line).or(fence(info, content, line, tail))
      case _            => List(Embedding.plain(info, content, line))

  private def layout(node: Layout, embeddings: List[Embedding]): List[Block] = node match
    case Layout.Paragraph(_, prose*) =>
      List(Block.Paragraph(phrasing(List.from(prose))))

    case Layout.Heading(_, level, prose*) =>
      List(Block.Heading(level, phrasing(List.from(prose))))

    case Layout.BlockQuote(_, layouts*) =>
      List(Block.Quotation(all(List.from(layouts), embeddings)))

    case Layout.ThematicBreak(_) =>
      List(Block.Rule())

    case Layout.HtmlBlock(_, html) =>
      List(Block.Code(Language(t"html"), Embedding.lines(html)))

    case Layout.CodeBlock(line, info, content) =>
      fence(info, content, line.n0, embeddings)

    case Layout.BulletList(_, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings)) })

      List(Block.Listing(false, listed))

    case Layout.OrderedList(_, _, _, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings)) })

      List(Block.Listing(true, listed))
