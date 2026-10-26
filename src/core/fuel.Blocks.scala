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

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Inline, Language, Token}

// A Markdown document as Pyrocosm blocks, node for node: Pyrocosm's own conversion
// (`Presentable`'s `markdown`), with the fenced code blocks dispatched to the embeddings in
// scope. Inline HTML is dropped, as the parser already comments it out; an inline image becomes
// a link to it. To be offered upstream once it has settled here.
object Blocks:
  def of(markdown: Markdown of Layout, embeddings: List[Embedding], linker: Linker = Linker.none)
  :   List[Block] =

    all(markdown.children, embeddings, linker)

  // The first heading's text, of whatever level, which is what a document is called.
  def title(blocks: List[Block]): Optional[Text] = blocks match
    case Block.Heading(_, content, _) :: _ => plain(content)
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
    case Inline.Snippet(_, tokens)     => tokens.map(_.text).join(t"")
    case _                          => t""

  private def all(nodes: List[Layout], embeddings: List[Embedding], linker: Linker): List[Block] = nodes match
    case head :: tail => layout(head, embeddings, linker) + all(tail, embeddings, linker)
    case _            => Nil

  private def phrasing(nodes: List[Prose], linker: Linker = Linker.none): List[Inline] = nodes.map(prose(_, linker))

  // A run of Markdown phrasing — a frame's label, say — as inlines: the first paragraph the
  // text parses to, or the text itself when it has none.
  def inline(text: Text, linker: Linker = Linker.none): List[Inline] =
    def first(nodes: List[Layout]): List[Inline] = nodes match
      case Layout.Paragraph(_, prose*) :: _ => phrasing(List.from(prose), linker)
      case _ :: tail                        => first(tail)
      case _                                => Inline.text(text)

    first(Parser.parse(text).children)

  // Inline code, whose tokens link when the linker says it is a reference: a qualified name a
  // link for each segment.
  private def snippet(code: Text, linker: Linker): Inline =
    Inline.Snippet(Language.Plain, linker.tokens(code))

  // Inline code is a reference when the linker says so: then its one token links.
  private def prose(node: Prose, linker: Linker): Inline = node match
    case Prose.Textual(text)       => Inline.Textual(text)
    case Prose.Emphasis(children*) => Inline.Emphasis(phrasing(List.from(children), linker))
    case Prose.Strong(children*)   => Inline.Strong(phrasing(List.from(children), linker))
    case Prose.Code(code)          => snippet(code, linker)
    case Prose.Softbreak           => Inline.Textual(t" ")
    case Prose.Linebreak           => Inline.Break()
    case Prose.HtmlInline(_)       => Inline.Textual(t"")

    case Prose.Link(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(List.from(content), linker))

    case Prose.Image(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(List.from(content), linker))

  // The first embedding to answer a fence, else the plain rendering.
  def fence(info: List[Text], content: Text, line: Int, embeddings: List[Embedding])
  :   List[Block] =

    embeddings match
      case head :: tail => head.embed(info, content, line).or(fence(info, content, line, tail))
      case _            => List(Embedding.plain(info, content, line))

  // A fenced block of a document: its info string, its content and its line (from zero).
  case class Fence(info: List[Text], content: Text, line: Int)

  // Every fenced block of a document, in the order its conversion meets them: depth first,
  // through block quotes and list items.
  def fences(markdown: Markdown of Layout): List[Fence] = gathered(markdown.children)

  private def gathered(nodes: List[Layout]): List[Fence] = nodes match
    case head :: tail => within(head) + gathered(tail)
    case _            => Nil

  private def within(node: Layout): List[Fence] = node match
    case Layout.CodeBlock(line, info, content)  => List(Fence(info, content, line.n0))
    case Layout.BlockQuote(_, layouts*)         => gathered(List.from(layouts))
    case Layout.BulletList(_, _, items*)        => listed(List.from(items))
    case Layout.OrderedList(_, _, _, _, items*) => listed(List.from(items))
    case _                                      => Nil

  private def listed(items: List[List[Layout]]): List[Fence] = items match
    case head :: tail => gathered(head) + listed(tail)
    case _            => Nil

  // An embedding which answers every fence with its sentinel, numbered in the order the
  // conversion meets them, which is the order of `fences`: a document's prose, with a place for
  // each fence's output.
  def sentinels(): Embedding = new Embedding:
    private var index: Int = 0

    def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
      val sentinel: Block = Output.sentinel(index)
      index += 1
      List(sentinel)

  // A document's prose with each sentinel replaced by its fence's output.
  def fill(blocks: List[Block], outputs: List[List[Block]]): List[Block] = blocks match
    case head :: tail => filled(head, outputs) + fill(tail, outputs)
    case _            => Nil

  private def filled(block: Block, outputs: List[List[Block]]): List[Block] = block match
    case Block.Code(Output.fence, _, _, index: Int, _) =>
      val empty: List[Block] = Nil
      outputs.skip(index).prim.or(empty)

    case Block.Quotation(content) =>
      List(Block.Quotation(fill(content, outputs)))

    case Block.Group(content) =>
      List(Block.Group(fill(content, outputs)))

    case Block.Listing(ordered, items) =>
      val refilled: List[Block.Item] = items.map: (item: Block.Item) =>
        item.copy(content = fill(item.content, outputs))

      List(Block.Listing(ordered, refilled))

    case other =>
      List(other)

  // Inline HTML (an HTML comment the parser read inline, say) is for the author, not the
  // reader: a paragraph of nothing else is no paragraph.
  private def hidden(node: Prose): Boolean = node match
    case Prose.HtmlInline(_) => true
    case Prose.Softbreak     => true
    case _                   => false

  private def layout(node: Layout, embeddings: List[Embedding], linker: Linker): List[Block] = node match
    case Layout.Paragraph(_, prose*) if prose.nonEmpty && prose.forall(hidden) =>
      Nil

    case Layout.Paragraph(_, prose*) =>
      List(Block.Paragraph(phrasing(List.from(prose), linker)))

    case Layout.Heading(_, level, prose*) =>
      List(Block.Heading(level, phrasing(List.from(prose), linker)))

    case Layout.BlockQuote(_, layouts*) =>
      List(Block.Quotation(all(List.from(layouts), embeddings, linker)))

    case Layout.ThematicBreak(_) =>
      List(Block.Rule())

    // An HTML comment is for the author, not the reader; other HTML is shown as it is.
    case Layout.HtmlBlock(_, html) if html.trim.starts(t"<!--") && html.trim.ends(t"-->") =>
      Nil

    case Layout.HtmlBlock(_, html) =>
      List(Block.Code(Language(t"html"), Embedding.lines(html)))

    case Layout.CodeBlock(line, info, content) =>
      fence(info, content, line.n0, embeddings)

    case Layout.BulletList(_, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings, linker)) })

      List(Block.Listing(false, listed))

    case Layout.OrderedList(_, _, _, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings, linker)) })

      List(Block.Listing(true, listed))
