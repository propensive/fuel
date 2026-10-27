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

// A link a document makes, by the line it is on — its paragraph's first, since phrasing has no
// line of its own — for the check which runs once every document is converted, when every
// target is known.
object Linkage:
  enum Target:
    case Document(section: Text, path: Text, fragment: Optional[Text])   // another document, by its path in its section
    case Site(path: Text, fragment: Optional[Text])                       // a page of the site, by its root-absolute path
    case Anchor(fragment: Text)                                           // a place on this page
    case Url(url: Text)                                                   // a URL, with a scheme
    case Sigil(reference: Text)                                           // a sigilled reference nothing resolved

  // The schema BinTEL writes a target under, anchored here so that a record holding one (the
  // output's prose) derives it in the member form; see `fuel_schemas.scala`.
  object Target:
    given telSchematic: Target is stratiform.TelSchematic over Tels.Type = atMember(TargetSchema.derived)

case class Linkage(line: Int, target: Linkage.Target)

// A Markdown document as Pyrocosm blocks, node for node: Pyrocosm's own conversion
// (`Presentable`'s `markdown`), with the fenced code blocks dispatched to the embeddings in
// scope. Inline HTML is dropped, as the parser already comments it out; an inline image becomes
// a link to it. To be offered upstream once it has settled here.
object Blocks:
  // Where a document is on the site, by its section and its path within it: what a relative
  // link and a `#fragment` resolve against.
  case class Page(section: Text, path: Text):
    def url: Text = Doc.url(section, path)

    // A relative link — `guide/x.md`, `../x.md` — against this document's directory, as a
    // path within the section; `Unset` when it climbs out of the section.
    def resolve(relative: Text): Optional[Text] =
      val segments: List[Text] = path.cut(t"/").filter(_ != t"")
      val directory: List[Text] = segments.keep(segments.size - 1)

      def recur(stack: List[Text], rest: List[Text]): Optional[List[Text]] = rest match
        case t".." :: tail => if stack.nil then Unset else recur(stack.keep(stack.size - 1), tail)
        case t"." :: tail  => recur(stack, tail)
        case head :: tail  => recur(stack + List(head), tail)
        case _             => stack

      recur(directory, relative.cut(t"/").filter(_ != t"")).let(_.join(t"/"))

  // What one conversion gathers besides its blocks: the links it made, for the check which
  // runs once every document is converted, and the anchors it gave its headings and fences,
  // each unique on its page.
  final class Context(val linker: Linker, val page: Optional[Page] = Unset, val glossary: Glossary = Glossary.none):
    private var references0: List[Linkage] = Nil
    private var anchors0: List[Text] = Nil

    def refer(line: Int, target: Linkage.Target): Unit = references0 = Linkage(line, target) :: references0
    def references: List[Linkage] = references0.reverse
    def anchors: List[Text] = anchors0.reverse

    // A slug as an anchor of the page: itself when no earlier heading took it, else numbered
    // from one until free, as GitHub numbers repeated headings.
    def anchor(slug: Text): Text =
      def free(candidate: Text, n: Int): Text =
        if !anchors0.has(candidate) then candidate else free(t"$slug-${n.show}", n + 1)

      val chosen: Text = free(if slug == t"" then t"section" else slug, 1)
      anchors0 = chosen :: anchors0
      chosen

    // An anchor a fence chose for itself, noted so that a link to it is known to land.
    def anchored(id: Text): Unit = if !anchors0.has(id) then anchors0 = id :: anchors0

  def of(markdown: Markdown of Layout, embeddings: List[Embedding], context: Context): List[Block] =
    all(markdown.children, embeddings, context)

  def of(markdown: Markdown of Layout, embeddings: List[Embedding], linker: Linker = Linker.none): List[Block] =
    of(markdown, embeddings, Context(linker))

  // A heading's anchor from its text, as GitHub makes one: lowercased, every character which
  // is not a letter, a digit, a space or a hyphen dropped, and each space a hyphen.
  def slug(text: Text): Text =
    Text(text.s.toLowerCase.nn.filter { c => c.isLetterOrDigit || c == ' ' || c == '-' }.replace(' ', '-').nn)

  // The first heading's text, of whatever level, which is what a document is called.
  def title(blocks: List[Block]): Optional[Text] = blocks match
    case Block.Heading(_, content, _) :: _ => plain(content)
    case _ :: tail                         => title(tail)
    case _                                 => Unset

  // The text of a run of phrasing, with no styling.
  def plain(inlines: List[Inline]): Text = inlines.map(plainInline).join(t"")

  private def plainInline(inline: Inline): Text = inline match
    case Inline.Textual(text)         => text
    case Inline.Phrase(content)       => plain(content)
    case Inline.Emphasis(content)     => plain(content)
    case Inline.Strong(content)       => plain(content)
    case Inline.Toned(_, content)     => plain(content)
    case Inline.Link(_, content)      => plain(content)
    case Inline.Annotated(content, _) => plain(content)
    case Inline.Snippet(_, tokens)    => tokens.map(_.text).join(t"")
    case _                            => t""

  private def all(nodes: List[Layout], embeddings: List[Embedding], context: Context): List[Block] = nodes match
    case head :: tail => layout(head, embeddings, context) + all(tail, embeddings, context)
    case _            => Nil

  private def phrasing(nodes: List[Prose], context: Context, line: Int): List[Inline] =
    nodes.map(prose(_, context, line))

  // A run of Markdown phrasing — a frame's label, say — as inlines: the first paragraph the
  // text parses to, or the text itself when it has none.
  def inline(text: Text, linker: Linker = Linker.none): List[Inline] =
    def first(nodes: List[Layout]): List[Inline] = nodes match
      case Layout.Paragraph(_, prose*) :: _ => phrasing(List.from(prose), Context(linker), 1)
      case _ :: tail                        => first(tail)
      case _                                => Inline.text(text)

    first(Parser.parse(text).children)

  // Inline code, whose tokens link when the linker says it is a reference: a qualified name a
  // link for each segment. A sigil (`#Name` for a type, `.name` for a term) says what is meant
  // and, once resolved, is not shown; a sigilled reference nothing resolves is shown as
  // written, and recorded for the check to report.
  private def snippet(code: Text, context: Context, line: Int): Inline =
    val tokens: List[Token] = context.linker.tokens(code)

    if Linker.kind(code).present && !tokens.exists(_.link.present)
    then context.refer(line, Linkage.Target.Sigil(code))

    Inline.Snippet(Language.Plain, tokens)

  private def prose(node: Prose, context: Context, line: Int): Inline = node match
    case Prose.Textual(text)       => Inline.Textual(text)
    case Prose.Emphasis(children*) => Inline.Emphasis(phrasing(List.from(children), context, line))
    case Prose.Strong(children*)   => Inline.Strong(phrasing(List.from(children), context, line))
    case Prose.Code(code)          => snippet(code, context, line)
    case Prose.Softbreak           => Inline.Textual(t" ")
    case Prose.Linebreak           => Inline.Break()
    case Prose.HtmlInline(_)       => Inline.Textual(t"")

    case Prose.Link(destination, _, content*) =>
      link(destination, phrasing(List.from(content), context, line), context, line)

    case Prose.Image(destination, _, content*) =>
      Inline.Link(Inline.Destination.External(destination), phrasing(List.from(content), context, line))

  // A destination's path and its fragment, after `#`.
  private def split(destination: Text): (Text, Optional[Text]) =
    val hash: Int = destination.s.indexOf('#')
    if hash < 0 then (destination, Unset)
    else (Text(destination.s.substring(0, hash).nn), Text(destination.s.substring(hash + 1).nn))

  // Where a link leads, by its destination as written: a glossary term (through the reference
  // definition the glossary added), a URL with a scheme, a place on this page, a page of the
  // site by its root-absolute path, or another document by a path relative to this one, which
  // is rewritten to that document's page. Each is recorded for the check; without a page (a
  // document whose section is not known) a link is left as it is written.
  private def link(destination: Text, content: List[Inline], context: Context, line: Int): Inline =
    val (path, fragment) = split(destination)
    val scheme: Boolean = path.s.matches("[A-Za-z][A-Za-z0-9+.-]*:.*")
    def external: Inline = Inline.Link(Inline.Destination.External(destination), content)

    if destination.starts(Glossary.scheme) then glossary(destination.skip(Glossary.scheme.length), content, context, line)
    else if scheme then
      context.refer(line, Linkage.Target.Url(destination))
      external
    else if path == t"" then
      context.page.lay(external): (page: Page) =>
        context.refer(line, Linkage.Target.Anchor(fragment.or(t"")))
        Inline.Link(Inline.Destination.Local(page.url, fragment), content)
    else if path.starts(t"/") then
      context.refer(line, Linkage.Target.Site(path, fragment))
      Inline.Link(Inline.Destination.Local(path, fragment), content)
    else
      context.page.lay(external): (page: Page) =>
        page.resolve(path) match
          case resolved: Text =>
            context.refer(line, Linkage.Target.Document(page.section, resolved, fragment))
            Inline.Link(Inline.Destination.Local(Doc.url(page.section, resolved), fragment), content)

          case _ =>
            context.refer(line, Linkage.Target.Document(page.section, path, fragment))
            external

  // A glossary term: its content annotated with the term's definition (its first paragraph,
  // which is a tooltip's size; the whole is on the glossary page), leading to the term's own
  // link when it has one, else to its entry on the glossary page. A term the glossary does not
  // have — a `glossary:` destination written by hand — leads to the glossary page, and the
  // check says it is not there.
  private def glossary(slug: Text, content: List[Inline], context: Context, line: Int): Inline =
    context.glossary(slug).lay[Inline](Inline.Link(Inline.Destination.Local(t"/glossary", slug), content)): (term: Glossary.Term) =>
      val destination: Inline.Destination = term.link.lay(Inline.Destination.Local(t"/glossary", term.slug)): (url: Text) =>
        val (path, fragment) = split(url)
        if url.starts(t"/") then Inline.Destination.Local(path, fragment) else Inline.Destination.External(url)

      destination match
        case Inline.Destination.Local(path, fragment) => context.refer(line, Linkage.Target.Site(path, fragment))
        case Inline.Destination.External(url)         => context.refer(line, Linkage.Target.Url(url))
        case _                                        => ()

      Inline.Annotated(List(Inline.Link(destination, content)), Blocks.inline(term.definition, context.linker))

    . also:
      // Recorded even when the term is unknown, so that the check reports it.
      if context.glossary(slug).absent then context.refer(line, Linkage.Target.Site(t"/glossary", slug))

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
  // each fence's output. A Scala fence's anchor is on its sentinel, and its definitions are
  // declared to the linker as the fence is met, so that the prose after it resolves them as it
  // would were the fence compiled in its place.
  def sentinels(linker: Linker = Linker.none): Embedding = new Embedding:
    private var index: Int = 0
    private var scala: Int = 0

    def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
      val scanned: Optional[(Text, List[(Text, Linker.Kind)])] =
        if info.prim != t"scala" && !info.has(t"fuel") then Unset else
          scala += 1
          Embedding.scan(info, content, scala)

      scanned.let { (scanned: (Text, List[(Text, Linker.Kind)])) => linker.define(scanned(0), scanned(1)) }

      val sentinel: Block = Output.sentinel(index) match
        case code: Block.Code => code.copy(id = scanned.let(_(0)))
        case other            => other

      index += 1
      List(sentinel)

  // A document's prose with each sentinel replaced by its fence's output.
  def fill(blocks: List[Block], outputs: List[List[Block]]): List[Block] = blocks match
    case head :: tail => filled(head, outputs) + fill(tail, outputs)
    case _            => Nil

  private def filled(block: Block, outputs: List[List[Block]]): List[Block] = block match
    case Block.Code(Output.fence, _, _, index: Int, _, _) =>
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

  private def layout(node: Layout, embeddings: List[Embedding], context: Context): List[Block] = node match
    case Layout.Paragraph(_, prose*) if prose.nonEmpty && prose.forall(hidden) =>
      Nil

    case Layout.Paragraph(line, prose*) =>
      List(Block.Paragraph(phrasing(List.from(prose), context, line.n1)))

    case Layout.Heading(line, level, prose*) =>
      val content: List[Inline] = phrasing(List.from(prose), context, line.n1)
      List(Block.Heading(level, content, context.anchor(slug(plain(content)))))

    case Layout.BlockQuote(_, layouts*) =>
      List(Block.Quotation(all(List.from(layouts), embeddings, context)))

    case Layout.ThematicBreak(_) =>
      List(Block.Rule())

    // An HTML comment is for the author, not the reader; other HTML is shown as it is.
    case Layout.HtmlBlock(_, html) if html.trim.starts(t"<!--") && html.trim.ends(t"-->") =>
      Nil

    case Layout.HtmlBlock(_, html) =>
      List(Block.Code(Language(t"html"), Embedding.lines(html)))

    // A fence's anchor is the id its embedding gave it (or its sentinel carries), which a
    // reference to a name it defines leads to.
    case Layout.CodeBlock(line, info, content) =>
      val blocks: List[Block] = fence(info, content, line.n0, embeddings)

      blocks.each: (block: Block) =>
        block match
          case Block.Code(_, _, _, _, _, id) => id.let(context.anchored(_))
          case _                             => ()

      blocks

    case Layout.BulletList(_, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings, context)) })

      List(Block.Listing(false, listed))

    case Layout.OrderedList(_, _, _, _, items*) =>
      val listed: List[Block.Item] =
        List.from(items.map { (item: List[Layout]) => Block.Item(all(item, embeddings, context)) })

      List(Block.Listing(true, listed))
