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

import soundness.{Language as _, Token as _, *}

import dysasymptotics.linearSize
import pyrocosm.{Block, Inline, Language, Markdowns, Token}

// A link a document makes, by the line it is on — its paragraph's first, since phrasing has no
// line of its own — for the check which runs once every document is converted, when every
// target is known.
object Linkage:
  enum Target:
    // Another document, by its path in its section.
    case Document(section: Text, path: Text, fragment: Optional[Text])

    // A page of the site, by its root-absolute path.
    case Site(path: Text, fragment: Optional[Text])

    case Anchor(fragment: Text)  // a place on this page
    case Url(url: Text)          // a URL, with a scheme
    case Sigil(reference: Text)  // a sigilled reference nothing resolved

case class Linkage(line: Int, target: Linkage.Target)

// A Markdown document as Pyrocosm blocks: Pyrocosm's own conversion (`Markdowns`), with its
// hooks answered here — the fenced code blocks dispatched to the embeddings in scope, inline
// code linked through the linker, links resolved against the page, and headings anchored.
object Blocks:
  // Where a document is on the site, by its section and its path within it: what a relative
  // link and a `#fragment` resolve against. `root` is the section's directory and `sections`
  // every configured section's name and directory, which a relative link climbing out of its
  // own section is resolved through, as the files on disk relate.
  case class Page
    ( section:  Text,
      path:     Text,
      root:     Optional[Text]       = Unset,
      sections: List[(Text, Text)]   = Nil ):

    def url: Text = Doc.url(section, path)

    // What a relative link resolves through beyond the page itself, for a key of its prose.
    def context: Text =
      val others: List[Text] = sections.map { (name: Text, root: Text) => t"$name=$root" }
      t"${root.or(t"")}\n${others.join(t"\n")}"

    // A relative link — `guide/x.md`, `../x.md` — against this document's directory, as a
    // section and a path within it: this document's section while the link stays within its
    // directory, else the configured section whose directory holds the file it leads to (the
    // innermost, where one is within another); `Unset` when no section holds it.
    def resolve(relative: Text): Optional[(Text, Text)] =
      def segments(text: Text): List[Text] = text.cut(t"/").filter(_ != t"")

      def recur(stack: List[Text], rest: List[Text]): Optional[List[Text]] = rest match
        case t".." :: tail => if stack.nil then Unset else recur(stack.keep(stack.size - 1), tail)
        case t"." :: tail  => recur(stack, tail)
        case head :: tail  => recur(stack + List(head), tail)
        case _             => stack

      val own: List[Text] = segments(path)
      val directory: List[Text] = own.keep(own.size - 1)
      val steps: List[Text] = segments(relative)

      recur(directory, steps).let { (within: List[Text]) => (section, within.join(t"/")) }.or:
        root.let: (root: Text) =>
          recur(segments(root) + directory, steps).let: (absolute: List[Text]) =>
            val holding: List[(Text, List[Text])] =
              sections.map { (name: Text, root: Text) => (name, segments(root)) }.filter:
                (_, root: List[Text]) =>
                  absolute.size > root.size && absolute.keep(root.size) == root

            def innermost
              ( rest: List[(Text, List[Text])], best: Optional[(Text, List[Text])] )
            :   Optional[(Text, List[Text])] =

              rest match
                case head :: tail =>
                  val deeper: Boolean = best.lay(true)(head(1).size > _(1).size)
                  innermost(tail, if deeper then head else best)

                case _ =>
                  best

            innermost(holding, Unset).let: (chosen: (Text, List[Text])) =>
              (chosen(0), absolute.skip(chosen(1).size).join(t"/"))

  // What one conversion gathers besides its blocks: the links it made, for the check which
  // runs once every document is converted, and the anchors it gave its headings and fences,
  // each unique on its page.
  final class Context
    ( val linker:   Linker,
      val page:     Optional[Page] = Unset,
      val glossary: Glossary       = Glossary.none ):

    private var references0: List[Linkage] = Nil
    private var anchors0: List[Text] = Nil

    def refer(line: Int, target: Linkage.Target): Unit =
      references0 = Linkage(line, target) :: references0

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

    // This conversion's answers to Pyrocosm's hooks, with the embeddings in scope.
    def hooks(embeddings: List[Embedding]): Markdowns.Hooks = new Markdowns.Hooks:
      private val context: Context = Context.this

      // A fence's anchor is the id its embedding gave it (or its sentinel carries), which a
      // reference to a name it defines leads to. The embeddings count lines from zero.
      override def fence(info: List[Text], content: Text, line: Int): List[Block] =
        val blocks: List[Block] = Blocks.fence(info, content, line - 1, embeddings)

        blocks.each: (block: Block) =>
          block match
            case Block.Code(_, _, _, _, _, id, _, _) => id.let(context.anchored(_))
            case _                             => ()

        blocks

      override def code(code: Text, line: Int): Inline = snippet(code, context, line)

      override def link(destination: Text, content: List[Inline], line: Int): Inline =
        Blocks.link(destination, content, context, line)

      override def heading(level: Int, content: List[Inline], line: Int): Block =
        Block.Heading(level, content, context.anchor(slug(plain(content))))

  def of(markdown: Markdown of Layout, embeddings: List[Embedding], context: Context): List[Block] =
    Markdowns.convert(markdown, context.hooks(embeddings))

  def of(markdown: Markdown of Layout, embeddings: List[Embedding], linker: Linker = Linker.none)
  :   List[Block] =

    of(markdown, embeddings, Context(linker))

  // A heading's anchor from its text, as GitHub makes one.
  def slug(text: Text): Text = Markdowns.slug(text)

  // The first heading's text, of whatever level, which is what a document is called.
  def title(blocks: List[Block]): Optional[Text] = Markdowns.title(blocks)

  // The text of a run of phrasing, with no styling.
  def plain(inlines: List[Inline]): Text = Markdowns.plain(inlines)

  // A run of Markdown phrasing — a frame's label, say — as inlines, linked through the linker.
  def inline(text: Text, linker: Linker = Linker.none): List[Inline] =
    Markdowns.inline(text, Context(linker).hooks(Nil))

  // Inline code, whose tokens link when the linker says it is a reference: a qualified name a
  // link for each segment. A sigil (`#Name` for a type, `.name` for a term) says what is meant
  // and, once resolved, is not shown; a sigilled reference nothing resolves is shown as
  // written, and recorded for the check to report.
  private def snippet(code: Text, context: Context, line: Int): Inline =
    val tokens: List[Token] = context.linker.tokens(code)

    if Linker.kind(code).present && !tokens.exists(_.link.present)
    then context.refer(line, Linkage.Target.Sigil(code))

    Inline.Snippet(Language.Plain, tokens)

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
    val glossed: Boolean = destination.starts(Glossary.scheme)

    if glossed then glossary(destination.skip(Glossary.scheme.length), content, context, line)
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
          case (section: Text, resolved: Text) =>
            context.refer(line, Linkage.Target.Document(section, resolved, fragment))
            val url: Text = Doc.url(section, resolved)
            Inline.Link(Inline.Destination.Local(url, fragment), content)

          case _ =>
            context.refer(line, Linkage.Target.Document(page.section, path, fragment))
            external

  // A glossary term: its content annotated with the term's definition (its first paragraph,
  // which is a tooltip's size; the whole is on the glossary page), leading to the term's own
  // link when it has one, else to its entry on the glossary page. A term the glossary does not
  // have — a `glossary:` destination written by hand — leads to the glossary page, and the
  // check says it is not there.
  private def glossary(slug: Text, content: List[Inline], context: Context, line: Int): Inline =
    val unknown: Inline = Inline.Link(Inline.Destination.Local(t"/glossary", slug), content)

    context.glossary(slug).lay[Inline](unknown): (term: Glossary.Term) =>
      val entry: Inline.Destination = Inline.Destination.Local(t"/glossary", term.slug)

      val destination: Inline.Destination = term.link.lay(entry): (url: Text) =>
        val (path, fragment) = split(url)

        if url.starts(t"/") then Inline.Destination.Local(path, fragment)
        else Inline.Destination.External(url)

      destination match
        case Inline.Destination.Local(path, fragment) =>
          context.refer(line, Linkage.Target.Site(path, fragment))

        case Inline.Destination.External(url) =>
          context.refer(line, Linkage.Target.Url(url))

        case _ =>
          ()

      val definition: List[Inline] = Blocks.inline(term.definition, context.linker)
      Inline.Annotated(List(Inline.Link(destination, content)), definition)

    . also:
      // Recorded even when the term is unknown, so that the check reports it.
      if context.glossary(slug).absent
      then context.refer(line, Linkage.Target.Site(t"/glossary", slug))

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
  def fences(markdown: Markdown of Layout): List[Fence] =
    Markdowns.fences(markdown).map: (fence: Markdowns.Fence) =>
      Fence(fence.info, fence.content, fence.line - 1)

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
        if info.head != t"scala" && !info.has(t"fuel") then Unset else
          scala += 1
          Embedding.scan(info, content, scala)

      scanned.let: (scanned: (Text, List[(Text, Linker.Kind)])) =>
        linker.define(scanned(0), scanned(1))

      val sentinel: Block = Output.sentinel(index) match
        case code: Block.Code => code.copy(id = scanned.let(_(0)))
        case other            => other

      index += 1
      List(sentinel)

  // A document's prose with each sentinel replaced by its fence's output.
  def fill(blocks: List[Block], outputs: List[List[Block]]): List[Block] =
    Markdowns.rewrite(blocks):
      case Block.Code(Output.fence, _, _, index: Int, _, _, _, _) =>
        val empty: List[Block] = Nil
        outputs.skip(index).head.or(empty)

      case _ =>
        Unset
