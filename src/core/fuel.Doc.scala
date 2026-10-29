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

import pyrocosm.{Block, Tone}

object Doc:
  // The URL of a document on the site: its section and its path, each segment encoded.
  def url(section: Text, path: Text): Text =
    def encode(text: Text): Text = Text(java.net.URLEncoder.encode(text.s, "UTF-8").nn.replace("+", "%20").nn)
    t"/doc/${encode(section)}/${path.cut(t"/").map(encode).join(t"/")}"

  // Parses and converts one document, its Scala fragments checked by `checker` — in one
  // session, each following the ones before it, when `session` says so, with the fixture
  // (its path and text) compiled first; `problems` are the fragments which did not do as
  // their preambles said. `section` is the section the document is in, which is what its
  // anchors and relative links are resolved against; `glossary` has the terms its prose may
  // name in brackets.
  def parse
    ( path:     Text,
      text:     Text,
      checker:  Checker                  = Checker.tokenized,
      session:  Boolean                  = false,
      fixture:  Optional[(Text, Text)]   = Unset,
      linker:   Linker                   = Linker.none,
      cache:    Optional[Cache]          = Unset,
      base:     Text                     = t"",
      section:  Optional[Text]           = Unset,
      glossary: Glossary                 = Glossary.none )
  :   Doc =

    val page: Optional[Blocks.Page] = section.let(Blocks.Page(_, path))

    // The glossary's reference definitions follow the text, so that `[term]` is a link the
    // parser makes and no line of the document moves. A fence left open runs to the end of the
    // input and would swallow them: that fence is a problem, and the document is read as
    // written.
    val definitions: Text = glossary.definitions
    val extended: Text = if definitions == t"" then text else t"$text\n\n$definitions"
    val parsed: Markdown of Layout = Parser.parse(extended)

    val unclosed: Optional[Int] =
      if definitions == t"" then Unset else parsed.children.reverse.head.let: (node: Layout) =>
        node match
          case Layout.CodeBlock(line, _, content) if content.s.contains(definitions.s) => line.n1
          case _                                                                       => Unset

    val markdown: Markdown of Layout = if unclosed.present then Parser.parse(text) else parsed

    val open: List[Block] = unclosed.let: (line: Int) =>
      List(Block.Diagnostic(Tone.Failure, Block.Location(path, line), List(Block.paragraph(t"This fence is not closed.")), Excerpts.plain(text, line, Block.Note.Style.Erroneous)))
    . or(Nil)

    cache match
      case cache: Cache =>
        kept(path, text, markdown, checker, session, fixture, linker, cache, base, page, glossary, open)

      case _ =>
        val context: Blocks.Context = Blocks.Context(linker, page, glossary)
        val scala = Embedding.ScalaCode(checker, path, session, fixture, linker)
        val embeddings: List[Embedding] = List(scala, Embedding.JavaCode)
        val blocks: List[Block] = Blocks.of(markdown, embeddings, context)
        val problems: List[Block] = scala.problems + ambiguous(path, linker) + open
        Doc(path, Blocks.title(blocks).or(filename(path)), blocks, problems, context.references, context.anchors, text)

  // A reference which named several entities is a caution: it is shown unlinked.
  private def ambiguous(path: Text, linker: Linker): List[Block] =
    linker.ambiguities.map: (reference: Text) =>
      val text: Text = t"`$reference` names more than one entity, so it is not linked; qualify it."
      Block.Diagnostic(Tone.Warning, Block.Location(path), List(Block.paragraph(text)))

  // A document compiled in two parts, each kept by the key of its own inputs: its prose, with a
  // sentinel where each fence is — keyed by that prose, unlinked, its place on the site, the
  // imports its fences make and the names they define, which its references resolve through —
  // and its fences, each kept by `ScalaCode`; then the prose filled with the fences' output.
  private def kept
    ( path:     Text,
      text:     Text,
      markdown: Markdown of Layout,
      checker:  Checker,
      session:  Boolean,
      fixture:  Optional[(Text, Text)],
      linker:   Linker,
      cache:    Cache,
      base:     Text,
      page:     Optional[Blocks.Page],
      glossary: Glossary,
      open:     List[Block] )
  :   Doc =

    val fences: List[Blocks.Fence] = Blocks.fences(markdown)

    val imports: List[Text] = fences.bind: (fence: Blocks.Fence) =>
      fence.content.cut(t"\n").filter(_.trim.starts(t"import ")).map(_.trim)

    // What each Scala fence will define, and under which anchor, as text: part of the prose's
    // key, since a reference resolves through them.
    val declared: List[Text] =
      val scala: List[Blocks.Fence] = fences.filter { (fence: Blocks.Fence) => fence.info.head == t"scala" || fence.info.has(t"fuel") }

      scala.indexed.bind: (fence: Blocks.Fence, index: Ordinal) =>
        Embedding.scan(fence.info, fence.content, index.n1).let: (scanned: (Text, List[(Text, Linker.Kind)])) =>
          List(t"${scanned(0)}=${scanned(1).map { (name: Text, kind: Linker.Kind) => t"$name:${kind.toString.tt}" }.join(t",")}")
        . or(Nil)

    val unlinked: List[Block] = Blocks.of(markdown, List(Blocks.sentinels()), Blocks.Context(Linker.none, page, glossary))
    val shape: Text = Hashes.of(Output.Prose.encode(Output.Prose(unlinked, Nil)))
    val key: Text = Hashes.of(base, t"prose", page.let(_.url).or(t""), shape, imports.join(t"\n"), declared.join(t"\n"))

    val prose: Output.Prose = cache.prose(key).or:
      linker.learn(imports)
      val context: Blocks.Context = Blocks.Context(linker, page, glossary)
      val blocks: List[Block] = Blocks.of(markdown, List(Blocks.sentinels(linker)), context)
      val prose: Output.Prose = Output.Prose(blocks, ambiguous(path, linker), context.references, context.anchors)
      cache.prose(key, prose)
      prose

    val scala = Embedding.ScalaCode(checker, path, session, fixture, linker, cache, base)
    val embeddings: List[Embedding] = List(scala, Embedding.JavaCode)

    val outputs: List[List[Block]] = fences.map: (fence: Blocks.Fence) =>
      Blocks.fence(fence.info, fence.content, fence.line, embeddings)

    val blocks: List[Block] = Blocks.fill(prose.blocks, outputs)
    Doc(path, Blocks.title(blocks).or(filename(path)), blocks, scala.problems + prose.problems + open, prose.references, prose.anchors, text)

  // The last segment of a path, without a `.md` extension.
  def filename(path: Text): Text =
    val name: Text = path.cut(t"/").reverse.head.or(path)
    if name.ends(t".md") then Text(name.s.substring(0, name.s.length - 3).nn) else name

// One Markdown document, compiled: its path relative to the directory it was found under, its
// title (the first level-one heading, else its filename), its blocks, the problems its
// fragments had (each a diagnostic at its place), the links it makes, for the check which runs
// once every document is compiled, and the anchors on it, which such a link may name; and its
// text, for a problem found later (a link which leads nowhere) to show its line.
case class Doc
  ( path:       Text,
    title:      Text,
    blocks:     List[Block],
    problems:   List[Block]     = Nil,
    references: List[Linkage]   = Nil,
    anchors:    List[Text]      = Nil,
    text:       Text            = t"" )
