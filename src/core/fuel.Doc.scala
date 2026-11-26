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

import dysasymptotics.linearSize
import pyrocosm.{Block, Digests, Tone}

object Doc:
  // The URL of a document on the site: its section and its path, each segment encoded.
  def url(section: Text, path: Text): Text =
    def encode(text: Text): Text = text.urlEncode.sub(t"+", t"%20")

    t"/doc/${encode(section)}/${path.cut(t"/").map(encode).join(t"/")}"

  // Parses and converts one document, its Scala fragments checked by `checker` — in one
  // session, each following the ones before it, when `session` says so, with the fixture
  // (its path and text) compiled first; `problems` are the fragments which did not do as
  // their preambles said. `section` is the section the document is in, which is what its
  // anchors and relative links are resolved against, `root` its directory and `sections`
  // every section's name and directory, which a link climbing out of it resolves through;
  // `glossary` has the terms its prose may name in brackets.
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
      glossary: Glossary                 = Glossary.none,
      root:     Optional[Text]           = Unset,
      sections: List[(Text, Text)]       = Nil )
  :   Doc =

    val page: Optional[Blocks.Page] = section.let(Blocks.Page(_, path, root, sections))

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
          case Layout.CodeBlock(line, _, content) if content.contains(definitions) => line.n1
          case _                                                                       => Unset

    val markdown: Markdown of Layout = if unclosed.present then Parser.parse(text) else parsed

    val open: List[Block] = unclosed.let: (line: Int) =>
      val message: Block = Block.paragraph(t"This fence is not closed.")
      val excerpt: List[Block] = Excerpts.plain(text, line, Block.Note.Style.Erroneous)
      val location: Block.Location = Block.Location(path, line)
      val code: fulminate.Bolt = FuelBolts(FuelBolts.core, FuelBolts.unclosed)
      List(Block.Diagnostic(Tone.Failure, location, List(message), excerpt, code = code))

    . or(Nil)

    cache match
      case cache: Cache =>
        kept
          ( path, text, markdown, checker, session, fixture, linker, cache, base, page, glossary,
            open, section )

      case _ =>
        val context: Blocks.Context = Blocks.Context(linker, page, glossary)
        val scala = Blocks.scala(checker, path, session, fixture, linker, section = section)
        val tables: Tables = Tables(path, linker)
        val embeddings: List[Embedding] = List(scala, Embedding.JavaCode, tables)
        val blocks: List[Block] = Blocks.of(markdown, embeddings, context)
        val unclear: List[Block] = ambiguous(path, text, linker, context, tables.ambiguous)
        val problems: List[Block] = scala.problems + tables.problems + unclear + open
        val title: Text = Blocks.title(blocks).or(filename(path))
        Doc(path, title, blocks, problems, context.references, context.anchors, text)

  // A reference which named several entities is a caution: it is shown unlinked. Each is
  // reported where the prose has it, on its line; one the prose did not place — in a glossary's
  // definition, say — at the document alone, unless it is one of the `elsewhere`, which a table
  // has reported at its own place.
  private def ambiguous
    ( path:      Text,
      text:      Text,
      linker:    Linker,
      context:   Blocks.Context,
      elsewhere: List[Text] )
  :   List[Block] =

    val lines: List[Text] = text.cut(t"\n")

    // The line the reference is written on: the first at or after its paragraph's first, and
    // before the paragraph ends, to have it in backticks.
    def written(first: Int, reference: Text): Int =
      def recur(line: Int, rest: List[Text]): Int = rest match
        case content :: rest if content.trim != t"" =>
          if content.contains(t"`$reference`") then line else recur(line + 1, rest)

        case _ =>
          first

      recur(first, lines.skip(first - 1))

    val placed: List[Block] = context.ambiguities.map: (first: Int, ambiguity: Linker.Ambiguity) =>
      val line: Int = written(first, ambiguity.reference)

      val excerpt: List[Block] =
        if text == t"" then Nil
        else Excerpts.plainAt(text, line, ambiguity.reference, Block.Note.Style.Caution)

      Doc.ambiguity(ambiguity, Block.Location(path, line), excerpt)

    val known: List[Text] = context.ambiguities.map(_(1).reference) + elsewhere

    val others: List[Linker.Ambiguity] = linker.ambiguities.filter:
      (ambiguity: Linker.Ambiguity) => !known.has(ambiguity.reference)

    val unplaced: List[Block] = others.map(Doc.ambiguity(_, Block.Location(path)))

    placed + unplaced

  // The caution that a reference names several entities, which it names where they are known,
  // at its place in a document or on an API's page, `↯/f.25.3`.
  def ambiguity
    ( ambiguity: Linker.Ambiguity, location: Block.Location, excerpt: List[Block] = Nil )
  :   Block.Diagnostic =

    val candidates: Text =
      if ambiguity.candidates.nil then t""
      else t" (${ambiguity.candidates.map { (key: Text) => t"`$key`" }.join(t", ")})"

    val reference: Text = ambiguity.reference

    val text: Text =
      t"`$reference` names more than one entity$candidates, so it is not linked; qualify it."

    val code: fulminate.Bolt = FuelBolts(FuelBolts.core, FuelBolts.ambiguous)
    Block.Diagnostic(Tone.Warning, location, List(Block.paragraph(text)), excerpt, code = code)

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
      open:     List[Block],
      section:  Optional[Text] )
  :   Doc =

    val fences: List[Blocks.Fence] = Blocks.fences(markdown)

    val imports: List[Text] = fences.bind: (fence: Blocks.Fence) =>
      fence.content.cut(t"\n").filter(_.trim.starts(t"import ")).map(_.trim)

    // What each Scala fence will define, and under which anchor, as text: part of the prose's
    // key, since a reference resolves through them.
    val declared: List[Text] =
      val scala: List[Blocks.Fence] = fences.filter: (fence: Blocks.Fence) =>
        fence.info.head == t"scala" || fence.info.has(t"fuel")

      scala.indexed.bind: (fence: Blocks.Fence, index: Ordinal) =>
        val scanned: Optional[(Text, List[(Text, Linker.Kind)])] =
          Embedding.scan(fence.info, fence.content, index.n1)

        scanned.let: (anchor: Text, definitions: List[(Text, Linker.Kind)]) =>
          val named: List[Text] = definitions.map: (name: Text, kind: Linker.Kind) =>
            t"$name:${kind.show}"

          List(t"$anchor=${named.join(t",")}")

        . or(Nil)

    val bare: Blocks.Context = Blocks.Context(Linker.none, page, glossary)
    val unlinked: List[Block] = Blocks.of(markdown, List(Blocks.sentinels()), bare)
    val shape: Text = Digests.of(Output.Prose.encode(Output.Prose(unlinked, Nil)))
    val url: Text = page.let(_.url).or(t"")
    val sections: Text = page.let(_.context).or(t"")

    val key: Text =
      Digests.of
        ( base, t"prose", url, sections, shape, imports.join(t"\n"), declared.join(t"\n") )

    val prose: Output.Prose = cache.prose(key).or:
      linker.learn(imports)
      val context: Blocks.Context = Blocks.Context(linker, page, glossary)
      val blocks: List[Block] = Blocks.of(markdown, List(Blocks.sentinels(linker)), context)
      val problems: List[Block] = ambiguous(path, text, linker, context, Nil)
      val prose: Output.Prose = Output.Prose(blocks, problems, context.references, context.anchors)
      cache.prose(key, prose)
      prose

    val scala = Blocks.scala(checker, path, session, fixture, linker, cache, base, section)
    val tables: Tables = Tables(path, linker)
    val embeddings: List[Embedding] = List(scala, Embedding.JavaCode, tables)

    val outputs: List[List[Block]] = fences.map: (fence: Blocks.Fence) =>
      Blocks.fence(fence.info, fence.content, fence.line, embeddings)

    val blocks: List[Block] = Blocks.fill(prose.blocks, outputs)
    val title: Text = Blocks.title(blocks).or(filename(path))
    val problems: List[Block] = scala.problems + tables.problems + prose.problems + open
    Doc(path, title, blocks, problems, prose.references, prose.anchors, text)

  // The last segment of a path, without a `.md` extension.
  def filename(path: Text): Text =
    val name: Text = path.cut(t"/").reverse.head.or(path)
    if name.ends(t".md") then name.skip(3, Rtl) else name

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
