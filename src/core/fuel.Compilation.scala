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
import pyrocosm.{Block, Tone}

// The compile step: every `*.md` beneath a document directory, read, parsed and converted.
object Compilation:
  // How a corpus's fragments are compiled: as one session per document, each fragment
  // following the ones before it, and with the document's fixture
  // (`<fixtures>/<section>/<path>.scala`, else `<fixtures>/<name>.scala`) first.
  // `linker` gives each document, by its section and its path, the linker its references
  // resolve through; with a `cache`, a document's prose and each of its fences is compiled only
  // when the key of its inputs is not kept, `base` giving the key of everything a document
  // depends on beyond its own text, by its path; `glossary` has the terms its prose may name;
  // `sections` is every section's name and directory, which a relative link leaving its own
  // section resolves through (a corpus gives its own when the regime gives none); `library`
  // is where the documents are read from, the filesystem unless an Orthant store holds them.
  case class Regime
    ( session:  Boolean                  = false,
      fixtures: Optional[Text]           = Unset,
      progress: (Text, Doc) => Unit      = (_, _) => (),
      linker:   (Text, Text) => Linker   = (_: Text, _: Text) => Linker.none,
      cache:    Optional[Cache]          = Unset,
      base:     Text => Text             = (_: Text) => t"",
      glossary: Glossary                 = Glossary.none,
      sections: List[(Text, Text)]       = Nil,
      library:  Library                  = Library.Disk )

  // A section's name where nothing says it: its directory's last segment.
  def name(root: Text): Text = root.cut(t"/").filter(_ != t"").reverse.head.or(root)

  // A section's name: the one the regime gives its directory, as the configuration names it.
  def name(root: Text, regime: Regime): Text =
    regime.sections.filter(_(1) == root).head.let(_(0)).or(name(root))

  // The Markdown documents beneath a root, in order: what a section compiles, and so what there
  // is to do, which a compile may count before it starts.
  def files(root: Text, library: Library = Library.Disk): List[Text] = library.files(root, t".md")

  def section(root: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Corpus.Section =

    val files: List[Text] = this.files(root, regime.library)

    def relative(path: Text): Text = path.skip(root.length + 1)

    val named: Text = name(root, regime)

    def fixture(path: Text): Optional[(Text, Text)] =
      Compilation.fixture(regime, named, relative(path))

    def recur(rest: List[Text], docs: List[Doc], skipped: List[Text]): Corpus.Section = rest match
      case path :: tail =>
        regime.library.text(path) match
          case text: Text =>
            val within: Text = relative(path)
            val linker: Linker = regime.linker(named, within)

            val parsed: Doc =
              Doc.parse
                ( within, text, checker, regime.session, fixture(path), linker, regime.cache,
                  regime.base(t"$named/$within"), named, regime.glossary, root,
                  regime.sections )

            regime.progress(within, parsed)
            recur(tail, docs + List(parsed), skipped)

          case _ =>
            recur(tail, docs, skipped + List(path))

      case _ =>
        Corpus.Section(named, root, docs, skipped)

    recur(files, Nil, Nil)

  // One document, by its path, compiled as a section's would be, with its fixture if the
  // regime names a directory holding one, in a section named by its directory; `Unset` if the
  // file cannot be read.
  def document(path: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Optional[Doc] =

    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    val slash: Optional[Ordinal] = path.offsetOf(t"/", Rtl)
    val directory: Text = slash.lay(t"") { (slash: Ordinal) => path.keep(slash.n0) }
    val section: Text = name(directory, regime)
    val leaf: Text = slash.lay(path) { (slash: Ordinal) => path.skip(slash.n0 + 1) }
    val fixture: Optional[(Text, Text)] = Compilation.fixture(regime, section, leaf)

    safely(path.as[Path on Linux]).let { (file: Path on Linux) => safely(file.read[Text]) }.let:
      (text: Text) =>
        val linker: Linker = regime.linker(section, path)

        Doc.parse
          ( path, text, checker, regime.session, fixture, linker, regime.cache, regime.base(path),
            section, regime.glossary, directory, regime.sections )

  // A document's fixture, when the regime names a directory holding one: the file at the
  // document's section and path within it (`<fixtures>/modules/logging.scala` for
  // `logging.md` in the section `modules`), else the one named by its file name alone
  // (`<fixtures>/logging.scala`), so that two documents of one name in different sections may
  // each have their own.
  def fixture(regime: Regime, section: Text, path: Text): Optional[(Text, Text)] =
    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    regime.fixtures.let: (directory: Text) =>
      val stem: Text = if path.ends(t".md") then path.keep(path.length - 3) else path

      val candidates: List[Text] =
        List(t"$directory/$section/$stem.scala", t"$directory/${Doc.filename(path)}.scala")

      def read(file: Text): Optional[(Text, Text)] =
        safely(file.as[Path on Linux]).let { (file: Path on Linux) => safely(file.read[Text]) }.let:
          (text: Text) => (file, text)

      val found: List[(Text, Text)] = candidates.bind: (file: Text) =>
        read(file).let(List(_)).or(Nil)

      found.head

  // A corpus as a language reads it: each document compiled from its text in that language,
  // which `library` reads — the regime's is the language's — as the text it was written in
  // was, so that a fence every language shares is compiled once where the regime keeps what
  // it compiles; and the document as it was written, where the language has none or its text
  // is the same. A translated heading keeps the anchor of the heading it translates, so a
  // link to a place in a document means the same in every language. Each document's problems
  // are its own: those the text it was written in has too — in the passages nobody has
  // translated, which are that text's — are not said again.
  def translated(corpus: Corpus, checker: Checker, regime: Regime): Corpus =
    def anchored(doc: Doc, definitive: Doc): Doc =
      def anchors(blocks: List[Block]): List[Optional[Text]] =
        blocks.bind:
          case Block.Heading(_, _, id) => List(id)
          case _                       => Nil

      val theirs: List[Optional[Text]] = anchors(definitive.blocks)

      if anchors(doc.blocks).size != theirs.size then doc else
        val (blocks, _) =
          doc.blocks.fold((List[Block](), theirs)):
            (state: (List[Block], List[Optional[Text]]), block: Block) =>
              val (done, left) = state

              block match
                case Block.Heading(level, content, _) =>
                  (done + List(Block.Heading(level, content, left.head.or(Unset))), left.skip(1))

                case other =>
                  (done + List(other), left)

        doc.copy(blocks = blocks, anchors = definitive.anchors)

    val complete: Regime =
      if !regime.sections.nil then regime
      else regime.copy(sections = corpus.sections.map { section => (section.name, section.root) })

    val sections: List[Corpus.Section] = corpus.sections.map: (section: Corpus.Section) =>
      val docs: List[Doc] = section.docs.map: (doc: Doc) =>
        val path: Text = t"${section.root}/${doc.path}"
        complete.library.text(path).lay(doc): (text: Text) =>
          if doc.text == text then doc else
            val parsed: Doc =
              Doc.parse
                ( doc.path, text, checker, complete.session,
                  fixture(complete, section.name, doc.path),
                  complete.linker(section.name, doc.path), complete.cache,
                  complete.base(t"${section.name}/${doc.path}"), section.name, complete.glossary,
                  section.root, complete.sections )

            val read: Doc = anchored(parsed, doc)
            read.copy(problems = novel(read.problems, doc.problems))

      section.copy(docs = docs, skipped = Nil)

    Corpus(sections)

  // The problems which are not also among others, wherever each is: a problem is the same
  // as another which says the same of the same kind of thing, whichever line it is on.
  def novel(problems: List[Block], others: List[Block]): List[Block] =
    def gist(block: Block): Block = block match
      case diagnostic: Block.Diagnostic => diagnostic.copy(location = Unset, excerpt = Nil)
      case other                        => other

    val known: List[Block] = others.map(gist)
    problems.filter { (problem: Block) => !known.has(gist(problem)) }

  // A file that could not be read or parsed, as a diagnostic.
  def unreadable(path: Text): Block =
    val message: Block = Block.paragraph(t"The file could not be read.")
    val code: fulminate.Bolt = FuelBolts(FuelBolts.core, FuelBolts.unreadable)
    Block.Diagnostic(Tone.Failure, Block.Location(path), List(message), code = code)

  def corpus(roots: List[Text], checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Corpus =

    val sections: List[(Text, Text)] =
      if !regime.sections.nil then regime.sections
      else roots.map { (root: Text) => (name(root), root) }

    val complete: Regime = regime.copy(sections = sections)
    Corpus(roots.map(section(_, checker, complete)))
