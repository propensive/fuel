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
  // section resolves through (a corpus gives its own when the regime gives none);
  // `translations` are the directories which hold the translations of others.
  case class Regime
    ( session:  Boolean                  = false,
      fixtures: Optional[Text]           = Unset,
      progress: (Text, Doc) => Unit      = (_, _) => (),
      linker:   (Text, Text) => Linker   = (_: Text, _: Text) => Linker.none,
      cache:    Optional[Cache]          = Unset,
      base:     Text => Text             = (_: Text) => t"",
      glossary: Glossary                 = Glossary.none,
      sections: List[(Text, Text)]       = Nil,
      translations: List[Translations.Target] = Nil )

  // A section's name is its directory's last segment.
  def name(root: Text): Text = root.cut(t"/").filter(_ != t"").reverse.head.or(root)

  // The Markdown documents beneath a root, in order: what a section compiles, and so what there
  // is to do, which a compile may count before it starts. A file in a directory of translations
  // is not a document.
  def files(root: Text, translations: List[Translations.Target] = Nil): List[Text] =
    sources(root).filter(!Translations.held(_, translations))

  // Every Markdown file beneath a root, in order.
  def sources(root: Text): List[Text] =
    import filesystemBackends.javaBaseFilesystem
    import filesystemOptions.dereferenceSymlinks
    import sortingAlgorithms.timsort
    import galilei.glob

    safely:
      val base: Path on Linux = root.as[Path on Linux]
      base.glob(Glob.parse(t"**/*.md")).map(_.encode).order: (path: Text) => path.s

    . or(Nil)

  def section(root: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Corpus.Section =

    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    val files: List[Text] = this.files(root, regime.translations)

    def relative(path: Text): Text = path.skip(root.length + 1)

    def read(path: Text): Optional[Text] =
      safely(path.as[Path on Linux]).let: (file: Path on Linux) => safely(file.read[Text])

    def fixture(path: Text): Optional[(Text, Text)] =
      Compilation.fixture(regime, name(root), relative(path))

    def recur(rest: List[Text], docs: List[Doc], skipped: List[Text]): Corpus.Section = rest match
      case path :: tail =>
        val text: Optional[Text] =
          safely(path.as[Path on Linux]).let: (file: Path on Linux) => safely(file.read[Text])

        text match
          case text: Text =>
            val within: Text = relative(path)
            val linker: Linker = regime.linker(name(root), within)

            val parsed: Doc =
              Doc.parse
                ( within, text, checker, regime.session, fixture(path), linker, regime.cache,
                  regime.base(t"${name(root)}/$within"), name(root), regime.glossary, root,
                  regime.sections )

            val problems: List[Block] =
              Translations.check
                ( within, text, Translations.of(path, regime.translations), read,
                  Translations.partial(Translations.within(path, regime.translations)) )

            val doc: Doc = parsed.copy(problems = parsed.problems + problems)

            regime.progress(within, doc)
            recur(tail, docs + List(doc), skipped)

          case _ =>
            recur(tail, docs, skipped + List(path))

      case _ =>
        Corpus.Section(name(root), root, docs, skipped)

    recur(files, Nil, Nil)

  // One document, by its path, compiled as a section's would be, with its fixture if the
  // regime names a directory holding one, in a section named by its directory; `Unset` if the
  // file cannot be read.
  def document(path: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Optional[Doc] =

    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    val directory: Text = Text(path.s.substring(0, path.s.lastIndexOf('/').max(0)).nn)
    val section: Text = name(directory)
    val leaf: Text = Text(path.s.substring(path.s.lastIndexOf('/') + 1).nn)
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

  // A corpus as a language reads it: each document compiled from its translation — the one
  // `Translations.serving` finds, the language's own or a fallback's — as its definitive text
  // was, so that a fence, being the same, is compiled once where the regime keeps what it
  // compiles; and the definitive document itself where nothing translates it. A translated
  // heading keeps the anchor of the heading it translates, so a link to a place in a document
  // means the same in every language. Nothing is reported: what is wrong with a translation
  // is said when the definitive text is compiled.
  def translated(corpus: Corpus, tongue: Text, checker: Checker, regime: Regime): Corpus =
    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    def read(path: Text): Optional[Text] =
      safely(path.as[Path on Linux]).let: (file: Path on Linux) => safely(file.read[Text])

    def anchored(doc: Doc, definitive: Doc): Doc =
      def anchors(blocks: List[Block]): List[Optional[Text]] =
        blocks.bind:
          case Block.Heading(_, _, id) => List(id)
          case _                       => Nil

      val theirs: List[Optional[Text]] = anchors(definitive.blocks)

      if anchors(doc.blocks).size != theirs.size then doc else
        val (blocks, _) =
          doc.blocks.stdlib.foldLeft((List[Block](), theirs)): (state, block) =>
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

        val served: Optional[Text] =
          Translations.serving(path, tongue, complete.translations, read(_).present)

        served.let(read(_)).lay(doc): (source: Text) =>
          val text: Text = Translation.parse(source).foreign.text

          val parsed: Doc =
            Doc.parse
              ( doc.path, text, checker, complete.session,
                fixture(complete, section.name, doc.path),
                complete.linker(section.name, doc.path), complete.cache,
                complete.base(t"${section.name}/${doc.path}"), section.name, complete.glossary,
                section.root, complete.sections )

          anchored(parsed, doc)

      section.copy(docs = docs, skipped = Nil)

    Corpus(sections)

  // A file that could not be read or parsed, as a diagnostic.
  def unreadable(path: Text): Block =
    val message: Block = Block.paragraph(t"The file could not be read.")
    Block.Diagnostic(Tone.Failure, Block.Location(path), List(message))

  def corpus(roots: List[Text], checker: Checker = Checker.tokenized, regime: Regime = Regime())
  :   Corpus =

    val sections: List[(Text, Text)] =
      if !regime.sections.nil then regime.sections
      else roots.map { (root: Text) => (name(root), root) }

    val complete: Regime = regime.copy(sections = sections)
    Corpus(roots.map(section(_, checker, complete)))
