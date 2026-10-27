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

import dysasymptotics.linearSize
import pyrocosm.{Block, Inline, Tone}

// The compile step: every `*.md` beneath a document directory, read, parsed and converted.
object Compilation:
  // How a corpus's fragments are compiled: as one session per document, each fragment
  // following the ones before it, and with the document's fixture (`<fixtures>/<name>.scala`)
  // first.
  // `linker` gives each document, by its section and its path, the linker its references
  // resolve through; with a `cache`, a document's prose and each of its fences is compiled only
  // when the key of its inputs is not kept, `base` giving the key of everything a document
  // depends on beyond its own text, by its path; `glossary` has the terms its prose may name.
  case class Regime
    ( session:  Boolean                  = false,
      fixtures: Optional[Text]           = Unset,
      progress: (Text, Int) => Unit      = (_, _) => (),
      linker:   (Text, Text) => Linker   = (_: Text, _: Text) => Linker.none,
      cache:    Optional[Cache]          = Unset,
      base:     Text => Text             = (_: Text) => t"",
      glossary: Glossary                 = Glossary.none )

  // A section's name is its directory's last segment.
  def name(root: Text): Text = root.cut(t"/").filter(_ != t"").reverse.prim.or(root)

  def section(root: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime()): Corpus.Section =
    import filesystemBackends.javaBaseFilesystem
    import filesystemOptions.dereferenceSymlinks
    import sortingAlgorithms.timsort
    import galilei.glob
    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    val files: List[Text] =
      safely:
        val base: Path on Linux = root.as[Path on Linux]
        base.glob(Glob.parse(t"**/*.md")).map(_.encode).order: (path: Text) => path.s

      . or(Nil)

    def relative(path: Text): Text = path.skip(root.length + 1)

    // The document's fixture, when the regime names a directory and the file is there.
    def fixture(path: Text): Optional[(Text, Text)] =
      regime.fixtures.let: (directory: Text) =>
        val file: Text = t"$directory/${Doc.filename(relative(path))}.scala"

        safely(file.as[Path on Linux]).let { (file: Path on Linux) => safely(file.read[Text]) }.let:
          (text: Text) => (file, text)

    def recur(rest: List[Text], docs: List[Doc], skipped: List[Text]): Corpus.Section = rest match
      case path :: tail =>
        val text: Optional[Text] =
          safely(path.as[Path on Linux]).let: (file: Path on Linux) => safely(file.read[Text])

        text match
          case text: Text =>
            val doc: Doc =
              Doc.parse
                ( relative(path), text, checker, regime.session, fixture(path),
                  regime.linker(name(root), relative(path)), regime.cache, regime.base(relative(path)),
                  name(root), regime.glossary )
            regime.progress(relative(path), doc.problems.size)
            recur(tail, docs + List(doc), skipped)

          case _ =>
            recur(tail, docs, skipped + List(path))

      case _ =>
        Corpus.Section(name(root), root, docs, skipped)

    recur(files, Nil, Nil)

  // One document, by its path, compiled as a section's would be, with its fixture if the
  // regime names a directory holding one, in a section named by its directory; `Unset` if the
  // file cannot be read.
  def document(path: Text, checker: Checker = Checker.tokenized, regime: Regime = Regime()): Optional[Doc] =
    import filesystemBackends.javaBaseFilesystem
    import charsets.utf8Charset
    import textSanitizers.skipSanitizer

    val fixture: Optional[(Text, Text)] =
      regime.fixtures.let: (directory: Text) =>
        val file: Text = t"$directory/${Doc.filename(path)}.scala"

        safely(file.as[Path on Linux]).let { (file: Path on Linux) => safely(file.read[Text]) }.let:
          (text: Text) => (file, text)

    val directory: Text = Text(path.s.substring(0, path.s.lastIndexOf('/').max(0)).nn)
    val section: Text = name(directory)

    safely(path.as[Path on Linux]).let { (file: Path on Linux) => safely(file.read[Text]) }.let:
      (text: Text) =>
        Doc.parse
          ( path, text, checker, regime.session, fixture, regime.linker(section, path), regime.cache,
            regime.base(path), section, regime.glossary )

  // A file that could not be read or parsed, as a diagnostic.
  def unreadable(path: Text): Block =
    Block.Diagnostic(Tone.Failure, Block.Location(path), List(Block.paragraph(t"The file could not be read.")))

  def corpus(roots: List[Text], checker: Checker = Checker.tokenized, regime: Regime = Regime()): Corpus =
    Corpus(roots.map(section(_, checker, regime)))
