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

// The compile step: every `*.md` beneath a document directory, read, parsed and converted.
object Compilation:
  // A section's name is its directory's last segment.
  def name(root: Text): Text = root.cut(t"/").filter(_ != t"").reverse.prim.or(root)

  def section(root: Text, checker: Checker = Checker.tokenized): Corpus.Section =
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

    def recur(rest: List[Text], docs: List[Doc], skipped: List[Text]): Corpus.Section = rest match
      case path :: tail =>
        val text: Optional[Text] =
          safely(path.as[Path on Linux]).let: (file: Path on Linux) => safely(file.read[Text])

        text match
          case text: Text =>
            recur(tail, docs + List(Doc.parse(relative(path), text, checker)), skipped)

          case _ =>
            recur(tail, docs, skipped + List(path))

      case _ =>
        Corpus.Section(name(root), root, docs, skipped)

    recur(files, Nil, Nil)

  def corpus(roots: List[Text], checker: Checker = Checker.tokenized): Corpus =
    Corpus(roots.map(section(_, checker)))
