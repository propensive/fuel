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

import collations.codepointCollation
import dysasymptotics.linearSize
import pyrocosm.Filing
import soundness.collationComparable
import sortingAlgorithms.timsort

// Where the files of the documentation are read from. The documents, the overlays, the
// glossaries and each section's file are named by their paths, as the configuration names
// them, and a library says what is at a path: the filesystem does (`Library.Disk`), and so
// does an Orthant store, for the paths within its working tree, in each language it has
// (`Depot#library`). Nothing which reads a file of the documentation need know which.
object Library:
  // The filesystem, as it is.
  object Disk extends Library:
    def text(path: Text): Optional[Text] = safely(path.as[Path on Linux]).let(Filing.text(_))

    def files(directory: Text, suffix: Text): List[Text] =
      safely(directory.as[Path on Linux]).lay(Nil): (root: Path on Linux) =>
        Filing.beneath(root).map(_.encode).filter(_.ends(suffix)).order(identity(_))

    def directory(path: Text): Boolean =
      safely(path.as[Path on Linux]).lay(false)(Filing.directory(_))

trait Library:
  // The text of the file at a path, if there is one which can be read.
  def text(path: Text): Optional[Text]

  // The files beneath a directory whose names end with a suffix, in the order of their paths.
  def files(directory: Text, suffix: Text): List[Text]

  // Whether a path is a directory.
  def directory(path: Text): Boolean
