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

import java.nio.file as jnf

import soundness.*

import alphabets.hexLowerCase
import providers.soundnessProvider

// The hashes the output is keyed by: Blake3, as TEL's own value hashes are, written as hex. A
// key is the hash of the hashes and texts it depends on, each in its place, so that a change to
// any of them changes it; a classpath's is the hash of each entry's bytes — a jar's, or a
// directory's files, by their paths — in the classpath's order. In time a classpath will be
// hashed by its API (a LIRA analysis of its TASTy), so that a change which leaves the API as it
// was recompiles nothing; until that is available, its bytes stand for it.
object Hashes:
  // The parts as one text, each prefixed with its length, so that no two lists of parts make
  // the same text.
  def of(parts: Text*): Text =
    val prefixed: Text = List(parts*).map { (part: Text) => t"${part.length.show}:$part" }.join(t"")
    prefixed.digest[Blake3].serialize[Hex]

  def of(data: Data): Text = data.digest[Blake3].serialize[Hex]

  // A file's bytes, or a directory's files — each by its path relative to the directory and
  // its bytes, in the order of their paths — or nothing for a path which is neither.
  def path(entry: Text): Text =
    val root: jnf.Path = jnf.Path.of(entry.s).nn

    if jnf.Files.isRegularFile(root) then of(Array.unsafeFrozen(jnf.Files.readAllBytes(root).nn))
    else if !jnf.Files.isDirectory(root) then of(t"absent", entry)
    else
      val stream = jnf.Files.walk(root).nn

      val files: scala.List[jnf.Path] =
        try stream.iterator.nn.asScala.filter(jnf.Files.isRegularFile(_)).toList.sortBy(_.toString)
        finally stream.close()

      val parts: scala.List[Text] =
        files.flatMap: (file: jnf.Path) =>
          val bytes: Data = Array.unsafeFrozen(jnf.Files.readAllBytes(file).nn)
          scala.List(root.relativize(file).nn.toString.tt, of(bytes))

      of(parts*)

  def classpath(entries: List[Text]): Text = of(entries.map(path)*)
