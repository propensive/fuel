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
  // Parses and converts one document, its Scala fragments checked by `checker` — in one
  // session, each following the ones before it, when `session` says so, with the fixture
  // (its path and text) compiled first; `problems` are the fragments which did not do as
  // their preambles said.
  def parse
    ( path:    Text,
      text:    Text,
      checker: Checker                  = Checker.tokenized,
      session: Boolean                  = false,
      fixture: Optional[(Text, Text)]   = Unset,
      linker:  Linker                   = Linker.none )
  :   Doc =

    val scala: Embedding.ScalaCode = Embedding.ScalaCode(checker, path, session, fixture, linker)
    val blocks: List[Block] = Blocks.of(Parser.parse(text), List(scala, Embedding.JavaCode), linker)

    // A reference which named several entities is a caution: it is shown unlinked.
    val ambiguous: List[Block] =
      linker.ambiguities.map: (reference: Text) =>
        Block.Diagnostic(Tone.Warning, Block.Location(path), List(Block.paragraph(t"`$reference` names more than one entity, so it is not linked; qualify it.")))

    Doc(path, Blocks.title(blocks).or(filename(path)), blocks, scala.problems + ambiguous)

  // The last segment of a path, without a `.md` extension.
  def filename(path: Text): Text =
    val name: Text = path.cut(t"/").reverse.prim.or(path)
    if name.ends(t".md") then Text(name.s.substring(0, name.s.length - 3).nn) else name

// One Markdown document, compiled: its path relative to the directory it was found under, its
// title (the first level-one heading, else its filename), its blocks, and the problems its
// fragments had, each a diagnostic at its place.
case class Doc(path: Text, title: Text, blocks: List[Block], problems: List[Block] = Nil)
