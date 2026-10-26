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

import soundness.{Language as _, *}

import pyrocosm.Block

// A fence's output moved to wherever the fence now is: its code blocks' numbering and its
// diagnostics' lines in this document shifted by the lines its fence moved, which is what lets
// prose change around a fence without the fence being compiled again. A diagnostic in another
// file — a fixture's — stays where it is.
object Relocation:
  def shift(blocks: List[Block], path: Text, delta: Int): List[Block] =
    if delta == 0 then blocks else blocks.map(shift(_, path, delta))

  def shift(block: Block, path: Text, delta: Int): Block = block match
    case code: Block.Code =>
      code.copy(start = code.start.let(_ + delta))

    case diagnostic: Block.Diagnostic =>
      val location: Optional[Block.Location] =
        diagnostic.location.let: (location: Block.Location) =>
          if location.path != path then location
          else location.copy(line = location.line.let(_ + delta))

      diagnostic.copy
        ( location = location,
          message  = shift(diagnostic.message, path, delta),
          excerpt  = shift(diagnostic.excerpt, path, delta),
          detail   = shift(diagnostic.detail, path, delta) )

    case Block.Group(content)     => Block.Group(shift(content, path, delta))
    case Block.Quotation(content) => Block.Quotation(shift(content, path, delta))

    case Block.Listing(ordered, items) =>
      val shifted: List[Block.Item] = items.map: (item: Block.Item) =>
        item.copy(content = shift(item.content, path, delta))

      Block.Listing(ordered, shifted)

    case other =>
      other
