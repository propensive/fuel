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

import pyrocosm.Block

// What `fuel compile` writes into its output directory, each record as BinTEL under the schema
// derived from its type: a document's prose, with a sentinel where each fence was; the output
// of one fence; and an API entity's page. Every record is stored once, named by the Blake3
// hash of its own bytes, and the manifest says which inputs produced which.
object Output:
  // The tactic a derived codec raises through: vouched pure, as a derived instance must be
  // under capture checking (Pyrocosm's `Channel.derivation.throwing` is the same).
  object derivation:
    given throwing: [error <: Hazard] => Tactic[error] =
      scala.caps.unsafe.unsafeAssumePure[Tactic[error]](ThrowTactic[error, Any]())

  // One fence's output: its blocks (the code, and the diagnostics which follow it), the
  // problems it raised, and what it left to its session — whether it joined it, the names it
  // defines, and its top-level imports — so that a later fragment can be compiled after it
  // without compiling it again.
  case class Fragment
    ( blocks:   List[Block],
      problems: List[Block],
      joined:   Boolean,
      defined:  List[Text],
      imports:  List[Text] )

  // A document's prose: its blocks, each fence a sentinel to be filled from its fragment, and
  // the problems of its prose alone (references which named several entities).
  case class Prose(blocks: List[Block], problems: List[Block])

  // An API entity's page.
  case class Page(blocks: List[Block])

  // Each record's BinTEL, derived here, once, where Pyrocosm's package-level codecs are in
  // scope as the model's own derivations see them; `Unset` for bytes which are not one.
  object Fragment:
    import derivation.throwing
    import pyrocosm.given

    def encode(fragment: Fragment): Data = fragment.bintel
    def decode(data: Data): Optional[Fragment] = try Bintel.read[Fragment](data) catch case _: Exception => Unset

  object Prose:
    import derivation.throwing
    import pyrocosm.given

    def encode(prose: Prose): Data = prose.bintel
    def decode(data: Data): Optional[Prose] = try Bintel.read[Prose](data) catch case _: Exception => Unset

  object Page:
    import derivation.throwing
    import pyrocosm.given

    def encode(page: Page): Data = page.bintel
    def decode(data: Data): Optional[Page] = try Bintel.read[Page](data) catch case _: Exception => Unset
