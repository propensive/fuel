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

import pyrocosm.Block

object Fragment:
  // The tactic the derived codec raises through: vouched pure, as a derived instance must be
  // under capture checking (Pyrocosm's `Channel.derivation.throwing` is the same).
  private given throwing: [error <: Hazard] => Tactic[error] =
    scala.caps.unsafe.unsafeAssumePure[Tactic[error]](ThrowTactic[error]())

  import pyrocosm.given

  // The record as BinTEL, under the schema derived from its type; `decode` is `Unset` for bytes
  // which are not one.
  def encode(fragment: Fragment): Data = fragment.bintel

  def decode(data: Data): Optional[Fragment] =
    safely(Bintel.read[Fragment](data))

// One fence's output: its blocks (the code, and the diagnostics which follow it), the
// problems it raised, and what it left to its session — whether it joined it, the names it
// defines, and its top-level imports — so that a later fragment can be compiled after it
// without compiling it again; the text a fragment which follows it is given; and the line
// of the fence it was compiled at, from which its lines are shifted to wherever the fence
// is now.
case class Fragment
  ( blocks:   List[Block],
    problems: List[Block],
    joined:   Boolean,
    defined:  List[Text],
    imports:  List[Text],
    follow:   Text,
    line:     Int )

// Where a fence's output is kept between compiles, by the key of the inputs which produced it.
// A key not kept is `Unset`, and its output is compiled and kept.
trait FragmentCache:
  def fragment(key: Text): Optional[Fragment]
  def fragment(key: Text, fragment: Fragment): Unit
