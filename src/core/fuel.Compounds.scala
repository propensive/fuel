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

import dysasymptotics.{linearAccess, linearSize}

// What a compound of a parsed TEL document says, as Fuel reads it: its inline atoms, its source
// atom, and the compounds beneath it, in the order written.
object Compounds:
  def inline(compound: Tel.Compound): List[Text] =
    compound.atoms.to[List].bind:
      case Tel.Atom.Inline(text, _) => List(text)
      case _                        => Nil

  def source(compound: Tel.Compound): Optional[Text] =
    compound.atoms.to[List].reap { case Tel.Atom.Source(text) => text }

  def children(compound: Tel.Compound): List[Tel.Compound] =
    compound.children.to[List].bind(_.compounds.to[List])

  // The line (from 1) of the keyword of the compound at `path` in `tel`, which must have been
  // read with its positions; 1 where it has none.
  def line(tel: Tel, path: List[Text]): Int =
    tel.locateKey(Telp(path)).lay(1)(_.line)

  // Each of `compounds`, the children of the compound at `at`, with its own path: its keyword,
  // and its occurrence (from 0) among the children before it with that keyword.
  def paths(at: List[Text], compounds: List[Tel.Compound]): List[(Tel.Compound, List[Text])] =
    compounds.indexed.map: (compound: Tel.Compound, index: Ordinal) =>
      val occurrence: Int = compounds.keep(index.n0).count(_.keyword == compound.keyword)
      (compound, at + List(compound.keyword, occurrence.show))
