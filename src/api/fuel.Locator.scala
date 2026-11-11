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

import stenography.Designator

// Where an entity is in an API of any format: its name, beneath its parent's, in one of the two
// forms a name may have — as a term or as a type — which is what a member written after it
// belongs to: `a.B#c` is the `c` of the type `B` of `a`, and `a.B.c` that of the term. The
// last name's own form is not written, and is a term's unless it is said. A format which has
// one form alone uses the term's. Unlike Scala's `Designator`, which names what the JVM has,
// a name at the top has both forms, so that `schema#field` is a member of a schema's type
// form, as `schema.Record` is one of its term form; and a `Designator` is a locator, a
// top-level name a term.
object Locator:
  def apply(text: Text): Locator =
    def recur(index: Int, start: Int, parent: Optional[Locator]): Locator =
      def next: Text = text.s.substring(start, index).nn.tt

      if index >= text.s.length then Locator(parent, next)
      else text.s.charAt(index) match
        case '.' => recur(index + 1, index + 1, Locator(parent, next, false))
        case '#' => recur(index + 1, index + 1, Locator(parent, next, true))
        case _   => recur(index + 1, start, parent)

    recur(0, 0, Unset)

  def of(designator: Designator): Locator = designator match
    case Designator.Top(name)          => Locator(Unset, name)
    case Designator.Term(parent, name) => Locator(of(parent), name, false)
    case Designator.Type(parent, name) => Locator(of(parent), name, true)

  given fromDesignator: Conversion[Designator, Locator] = of(_)

case class Locator(parent: Optional[Locator], name: Text, isType: Boolean = false):
  def child(name: Text, isType: Boolean = false): Locator = Locator(this, name, isType)
  def term: Locator = if isType then copy(isType = false) else this
  def tpe: Locator = if isType then this else copy(isType = true)

  // What separates the name from a member's: `#` after a type, `.` after a term.
  def separator: Text = if isType then t"#" else t"."

  def qualified: Text =
    parent.lay(name): (parent: Locator) => t"${parent.qualified}${parent.separator}$name"

  // The same, with the top-level name a term, for a format in which it has no other form.
  def rooted: Locator =
    parent.lay(if isType then copy(isType = false) else this): (parent: Locator) =>
      val above: Locator = parent.rooted
      if above eq parent then this else copy(parent = above)

  // As Scala's designator, which a top-level name is of in one form.
  def designator: Designator = parent match
    case parent: Locator =>
      if isType then Designator.Type(parent.designator, name)
      else Designator.Term(parent.designator, name)

    case _ =>
      Designator.Top(name)
