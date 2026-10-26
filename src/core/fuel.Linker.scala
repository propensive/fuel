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

import pyrocosm.{Inline, Token, model}
import stenography.Syntax

// Where a reference in a document leads: the hook by which inline code (`` `Text` ``) and the
// typed tokens of a fragment are linked to the page which documents what they name, when a
// model of the API is at hand to say that such a page exists. A document is converted with
// the linker its scope gives it — its enclosing entity, its imports and the project's — and
// learns the imports its own fragments make; `fuel-core` knows only the hook, and `none`.
trait Linker:
  // A reference as written in prose: a name, qualified or not, as the scope resolves it.
  def link(reference: Text): Optional[Inline.Destination]

  // A token of highlighted code, by its text and the type the compiler gave it: linked only
  // when the token *is* the name of what the type names, so that a value whose type is `Int`
  // does not link to `Int`.
  def link(name: Text, tpe: Syntax): Optional[Inline.Destination]

  // The imports a fragment makes at its top level (`import a.b.*`), which extend the scope.
  def learn(imports: List[Text]): Unit

  // The references which named several entities, and so were not linked.
  def ambiguities: List[Text]

  // A reference as the tokens of inline code: by default one token, linked as a whole; a
  // linker which can, gives a qualified name a linked token for each segment, each after the
  // first `chained` to the name before it.
  def tokens(reference: Text): List[Token] =
    List(Token.plain(reference).copy(link = link(reference)))

object Linker:
  // Links nothing: what a document without an API is converted with.
  object none extends Linker:
    def link(reference: Text): Optional[Inline.Destination] = Unset
    def link(name: Text, tpe: Syntax): Optional[Inline.Destination] = Unset
    def learn(imports: List[Text]): Unit = ()
    def ambiguities: List[Text] = Nil

  // A highlighted token as the model's, with the link its type gives it.
  def token(linker: Linker, token: harlequin.Token): Token =
    val plain: Token = token.model
    token.meta.lay(plain) { (meta: harlequin.Token.Meta) => plain.copy(link = linker.link(token.text, meta.tpe)) }

  def lines(linker: Linker, source: SourceCode): List[pyrocosm.Block.Line] =
    source.lines.to[List].map: (line: List[harlequin.Token]) =>
      pyrocosm.Block.Line(chain(line.map(token(linker, _))))

  // A line of code with each qualified name chained: a linked token, a `.` or `#`, and a linked
  // token whose page is a member of the first's — `com`, `.`, `example` — continue the name the
  // first began, however many segments it has. Pages nest as names do (`/api/com.example` is a
  // member of `/api/com`), which is what says one segment qualifies the next.
  def chain(tokens: List[Token]): List[Token] =
    def path(token: Token): Optional[Text] = token.link match
      case Inline.Destination.Local(path, _) => path
      case _                                 => Unset

    def nested(outer: Text, inner: Text): Boolean =
      inner.starts(t"$outer.") || inner.starts(t"$outer~")

    def member(outer: Token, inner: Token): Boolean =
      path(outer).let { (outer: Text) => path(inner).let(nested(outer, _)) }.or(Unset).or(false)

    def recur(tokens: List[Token], done: List[Token]): List[Token] = tokens match
      case left :: separator :: right :: rest
      if (separator.text == t"." || separator.text == t"#") && member(left, right) =>
        val chained: Token = right.copy(chained = true)
        recur(chained :: rest, separator.copy(chained = true) :: left :: done)

      case head :: rest =>
        recur(rest, head :: done)

      case _ =>
        done.reverse

    recur(tokens, Nil)
