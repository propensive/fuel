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

import pyrocosm.{Inline, Token, model}
import stenography.Syntax

object Linker:
  // What a reference means, when it says: a type (written `#Name`, or a member of a type) or
  // a term (written `.name`, or a member of a term).
  enum Kind:
    case Type, Term

  // The kind a reference's first character says, if it says one: `#` names a type and `.` a
  // term, each followed by a name as code would write it (`Foo`, `Foo.bar`, `Foo#Bar`), so
  // that a path (`.pyrocosm/fuel/config.tel`) or a selector is code as written, not a
  // reference.
  def kind(reference: Text): Optional[Kind] =
    val named: Boolean = reference.length > 1 && reference.skip(1).match
      case r"[A-Za-z_][\w\x24]*([.#][A-Za-z_][\w\x24]*)*" => true
      case _                                             => false

    if !named then Unset
    else if reference.starts(t"#") then Kind.Type
    else if reference.starts(t".") then Kind.Term
    else Unset

  // A reference which named several entities, as it was written, with the keys of those it
  // named, where they are entities of one API.
  case class Ambiguity(reference: Text, candidates: List[Text] = Nil)

  // A linker which keeps the ambiguous references among those it is asked for, which is how
  // what converts one part of a document — a table — knows which are its own.
  final class Noting(linker: Linker) extends Linker:
    private var noted0: List[Ambiguity] = Nil

    def noted: List[Ambiguity] = noted0.reverse.distinct

    private def note(reference: Text, linked: Boolean): Unit =
      if !linked then linker.ambiguity(reference).let: (ambiguity: Ambiguity) =>
        noted0 = ambiguity :: noted0

    def link(reference: Text): Optional[Inline.Destination] =
      val destination: Optional[Inline.Destination] = linker.link(reference)
      note(reference, destination.present)
      destination

    override def tokens(reference: Text): List[Token] =
      val tokens: List[Token] = linker.tokens(reference)
      note(reference, tokens.exists(_.link.present))
      tokens

    def link(name: Text, tpe: Syntax): Optional[Inline.Destination] = linker.link(name, tpe)
    def learn(imports: List[Text]): Unit = linker.learn(imports)
    def define(anchor: Text, names: List[(Text, Linker.Kind)]): Unit = linker.define(anchor, names)
    def ambiguities: List[Ambiguity] = linker.ambiguities

  // Links nothing: what a document without an API is converted with.
  object none extends Linker:
    def link(reference: Text): Optional[Inline.Destination] = Unset
    def link(name: Text, tpe: Syntax): Optional[Inline.Destination] = Unset
    def learn(imports: List[Text]): Unit = ()
    def define(anchor: Text, names: List[(Text, Linker.Kind)]): Unit = ()
    def ambiguities: List[Ambiguity] = Nil

  // A highlighted token as the model's, with the link its type gives it.
  def token(linker: Linker, token: harlequin.SourceCode.Token): Token =
    val plain: Token = token.model

    token.meta.lay(plain): (meta: harlequin.SourceCode.Token.Meta) =>
      plain.copy(link = linker.link(token.text, meta.tpe))

  def lines(linker: Linker, source: SourceCode): List[pyrocosm.Block.Line] =
    Comments.mark:
      source.lines.to[List].map: (line: List[harlequin.SourceCode.Token]) =>
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

  // The imports a fragment makes at its top level, which extend the scope: `import a.b.*` by
  // every member of `a.b`, and `import a.b.C` by `C` alone.
  def learn(imports: List[Text]): Unit

  // The names a fragment defines at its top level, each a type or a term, by the anchor the
  // fragment has on its page: what a later reference in the same document resolves to before
  // anything else, as a REPL's own definitions shadow a library's; the latest definition of a
  // name wins.
  def define(anchor: Text, names: List[(Text, Linker.Kind)]): Unit

  // The references which named several entities, and so were not linked.
  def ambiguities: List[Linker.Ambiguity]

  // The ambiguity a reference was found to be, when it was asked for.
  def ambiguity(reference: Text): Optional[Linker.Ambiguity] =
    val written: Text = reference.trim
    ambiguities.seek(_.reference == written)

  // A reference as the tokens of inline code: by default one token, linked as a whole; a
  // linker which can, gives a qualified name a linked token for each segment, each after the
  // first `chained` to the name before it.
  def tokens(reference: Text): List[Token] =
    List(Token.plain(reference).copy(link = link(reference)))
