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

import dysasymptotics.linearSize
import pyrocosm.{Block, Inline, Language, Token}
import stenography.{Designator, Syntax}

// A signature as the model's tokens, from stenography's syntax: each designator linked to its
// page where one exists, and qualified only as far as the scope needs (a name whose parent is
// enclosing, imported or standard is its leaf alone; any other is every segment, each linked
// where it exists), as Amok's `syntaxIsRenderable` rendered it. The one place a signature's
// shape is decided, so that another way of showing one can be tried here alone.
object Signatures:
  private def symbol(text: Text): Token = Token(text, Token.Accent.Symbol)
  private def parens(text: Text): Token = Token(text, Token.Accent.Parens)
  private def keyword(text: Text): Token = Token(text, Token.Accent.Keyword)
  private def plain(text: Text): Token = Token.plain(text)

  // A designator's tokens: its leaf, or its segments each linked.
  def designator(designator: Designator, api: Api, scope: Scope): List[Token] =
    val isType: Boolean = designator.isInstanceOf[Designator.Type]
    val accent: Token.Accent = if isType then Token.Accent.Typal else Token.Accent.Term

    def linked(designator: Designator, accent: Token.Accent): Token =
      val entity: Optional[Api.Entity] = api.find(designator).let { (e: Api.Entity) => e.alias.let(api.find).or(e) }
      Token(designator.name, accent, Unset, entity.let { (e: Api.Entity) => Inline.Destination.Local(e.url) })

    designator.parent match
      case Unset => List(linked(designator, accent))
      case parent: Designator =>
        if scope.has(parent) then List(linked(designator, accent))
        else
          val separator: Text = parent match
            case Designator.Type(_, _) => t"#"
            case _                     => t"."

          this.designator(parent, api, scope) + List(symbol(separator), linked(designator, accent))

  private def joined(items: List[List[Token]], separator: Text): List[Token] = items match
    case head :: tail if !tail.nil => head + List(symbol(separator)) + joined(tail, separator)
    case head :: _                 => head
    case _                         => Nil

  // Parenthesised when its precedence is below its context's, as the syntax's own text is.
  private def within(syntax: Syntax, context: Syntax, api: Api, scope: Scope): List[Token] =
    val inner: List[Token] = tokens(syntax, api, scope)
    if syntax.precedence < context.precedence then List(parens(t"(")) + inner + List(parens(t")")) else inner

  def tokens(syntax: Syntax, api: Api, scope: Scope): List[Token] = syntax match
    case Syntax.Simple(d)        => designator(d, api, scope)
    case Syntax.Value(d)         => designator(d, api, scope) + List(symbol(t"."), keyword(t"type"))
    case Syntax.Symbolic(text)   => List(Token(text, Token.Accent.Typal))
    case Syntax.Primitive(text)  => List(Token(text, Token.Accent.Typal))
    case Syntax.Selection(l, r)  => tokens(l, api, scope) + List(symbol(t"."), Token(r, Token.Accent.Term))
    case Syntax.Prefix(m, r)     => List(symbol(m), plain(t" ")) + within(r, syntax, api, scope)
    case Syntax.Suffix(l, s)     => within(l, syntax, api, scope) + List(symbol(s))
    case Syntax.Compound(parts)  => parts.bind(tokens(_, api, scope))
    case Syntax.Projection(b, t) => within(b, syntax, api, scope) + List(symbol(t"#"), Token(t, Token.Accent.Typal))

    case Syntax.Infix(l, m, r) =>
      val spaced: Text = if m == t"," then t", " else t" $m "
      within(l, syntax, api, scope) + List(symbol(spaced)) + within(r, syntax, api, scope)

    case Syntax.Named(isUsing, name, tpe) =>
      val prefix: List[Token] = if isUsing then List(keyword(t"using"), plain(t" ")) else Nil
      val named: List[Token] = if name == t"" then Nil else List(Token(name, Token.Accent.Term), symbol(t":"), plain(t" "))
      prefix + named + tokens(tpe, api, scope)

    case Syntax.Sequence(style, items) =>
      val (open, close): (Text, Text) = style match
        case '(' => (t"(", t")")
        case '[' => (t"[", t"]")
        case _   => (t"{", t"}")

      List(parens(open)) + joined(items.map(tokens(_, api, scope)), t", ") + List(parens(close))

    case Syntax.Application(left, elements, infix) =>
      // An infix alias is written infix when its leaf alone names it.
      (left, elements) match
        case (Syntax.Simple(Designator.Type(parent, name)), first :: second :: Nil) if infix && scope.has(parent) =>
          within(first, syntax, api, scope) + List(plain(t" "), Token(name, Token.Accent.Typal), plain(t" ")) + within(second, syntax, api, scope)

        case _ =>
          tokens(left, api, scope) + List(parens(t"[")) + joined(elements.map(tokens(_, api, scope)), t", ") + List(parens(t"]"))

    case Syntax.Declaration(_, clauses, result) =>
      clauses.bind(tokens(_, api, scope)) + List(symbol(t":"), plain(t" ")) + tokens(result, api, scope)

    case Syntax.Capturing(base, refs) =>
      within(base, syntax, api, scope) + List(symbol(t"^")) + refs.lay(Nil: List[Token]): (refs: List[Syntax]) =>
        List(parens(t"{")) + joined(refs.map(tokens(_, api, scope)), t", ") + List(parens(t"}"))

    case Syntax.Function(parameters, contextual, impure, refs, result) =>
      val arrow: Text = (if contextual then t"?" else t"") + (if impure then t"=>" else t"->")
      val captures: List[Token] = if refs.nil then Nil else List(parens(t"{")) + joined(refs.map(tokens(_, api, scope)), t", ") + List(parens(t"}"))
      within(parameters, syntax, api, scope) + List(plain(t" "), symbol(arrow)) + captures + List(plain(t" ")) + tokens(result, api, scope)

    case Syntax.Structural(_, _, _) | Syntax.Match(_, _) =>
      List(Token(syntax.qualified, Token.Accent.Typal))

  // A declaration as one line: its modifiers, its keyword, its name, its clauses, its result
  // (`: T` for a term, ` = T` for an alias, bounds for an abstract type), its parents; an
  // extension's group before its `def`.
  def declaration(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): List[Token] =
    val modifiers: List[Token] = declaration.modifiers.bind { (m: Text) => List(keyword(m), plain(t" ")) }
    val named: Token = Token(name, if declaration.kind.isType then Token.Accent.Typal else Token.Accent.Term, Token.Role.Binding)
    val clauses: List[Token] = declaration.clauses.bind(tokens(_, api, scope))

    val result: List[Token] = declaration.result.lay(Nil: List[Token]): (result: Syntax) =>
      declaration.kind match
        case Api.Kind.Type => result match
          case Syntax.Compound(_) => List(plain(t" ")) + tokens(result, api, scope)
          case _                  => List(plain(t" "), symbol(t"="), plain(t" ")) + tokens(result, api, scope)
        case _ => List(symbol(t":"), plain(t" ")) + tokens(result, api, scope)

    val parents: List[Token] =
      if declaration.parents.nil then Nil
      else List(plain(t" "), keyword(t"extends"), plain(t" ")) + joined(declaration.parents.map(tokens(_, api, scope)), t", ")

    declaration.kind match
      case Api.Kind.Extension =>
        val group: List[Token] = declaration.group.bind(tokens(_, api, scope))
        List(keyword(t"extension"), plain(t" ")) + group + List(plain(t" ")) + modifiers + List(keyword(t"def"), plain(t" "), named) + clauses + result

      case Api.Kind.Package =>
        List(keyword(t"package"), plain(t" "), named)

      case kind =>
        modifiers + List(keyword(kind.keyword), plain(t" "), named) + clauses + result + parents

  def code(tokens: List[Token]): Inline.Snippet = Inline.Snippet(Language.Scala, tokens)
  def line(tokens: List[Token]): Block.Code = Block.Code(Language.Scala, List(Block.Line(tokens)))
