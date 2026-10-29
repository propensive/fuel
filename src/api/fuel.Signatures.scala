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

          // Each segment after the first continues the name, so that it is a link of its own
          // and the name up to it one thing (Pyrocosm's `chained`).
          val next: Token = linked(designator, accent).copy(chained = true)
          val continued: List[Token] = List(symbol(separator).copy(chained = true), next)

          this.designator(parent, api, scope) + continued

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
        group(declaration.group, api, scope) + List(plain(t" ")) + modifiers + List(keyword(t"def"), plain(t" "), named) + clauses + result

      case Api.Kind.Package =>
        List(keyword(t"package"), plain(t" "), named)

      // A given object provides its first parent, which is how it was written.
      case Api.Kind.Given if declaration.result.absent && !declaration.parents.nil =>
        modifiers + List(keyword(t"given"), plain(t" "), named, symbol(t":"), plain(t" ")) + tokens(declaration.parents.stdlib.head, api, scope)

      case kind =>
        modifiers + List(keyword(kind.keyword), plain(t" "), named) + clauses + result + parents

  // The clauses an extension extends, after its keyword: `extension (text: String)`.
  def group(group: List[Syntax], api: Api, scope: Scope): List[Token] =
    List(keyword(t"extension"), plain(t" ")) + group.bind(tokens(_, api, scope))

  def code(tokens: List[Token]): Inline.Snippet = Inline.Snippet(Language.Scala, tokens)
  def line(tokens: List[Token]): Block.Code = Block.Code(Language.Scala, List(Block.Line(tokens)))

  // The columns a signature may run to: on a page, in a code block spanning the content, and
  // in the signature column of a member table — the widths the dashboard's panels leave, since
  // the browser's own is never known here.
  val pageWidth: Int = 88
  val cellWidth: Int = 40

  // The parts of a signature which Consequent Style lays out: what comes before the clauses
  // (modifiers, keyword, name and type parameters), each term clause as its items, what
  // follows the colon, and the parents; and whether the result is introduced by `:`, by `=`
  // (an alias) or by nothing (a bound).
  private case class Clause(items: scala.List[List[Token]], contextual: Boolean)

  private case class Parts
    ( head:    List[Token],
      clauses: scala.List[Clause],
      marker:  Optional[Text],
      result:  List[Token],
      parents: List[Token] )

  private def width(tokens: List[Token]): Int = tokens.stdlib.map(_.text.s.length).sum
  private def spaces(count: Int): Token = plain(Text(" ".repeat(count.max(0)).nn))

  // A term clause's items, each without its `using`, which a clause carries once.
  private def clauseOf(sequence: Syntax, api: Api, scope: Scope): Optional[Clause] = sequence match
    case Syntax.Sequence('(', items) =>
      val contextual: Boolean = items.stdlib.forall { case Syntax.Named(using, _, _) => using; case _ => false } && !items.nil
      val rendered: scala.List[List[Token]] = items.stdlib.map:
        case Syntax.Named(_, name, tpe) => tokens(Syntax.Named(false, name, tpe), api, scope)
        case other                      => tokens(other, api, scope)
      Clause(rendered, contextual)
    case _ => Unset

  private def parts(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): Parts =
    val modifiers: List[Token] = declaration.modifiers.bind { (m: Text) => List(keyword(m), plain(t" ")) }
    val named: Token = Token(name, if declaration.kind.isType then Token.Accent.Typal else Token.Accent.Term, Token.Role.Binding)

    // Type parameters stay with the name; term clauses are laid out.
    val (typeClauses, termClauses) = declaration.clauses.stdlib.partition { case Syntax.Sequence('[', _) => true; case _ => false }
    val typed: List[Token] = List.from(typeClauses).bind(tokens(_, api, scope))
    val clauses: scala.List[Clause] = termClauses.flatMap { c => clauseOf(c, api, scope).let(scala.List(_)).or(scala.Nil) }

    val (marker, result): (Optional[Text], List[Token]) = declaration.result.lay((Unset, Nil: List[Token])): (result: Syntax) =>
      declaration.kind match
        case Api.Kind.Type => result match
          case Syntax.Compound(_) => (Unset, tokens(result, api, scope))
          case _                  => (t"=", tokens(result, api, scope))
        case _ => (t":", tokens(result, api, scope))

    val parents: List[Token] =
      if declaration.parents.nil then Nil
      else List(keyword(t"extends"), plain(t" ")) + joined(declaration.parents.map(tokens(_, api, scope)), t", ")

    declaration.kind match
      case Api.Kind.Extension =>
        Parts(group(declaration.group, api, scope) + List(plain(t" ")) + modifiers + List(keyword(t"def"), plain(t" "), named) + typed, clauses, marker, result, Nil)

      case Api.Kind.Package =>
        Parts(List(keyword(t"package"), plain(t" "), named), scala.Nil, Unset, Nil, Nil)

      // A given object provides its first parent, which is how it was written.
      case Api.Kind.Given if declaration.result.absent && !declaration.parents.nil =>
        Parts(modifiers + List(keyword(t"given"), plain(t" "), named), scala.Nil, t":", tokens(declaration.parents.stdlib.head, api, scope), Nil)

      case kind =>
        Parts(modifiers + List(keyword(kind.keyword), plain(t" "), named) + typed, clauses, marker, result, parents)

  // A clause written tight, as part of a line: `(a: A, using b: B)`.
  private def compact(clause: Clause): List[Token] =
    val using: List[Token] = if clause.contextual then List(keyword(t"using"), plain(t" ")) else Nil
    List(parens(t"(")) + using + joined(List.from(clause.items), t", ") + List(parens(t")"))

  // A clause on lines of its own, indented two: `( a: A, b: B )` where it fits, else an item a
  // line, the types aligned, the closing bracket with the last.
  private def formal(clause: Clause, width: Int): scala.List[List[Token]] =
    val using: List[Token] = if clause.contextual then List(keyword(t"using"), plain(t" ")) else Nil
    val open: List[Token] = List(spaces(2), parens(t"("), plain(t" ")) + using
    val oneLine: List[Token] = open + joined(List.from(clause.items), t", ") + List(plain(t" "), parens(t")"))

    if this.width(oneLine) <= width || clause.items.length <= 1 then scala.List(oneLine) else
      // An item's name is what precedes its `:`; the types begin in one column.
      def split(item: List[Token]): (List[Token], List[Token]) =
        val at: Int = item.stdlib.indexWhere(_.text == t":")
        val none: List[Token] = Nil
        if at < 0 then (none, item) else (List.from(item.stdlib.take(at + 1)), List.from(item.stdlib.drop(at + 2)))

      val parts: scala.List[(List[Token], List[Token])] = clause.items.map(split)
      val aligned: Boolean = parts.forall(!_(0).nil)
      val column: Int = parts.map { p => this.width(p(0)) }.maxOption.getOrElse(0)
      val indent: Int = this.width(open)

      parts.zipWithIndex.map: (part, index) =>
        val (name, tpe) = part
        val lead: List[Token] = if index == 0 then open else List(spaces(indent))
        val body: List[Token] = if aligned then name + List(spaces(column - this.width(name) + 1)) + tpe else name + (if name.nil then Nil else List(plain(t" "))) + tpe
        val tail: List[Token] = if index == parts.length - 1 then List(plain(t" "), parens(t")")) else List(symbol(t","))
        lead + body + tail

  // A signature on as few lines as fit the width, in Consequent Style: on one line where it
  // fits; else the head with its first clause where that fits, each further clause on lines of
  // its own, the result after its marker and three spaces (`:   Result`) at the head's column,
  // and the parents flush beneath.
  private def layout(parts: Parts, width: Int): scala.List[List[Token]] =
    val marked: List[Token] = parts.marker.lay(if parts.result.nil then Nil else List(plain(t" ")) + parts.result) { (m: Text) =>
      List(symbol(m), plain(t" ")) + parts.result }
    val extending: List[Token] = if parts.parents.nil then Nil else List(plain(t" ")) + parts.parents
    val oneLine: List[Token] = parts.head + List.from(parts.clauses).bind(compact) + marked + extending

    if this.width(oneLine) <= width then scala.List(oneLine) else
      // The first clause stays with the head where the two fit a line.
      val firstFits: Boolean = parts.clauses.headOption.exists { clause => this.width(parts.head + compact(clause)) <= width }

      val (first, rest): (List[Token], scala.List[Clause]) =
        if firstFits then (parts.head + compact(parts.clauses.head), parts.clauses.tail) else (parts.head, parts.clauses)

      val clauseLines: scala.List[List[Token]] = rest.flatMap(formal(_, width))

      val resultLine: scala.List[List[Token]] =
        if parts.result.nil then scala.Nil
        else scala.List(parts.marker.lay(List(spaces(4)) + parts.result) { (m: Text) => List(symbol(m), spaces(3)) + parts.result })

      val parentLine: scala.List[List[Token]] = if parts.parents.nil then scala.Nil else scala.List(parts.parents)
      (if first.nil then scala.Nil else scala.List(first)) ++ clauseLines ++ resultLine ++ parentLine

  // A declaration's signature as the lines of a code block on its page.
  def block(name: Text, declaration: Api.Declaration, api: Api, scope: Scope, width: Int = pageWidth): Block.Code =
    Block.Code(Language.Scala, List.from(layout(parts(name, declaration, api, scope), width).map(Block.Line(_))))

  // A declaration's clauses and result, without its name, as a snippet for a table's cell: its
  // lines joined by line breaks, which a fixed table keeps.
  def cell(declaration: Api.Declaration, api: Api, scope: Scope, width: Int = cellWidth): Inline.Snippet =
    val all: Parts = parts(t"", declaration, api, scope)
    val headless: Parts = all.copy(head = Nil)
    // Without a head there is nothing for a clause to be indented beneath: each line comes in by
    // the two columns of that indent.
    def dedent(line: List[Token]): List[Token] = line.stdlib match
      case scala.::(lead, rest) if lead.text.s.isBlank =>
        if lead.text.s.length <= 2 then List.from(rest) else List.from(spaces(lead.text.s.length - 2) :: rest)
      case _ => line

    val lines: scala.List[List[Token]] = layout(headless, width).map(dedent)
    code(List.from(lines.zipWithIndex.flatMap { (line, i) => (if i == 0 then scala.Nil else scala.List(plain(t"\n"))) ++ line.stdlib }))
