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
  private def term(text: Text): Token = Token(text, Token.Accent.Term)
  private def typal(text: Text): Token = Token(text, Token.Accent.Typal)
  private val space: Token = plain(t" ")

  private def accentOf(isType: Boolean): Token.Accent =
    if isType then Token.Accent.Typal else Token.Accent.Term

  // Tokens between a pair of brackets, and syntaxes separated by commas.
  private def enclosed(open: Text, inner: List[Token], close: Text): List[Token] =
    List(parens(open)) + inner + List(parens(close))

  private def listed(items: List[Syntax], api: Api, scope: Scope): List[Token] =
    joined(items.map(tokens(_, api, scope)), t", ")

  private def captureSet(refs: List[Syntax], api: Api, scope: Scope): List[Token] =
    enclosed(t"{", listed(refs, api, scope), t"}")

  // A designator's tokens: its leaf, or its segments each linked.
  def designator(designator: Designator, api: Api, scope: Scope): List[Token] =
    val isType: Boolean = designator match
      case Designator.Type(_, _) => true
      case _                     => false

    val accent: Token.Accent = accentOf(isType)

    def linked(designator: Designator, accent: Token.Accent): Token =
      val entity: Optional[Api.Entity] =
        api.find(designator).let: (e: Api.Entity) => e.alias.let(api.find).or(e)

      val destination: Optional[Inline.Destination] =
        entity.let: (e: Api.Entity) => Inline.Destination.Local(e.url)

      Token(designator.name, accent, Unset, destination)

    // A name reached through an export — `vacuous.Optional`, which `soundness` exports — is
    // written as the export whose package is in scope or imported, `Optional` or
    // `soundness.Optional`, since that is how the project's sources name it, and what a
    // reader's purview can elide; its link is still the entity's own page.
    def exported(designator: Designator): Optional[Designator] =
      val forwarders: List[Api.Entity] = api.exporters.get(Api.key(designator)).getOrElse(Nil)

      def through(parents: List[Designator]): Optional[Api.Entity] =
        forwarders.seek: (e: Api.Entity) => e.parent.let(parents.has(_)).or(false)

      through(scope.enclosing + scope.standard).or(through(scope.imports)).let: (e: Api.Entity) =>
        if isType then e.tpe else e.term

    designator.parent match
      case parent: Designator =>
        // A leaf alone hints at the whole name it was shortened from.
        if scope.has(parent) then List(linked(designator, accent).copy(hint = designator.qualified))
        else exported(designator).let: (alias: Designator) =>
          this.designator(alias, api, scope) match
            case head :: tail => head.copy(hint = designator.qualified) :: tail
            case _            => Nil

        . or:
          val separator: Text = parent match
            case Designator.Type(_, _) => t"#"
            case _                     => t"."

          // Each segment after the first continues the name, so that it is a link of its own
          // and the name up to it one thing (Pyrocosm's `chained`).
          val next: Token = linked(designator, accent).copy(chained = true)
          val continued: List[Token] = List(symbol(separator).copy(chained = true), next)

          this.designator(parent, api, scope) + continued

      case _ => List(linked(designator, accent))

  private def joined(items: List[List[Token]], separator: Text): List[Token] = items match
    case head :: tail if !tail.nil => head + List(symbol(separator)) + joined(tail, separator)
    case head :: _                 => head
    case _                         => Nil

  // Parenthesised when its precedence is below its context's, as the syntax's own text is.
  private def within(syntax: Syntax, context: Syntax, api: Api, scope: Scope): List[Token] =
    val inner: List[Token] = tokens(syntax, api, scope)
    if syntax.precedence < context.precedence then enclosed(t"(", inner, t")") else inner

  def tokens(syntax: Syntax, api: Api, scope: Scope): List[Token] = syntax match
    case Syntax.Simple(d)        => designator(d, api, scope)
    case Syntax.Value(d)         => designator(d, api, scope) + List(symbol(t"."), keyword(t"type"))
    case Syntax.Symbolic(text)   => List(typal(text))
    case Syntax.Primitive(text)  => List(typal(text))
    case Syntax.Selection(l, r)  => tokens(l, api, scope) + List(symbol(t"."), term(r))
    case Syntax.Prefix(m, r)     => List(symbol(m), plain(t" ")) + within(r, syntax, api, scope)
    case Syntax.Suffix(l, s)     => within(l, syntax, api, scope) + List(symbol(s))
    case Syntax.Compound(parts)  => parts.bind(tokens(_, api, scope))
    case Syntax.Projection(b, t) => within(b, syntax, api, scope) + List(symbol(t"#"), typal(t))

    case Syntax.Infix(l, m, r) =>
      val spaced: Text = if m == t"," then t", " else t" $m "
      within(l, syntax, api, scope) + List(symbol(spaced)) + within(r, syntax, api, scope)

    case Syntax.Named(isUsing, name, tpe) =>
      val prefix: List[Token] = if isUsing then List(keyword(t"using"), plain(t" ")) else Nil
      val named: List[Token] = if name == t"" then Nil else List(term(name), symbol(t":"), space)
      prefix + named + tokens(tpe, api, scope)

    case Syntax.Sequence(style, items) =>
      val (open, close) = style match
        case '(' => (t"(", t")")
        case '[' => (t"[", t"]")
        case _   => (t"{", t"}")

      enclosed(open, listed(items, api, scope), close)

    case Syntax.Application(left, elements, infix) =>
      // An infix alias is written infix when its leaf alone names it.
      val alias: Optional[Text] = left match
        case Syntax.Simple(Designator.Type(parent, name)) if infix && scope.has(parent) => name
        case _                                                                          => Unset

      (alias, elements) match
        case (name: Text, first :: second :: Nil) =>
          val operator: List[Token] = List(space, typal(name), space)
          within(first, syntax, api, scope) + operator + within(second, syntax, api, scope)

        case _ =>
          tokens(left, api, scope) + enclosed(t"[", listed(elements, api, scope), t"]")

    case Syntax.Declaration(_, clauses, result) =>
      clauses.bind(tokens(_, api, scope)) + List(symbol(t":"), space) + tokens(result, api, scope)

    case Syntax.Capturing(base, refs) =>
      val captured: List[Token] = refs.lay(Nil: List[Token])(captureSet(_, api, scope))
      within(base, syntax, api, scope) + List(symbol(t"^")) + captured

    case Syntax.Function(parameters, contextual, impure, refs, result) =>
      val arrow: Text = (if contextual then t"?" else t"") + (if impure then t"=>" else t"->")
      val captures: List[Token] = if refs.nil then Nil else captureSet(refs, api, scope)
      val left: List[Token] = within(parameters, syntax, api, scope)
      left + List(space, symbol(arrow)) + captures + List(space) + tokens(result, api, scope)

    case Syntax.Structural(_, _, _) | Syntax.Match(_, _) =>
      List(Token(syntax.qualified, Token.Accent.Typal))

  // A declaration as one line: its modifiers, its keyword, its name, its clauses, its result
  // (`: T` for a term, ` = T` for an alias, bounds for an abstract type), its parents; an
  // extension's group before its `def`.
  def declaration(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): List[Token] =
    val modifiers: List[Token] = declaration.modifiers.bind: (m: Text) => List(keyword(m), space)
    val named: Token = Token(name, accentOf(declaration.kind.isType), Token.Role.Binding)
    val clauses: List[Token] = declaration.clauses.bind(tokens(_, api, scope))

    val result: List[Token] = declaration.result.lay(Nil: List[Token]): (result: Syntax) =>
      declaration.kind match
        case Api.Kind.Type => result match
          case Syntax.Compound(_) => List(space) + tokens(result, api, scope)
          case _                  => List(space, symbol(t"="), space) + tokens(result, api, scope)

        case _ => List(symbol(t":"), space) + tokens(result, api, scope)

    val parents: List[Token] =
      if declaration.parents.nil then Nil
      else List(space, keyword(t"extends"), space) + listed(declaration.parents, api, scope)

    declaration.kind match
      case Api.Kind.Extension =>
        val head: List[Token] = group(declaration.group, api, scope) + List(space) + modifiers
        head + List(keyword(t"def"), space, named) + clauses + result

      case Api.Kind.Package =>
        List(keyword(t"package"), space, named)

      // A given object provides its first parent, which is how it was written.
      case Api.Kind.Given if declaration.result.absent && !declaration.parents.nil =>
        val provided: List[Token] = tokens(declaration.parents.stdlib.head, api, scope)
        modifiers + List(keyword(t"given"), space, named, symbol(t":"), space) + provided

      case kind =>
        modifiers + List(keyword(kind.keyword), space, named) + clauses + result + parents

  // The clauses an extension extends, after its keyword: `extension (text: String)`.
  def group(group: List[Syntax], api: Api, scope: Scope): List[Token] =
    List(keyword(t"extension"), space) + group.bind(tokens(_, api, scope))

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
      def isUsing(item: Syntax): Boolean = item match
        case Syntax.Named(using, _, _) => using
        case _                         => false

      val contextual: Boolean = !items.nil && items.stdlib.forall(isUsing)

      val rendered: scala.List[List[Token]] = items.stdlib.map:
        case Syntax.Named(_, name, tpe) => tokens(Syntax.Named(false, name, tpe), api, scope)
        case other                      => tokens(other, api, scope)

      Clause(rendered, contextual)

    case _ => Unset

  private def parts(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): Parts =
    val modifiers: List[Token] = declaration.modifiers.bind: (m: Text) => List(keyword(m), space)
    val named: Token = Token(name, accentOf(declaration.kind.isType), Token.Role.Binding)

    // Type parameters stay with the name; term clauses are laid out.
    def isTypeClause(clause: Syntax): Boolean = clause match
      case Syntax.Sequence('[', _) => true
      case _                       => false

    val (typeClauses, termClauses) = declaration.clauses.stdlib.partition(isTypeClause)
    val typed: List[Token] = List.from(typeClauses).bind(tokens(_, api, scope))

    val clauses: scala.List[Clause] =
      termClauses.flatMap: c => clauseOf(c, api, scope).let(scala.List(_)).or(scala.Nil)

    val unmarked: (Optional[Text], List[Token]) = (Unset, Nil: List[Token])

    val (marker, result) = declaration.result.lay(unmarked): (result: Syntax) =>
      declaration.kind match
        case Api.Kind.Type => result match
          case Syntax.Compound(_) => (Unset, tokens(result, api, scope))
          case _                  => (t"=", tokens(result, api, scope))

        case _ => (t":", tokens(result, api, scope))

    val parents: List[Token] =
      if declaration.parents.nil then Nil
      else List(keyword(t"extends"), space) + listed(declaration.parents, api, scope)

    declaration.kind match
      case Api.Kind.Extension =>
        val head: List[Token] = group(declaration.group, api, scope) + List(space) + modifiers
        Parts(head + List(keyword(t"def"), space, named) + typed, clauses, marker, result, Nil)

      case Api.Kind.Package =>
        Parts(List(keyword(t"package"), space, named), scala.Nil, Unset, Nil, Nil)

      // A given object provides its first parent, which is how it was written.
      case Api.Kind.Given if declaration.result.absent && !declaration.parents.nil =>
        val provided: List[Token] = tokens(declaration.parents.stdlib.head, api, scope)
        Parts(modifiers + List(keyword(t"given"), space, named), scala.Nil, t":", provided, Nil)

      case kind =>
        val head: List[Token] = modifiers + List(keyword(kind.keyword), space, named) + typed
        Parts(head, clauses, marker, result, parents)

  // A clause written tight, as part of a line: `(a: A, using b: B)`.
  private def compact(clause: Clause): List[Token] =
    val using: List[Token] = if clause.contextual then List(keyword(t"using"), space) else Nil
    enclosed(t"(", using + joined(List.from(clause.items), t", "), t")")

  // A clause on lines of its own, indented two: `( a: A, b: B )` where it fits, else an item a
  // line, the types aligned, the closing bracket with the last.
  private def formal(clause: Clause, width: Int): scala.List[List[Token]] =
    val using: List[Token] = if clause.contextual then List(keyword(t"using"), space) else Nil
    val open: List[Token] = List(spaces(2), parens(t"("), space) + using
    val items: List[Token] = joined(List.from(clause.items), t", ")
    val oneLine: List[Token] = open + items + List(space, parens(t")"))

    if this.width(oneLine) <= width || clause.items.length <= 1 then scala.List(oneLine) else
      // An item's name is what precedes its `:`; the types begin in one column.
      def split(item: List[Token]): (List[Token], List[Token]) =
        val at: Int = item.stdlib.indexWhere(_.text == t":")
        val none: List[Token] = Nil

        if at < 0 then (none, item)
        else (List.from(item.stdlib.take(at + 1)), List.from(item.stdlib.drop(at + 2)))

      val parts: scala.List[(List[Token], List[Token])] = clause.items.map(split)
      val aligned: Boolean = parts.forall(!_(0).nil)
      val column: Int = parts.map { p => this.width(p(0)) }.maxOption.getOrElse(0)
      val indent: Int = this.width(open)

      parts.zipWithIndex.map: (part, index) =>
        val (name, tpe) = part
        val lead: List[Token] = if index == 0 then open else List(spaces(indent))

        val body: List[Token] =
          if aligned then name + List(spaces(column - this.width(name) + 1)) + tpe
          else name + (if name.nil then Nil else List(space)) + tpe

        val tail: List[Token] =
          if index == parts.length - 1 then List(space, parens(t")")) else List(symbol(t","))

        lead + body + tail

  // A signature on as few lines as fit the width, in Consequent Style: on one line where it
  // fits; else the head with its first clause where that fits, each further clause on lines of
  // its own, the result after its marker and three spaces (`:   Result`) at the head's column,
  // and the parents flush beneath.
  private def layout(parts: Parts, width: Int): scala.List[List[Token]] =
    val bare: List[Token] = if parts.result.nil then Nil else List(space) + parts.result

    val marked: List[Token] =
      parts.marker.lay(bare): (m: Text) => List(symbol(m), space) + parts.result

    val extending: List[Token] = if parts.parents.nil then Nil else List(space) + parts.parents
    val clauses: List[Token] = List.from(parts.clauses).bind(compact)
    val oneLine: List[Token] = parts.head + clauses + marked + extending

    if this.width(oneLine) <= width then scala.List(oneLine) else
      // The first clause stays with the head where the two fit a line.
      val firstFits: Boolean =
        parts.clauses.headOption.exists: clause => this.width(parts.head + compact(clause)) <= width

      val (first, rest) =
        if firstFits then (parts.head + compact(parts.clauses.head), parts.clauses.tail)
        else (parts.head, parts.clauses)

      val clauseLines: scala.List[List[Token]] = rest.flatMap(formal(_, width))
      def introduced(marker: Text): List[Token] = List(symbol(marker), spaces(3))

      val resultLine: scala.List[List[Token]] =
        if parts.result.nil then scala.Nil
        else scala.List(parts.marker.lay(List(spaces(4)))(introduced) + parts.result)

      val parentLine: scala.List[List[Token]] =
        if parts.parents.nil then scala.Nil else scala.List(parts.parents)

      val firstLine: scala.List[List[Token]] = if first.nil then scala.Nil else scala.List(first)
      firstLine ++ clauseLines ++ resultLine ++ parentLine

  // A declaration's signature as the lines of a code block on its page.
  def block
    ( name: Text, declaration: Api.Declaration, api: Api, scope: Scope, width: Int = pageWidth )
  :   Block.Code =

    val lines: scala.List[List[Token]] = layout(parts(name, declaration, api, scope), width)
    Block.Code(Language.Scala, List.from(lines.map(Block.Line(_))))

  // A declaration's clauses and result, without its name, as a snippet for a table's cell: its
  // lines joined by line breaks, which a fixed table keeps.
  def cell(declaration: Api.Declaration, api: Api, scope: Scope, width: Int = cellWidth)
  :   Inline.Snippet =

    val all: Parts = parts(t"", declaration, api, scope)
    val headless: Parts = all.copy(head = Nil)

    // Without a head there is nothing for a clause to be indented beneath: each line comes in by
    // the two columns of that indent.
    def dedent(line: List[Token]): List[Token] = line.stdlib match
      case scala.::(lead, rest) if lead.text.s.isBlank =>
        val indent: Int = lead.text.s.length
        if indent <= 2 then List.from(rest) else List.from(spaces(indent - 2) :: rest)

      case _ => line

    val lines: scala.List[List[Token]] = layout(headless, width).map(dedent)

    val broken: scala.List[Token] = lines.zipWithIndex.flatMap: (line, i) =>
      (if i == 0 then scala.Nil else scala.List(plain(t"\n"))) ++ line.stdlib

    code(List.from(broken))
