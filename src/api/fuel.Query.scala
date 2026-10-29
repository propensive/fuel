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

import scala.collection.immutable as sci

import soundness.{Language as _, Token as _, *}

import pyrocosm.Token

// A search of the API, as the user writes it: whitespace-separated terms, every one a
// constraint which a declaration must satisfy —
//
//   /pkg.name     in that package or object (its key, or beneath it)
//   #   .         a type member; a term member (alone, or glued to a name: `#Text`, `.parse`)
//   ?   ~         a given; an extension
//   +Type         returns a subtype of `Type` (`+List[Int]`, `+Shape`, `+Double`)
//   -Type         takes a parameter whose type is a supertype of `Type` (`-Int` finds `Any`)
//   `name`        exactly this name, symbols and all, never fuzzily (`+`, `::`, `type`)
//   def  inline … a kind (`def`, `val`, `case class`, …) or a modifier it must be declared with
//   word          a name, matched fuzzily
//
// A type is a dotted name with optional arguments (`List[Int]`), a tuple (`(Int, String)`) or
// a function (`Int => String`, `Int ?=> String`); whitespace separates terms except inside
// balanced brackets, quotes and backticks. Several names must all match the one name a
// declaration has. A symbolic name must be written in backticks, since `+ - # . / ? ~` are all
// sigils; a keyword in backticks is a name. `private` is accepted but can match nothing, since
// private members are not documented, which `warnings` says.
object Query:
  enum Form:
    case Type, Term

  object Keyword:
    val all: sci.Map[Text, Keyword] = sci.Map.from(scala.Predef.wrapRefArray(Keyword.values).map { keyword => (keyword.text, keyword) })
    def parse(word: Text): Optional[Keyword] = all.get(word).getOrElse(Unset)

  // Every bare word which is a constraint rather than a name: a kind, or a modifier spelt as
  // `Inspection.modifiers` spells it.
  enum Keyword:
    case Def, Val, Var, Given, Extension, Class, CaseClass, Trait, Object, Enum, Case, Type, Package
    case Inline, Transparent, Private, Protected, Final, Open, Sealed, Abstract, Lazy, Opaque, Infix, Erased, Implicit, Override

    def kinds: sci.Set[Api.Kind] = this match
      case Def       => sci.Set(Api.Kind.Def)
      case Val       => sci.Set(Api.Kind.Val)
      case Var       => sci.Set(Api.Kind.Var)
      case Given     => sci.Set(Api.Kind.Given)
      case Extension => sci.Set(Api.Kind.Extension)
      case Class     => sci.Set(Api.Kind.Class)
      case CaseClass => sci.Set(Api.Kind.CaseClass)
      case Trait     => sci.Set(Api.Kind.Trait)
      case Object    => sci.Set(Api.Kind.Object)
      case Enum      => sci.Set(Api.Kind.Enum)
      case Case      => sci.Set(Api.Kind.Case)
      case Type      => sci.Set(Api.Kind.Type, Api.Kind.TypeParam)
      case Package   => sci.Set(Api.Kind.Package)
      case _         => sci.Set()

    def modifier: Optional[Text] = this match
      case Inline | Transparent | Private | Protected | Final | Open | Sealed | Abstract | Lazy | Opaque | Infix | Erased | Implicit | Override =>
        Text(toString.toLowerCase.nn)
      case _ => Unset

    def text: Text = this match
      case CaseClass => t"case class"
      case other     => Text(other.toString.toLowerCase.nn)

  case class Name(text: Text, exact: Boolean)

  // A type as written in a `+` or `-` term, unresolved: its name and its arguments. A tuple is
  // `scala.TupleN`, a function `scala.FunctionN` and a context function `scala.ContextFunctionN`,
  // as the model shapes them.
  case class TypeQuery(name: Text, args: List[TypeQuery] = Nil):
    def text: Text = if args.nil then name else t"$name[${args.map(_.text).join(t", ")}]"

  object Error:
    enum Reason:
      case Unterminated(open: Char)
      case EmptyOperand(sigil: Char)
      case BareSymbol(text: Text)
      case BadType(text: Text)

      def message: Message = this match
        case Unterminated(open)  => m"the ${open.toString.tt} is not closed"
        case EmptyOperand(sigil) => m"${sigil.toString.tt} needs a type after it"
        case BareSymbol(text)    => m"a symbolic name is written in backticks, as `$text`"
        case BadType(text)       => m"$text is not a type"

  case class Error(reason: Error.Reason, offset: Int)(using Diagnostics) extends soundness.Error(reason.message)

  // A query, or what is wrong with it.
  def read(text: Text): Query | Error = try parse(text) catch case error: Error => error

  // The text as tokens, for a field to show it coloured as it is typed: sigils as symbols,
  // keywords as keywords, types as types, names as terms.
  def tokens(text: Text): List[Token] =
    val s: String = text.s
    val length: Int = s.length
    val tokens: scala.collection.mutable.ListBuffer[Token] = scala.collection.mutable.ListBuffer()

    def add(from: Int, until: Int, accent: Token.Accent): Unit =
      if until > from then tokens += Token(s.substring(from, until).nn.tt, accent)

    def space(index: Int): Int = if index < length && s.charAt(index).isWhitespace then space(index + 1) else index

    def word(index: Int, dotted: Boolean): Int =
      if index < length && (s.charAt(index).isLetterOrDigit || s.charAt(index) == '_' || (dotted && s.charAt(index) == '.')) then word(index + 1, dotted) else index

    def balanced(index: Int, depth: Int, quote: Optional[Char]): Int =
      if index >= length then index else
        val char: Char = s.charAt(index)
        quote match
          case q: Char => balanced(index + 1, depth, if char == q then Unset else q)
          case _ =>
            if char.isWhitespace && depth == 0 then index
            else if char == '(' || char == '[' || char == '{' then balanced(index + 1, depth + 1, Unset)
            else if char == ')' || char == ']' || char == '}' then balanced(index + 1, (depth - 1).max(0), Unset)
            else if char == '"' || char == '`' then balanced(index + 1, depth, char)
            else balanced(index + 1, depth, Unset)

    def recur(index: Int): Unit = if index < length then
      val char: Char = s.charAt(index)
      val gap: Int = space(index)

      if gap > index then
        add(index, gap, Token.Accent.Unparsed)
        recur(gap)
      else char match
        case '/' =>
          add(index, index + 1, Token.Accent.Symbol)
          val start: Int = space(index + 1)
          val end: Int = word(start, true)
          add(index + 1, start, Token.Accent.Unparsed)
          add(start, end, Token.Accent.Term)
          recur(end)

        case '#' | '.' | '?' | '~' =>
          add(index, index + 1, Token.Accent.Symbol)
          recur(index + 1)

        case '+' | '-' =>
          add(index, index + 1, Token.Accent.Symbol)
          val start: Int = space(index + 1)
          val end: Int = balanced(start, 0, Unset)
          add(index + 1, start, Token.Accent.Unparsed)
          add(start, end, Token.Accent.Typal)
          recur(end)

        case '`' =>
          val close: Int = s.indexOf('`', index + 1)
          val end: Int = if close < 0 then length else close + 1
          add(index, end, Token.Accent.String)
          recur(end)

        case char if char.isLetterOrDigit || char == '_' =>
          val end: Int = word(index, false)
          val text: Text = s.substring(index, end).nn.tt
          add(index, end, if Keyword.parse(text).present then Token.Accent.Keyword else Token.Accent.Term)
          recur(end)

        case _ =>
          val end: Int = balanced(index, 0, Unset)
          add(index, end.max(index + 1), Token.Accent.Error)
          recur(end.max(index + 1))

    recur(0)
    List.from(tokens)

  private def parse(text: Text)(using CanThrow[Error]): Query =
    import errorDiagnostics.emptyDiagnostics
    val s: String = text.s
    val length: Int = s.length

    def fail(reason: Error.Reason, offset: Int): Nothing = throw Error(reason, offset)
    def space(char: Char): Boolean = char.isWhitespace
    def wordChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

    def skipSpace(index: Int): Int = if index < length && space(s.charAt(index)) then skipSpace(index + 1) else index

    def word(index: Int, dotted: Boolean): Int =
      if index < length && (wordChar(s.charAt(index)) || (dotted && s.charAt(index) == '.')) then word(index + 1, dotted) else index

    // The end of a run which whitespace ends only outside brackets, quotes and backticks.
    def balanced(index: Int, stack: scala.List[Char]): Int =
      if index >= length then
        if stack.isEmpty then index else fail(Error.Reason.Unterminated(stack.head), index)
      else
        val char: Char = s.charAt(index)
        stack match
          case scala.::(open, rest) if char == open         => balanced(index + 1, rest)
          case scala.::('"' | '`', _)                       => balanced(index + 1, stack)
          case _ if space(char) && stack.isEmpty            => index
          case _ => char match
            case '(' => balanced(index + 1, ')' :: stack)
            case '[' => balanced(index + 1, ']' :: stack)
            case '{' => balanced(index + 1, '}' :: stack)
            case '"' => balanced(index + 1, '"' :: stack)
            case '`' => balanced(index + 1, '`' :: stack)
            case _   => balanced(index + 1, stack)

    // A type: a dotted name with optional arguments, a parenthesised tuple or function type.
    def typeQuery(from: Int, until: Int): TypeQuery =
      val raw: String = s.substring(from, until).nn
      val offset: Int = from

      def bad(): Nothing = fail(Error.Reason.BadType(raw.trim.nn.tt), offset)

      // Splits at the top-level occurrences of a separator, respecting brackets.
      def split(text: String, separator: String): scala.List[String] =
        var depth: Int = 0
        var start: Int = 0
        var i: Int = 0
        val parts: scala.collection.mutable.ListBuffer[String] = scala.collection.mutable.ListBuffer()
        while i < text.length do
          val c: Char = text.charAt(i)
          if c == '(' || c == '[' || c == '{' then depth += 1
          else if c == ')' || c == ']' || c == '}' then depth -= 1
          else if depth == 0 && text.startsWith(separator, i) then
            parts += text.substring(start, i).nn
            i += separator.length - 1
            start = i + 1
          i += 1
        parts += text.substring(start).nn
        parts.toList

      def parse(text0: String): TypeQuery =
        val text: String = text0.trim.nn
        if text.isEmpty then bad()

        // A function type has the lowest precedence; a context function is `?=>`.
        val contextual: scala.List[String] = split(text, "?=>")
        val plain: scala.List[String] = split(text, "=>")

        if contextual.length > 1 then
          val params: scala.List[TypeQuery] = tuple(contextual.head.trim.nn, true)
          TypeQuery(Text(s"scala.ContextFunction${params.length}"), List.from(params :+ parse(contextual.tail.mkString("?=>"))))
        else if plain.length > 1 then
          val params: scala.List[TypeQuery] = tuple(plain.head.trim.nn, true)
          TypeQuery(Text(s"scala.Function${params.length}"), List.from(params :+ parse(plain.tail.mkString("=>"))))
        else if text.startsWith("(") then
          if !text.endsWith(")") then bad()
          val elements: scala.List[TypeQuery] = tuple(text, false)
          if elements.length == 1 then elements.head
          else TypeQuery(Text(s"scala.Tuple${elements.length}"), List.from(elements))
        else
          val end: Int = text.indexWhere { c => !(wordChar(c) || c == '.') } match
            case -1  => text.length
            case end => end
          val name: String = text.substring(0, end).nn
          if name.isEmpty then bad()
          val rest: String = text.substring(end).nn.trim.nn
          if rest.isEmpty then TypeQuery(name.tt)
          else if rest.startsWith("[") && rest.endsWith("]") then
            TypeQuery(name.tt, List.from(split(rest.substring(1, rest.length - 1).nn, ",").map(parse)))
          else bad()

      // The elements of a parenthesised group, or a single unparenthesised type as a group of
      // one (a function's sole parameter).
      def tuple(text: String, loneAllowed: Boolean): scala.List[TypeQuery] =
        if text.startsWith("(") && text.endsWith(")") then
          val inner: String = text.substring(1, text.length - 1).nn
          if inner.trim.nn.isEmpty then scala.Nil else split(inner, ",").map(parse)
        else if loneAllowed then scala.List(parse(text))
        else bad()

      parse(raw)

    def recur(index: Int, query: Query): Query =
      if index >= length then query else
        val char: Char = s.charAt(index)

        def operand(sigil: Char): (Int, Int) =
          val start: Int = skipSpace(index + 1)
          val end: Int = balanced(start, scala.Nil)
          if end == start then fail(Error.Reason.EmptyOperand(sigil), index)
          (start, end)

        if space(char) then recur(index + 1, query)
        else char match
          case '/' =>
            val start: Int = skipSpace(index + 1)
            val end: Int = word(start, true)
            if end == start then fail(Error.Reason.EmptyOperand('/'), index)
            recur(end, query.copy(packages = query.packages + List(s.substring(start, end).nn.tt)))

          case '#' => recur(index + 1, query.copy(form = Form.Type))
          case '.' => recur(index + 1, query.copy(form = Form.Term))
          case '?' => recur(index + 1, query.copy(kinds = query.kinds + Api.Kind.Given))
          case '~' => recur(index + 1, query.copy(kinds = query.kinds + Api.Kind.Extension))

          case '+' =>
            val (start, end) = operand('+')
            recur(end, query.copy(results = query.results + List(typeQuery(start, end))))

          case '-' =>
            val (start, end) = operand('-')
            recur(end, query.copy(parameters = query.parameters + List(typeQuery(start, end))))

          case '`' =>
            val close: Int = s.indexOf('`', index + 1)
            if close < 0 then fail(Error.Reason.Unterminated('`'), index)
            recur(close + 1, query.copy(names = query.names + List(Name(s.substring(index + 1, close).nn.tt, true))))

          case char if wordChar(char) =>
            val end: Int = word(index, false)
            val text: Text = s.substring(index, end).nn.tt

            // `case class` is one keyword.
            val next: Int = skipSpace(end)
            val afterNext: Int = word(next, false)

            if text == t"case" && next > end && s.substring(next, afterNext) == "class" then
              recur(afterNext, query.copy(kinds = query.kinds ++ Keyword.CaseClass.kinds))
            else Keyword.parse(text).lay(recur(end, query.copy(names = query.names + List(Name(text, false))))): keyword =>
              recur(end, query.copy(kinds = query.kinds ++ keyword.kinds, modifiers = keyword.modifier.lay(query.modifiers)(query.modifiers + _)))

          case _ =>
            val end: Int = balanced(index, scala.Nil)
            fail(Error.Reason.BareSymbol(s.substring(index, end).nn.tt), index)

    recur(0, Query())

case class Query
  ( packages:   List[Text]            = Nil,
    form:       Optional[Query.Form]  = Unset,
    kinds:      sci.Set[Api.Kind]     = sci.Set(),
    modifiers:  sci.Set[Text]         = sci.Set(),
    names:      List[Query.Name]      = Nil,
    results:    List[Query.TypeQuery] = Nil,
    parameters: List[Query.TypeQuery] = Nil ):

  def empty: Boolean =
    packages.nil && form.absent && kinds.isEmpty && modifiers.isEmpty && names.nil && results.nil && parameters.nil

  // What can be said of a query before it is run.
  def warnings: List[Text] =
    if modifiers.contains(t"private") then List(t"Private members are not documented, so nothing can match `private`.") else Nil
