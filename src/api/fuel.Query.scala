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

import soundness.{Token as _, *}

import dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics
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
    val all: Map[Text, Keyword] =
      List.range(0, Keyword.values.length).map(Keyword.fromOrdinal).map: keyword =>
        (keyword.text, keyword)

      . to[Map]

    def parse(word: Text): Optional[Keyword] = all.at(word)

  // Every bare word which is a constraint rather than a name: a kind, or a modifier spelt as
  // `Inspection.modifiers` spells it.
  enum Keyword:
    case Def, Val, Var, Given, Extension, Class, CaseClass, Trait, Object, Enum, Case, Type, Package
    case Inline, Transparent, Private, Protected, Final, Open, Sealed, Abstract, Lazy, Opaque, Infix
    case Erased, Implicit, Override

    def kinds: Set[Api.Kind] = this match
      case Def       => Set(Api.Kind.Def)
      case Val       => Set(Api.Kind.Val)
      case Var       => Set(Api.Kind.Var)
      case Given     => Set(Api.Kind.Given)
      case Extension => Set(Api.Kind.Extension)
      case Class     => Set(Api.Kind.Class)
      case CaseClass => Set(Api.Kind.CaseClass)
      case Trait     => Set(Api.Kind.Trait)
      case Object    => Set(Api.Kind.Object)
      case Enum      => Set(Api.Kind.Enum)
      case Case      => Set(Api.Kind.Case)
      case Type      => Set(Api.Kind.Type, Api.Kind.TypeParam)
      case Package   => Set(Api.Kind.Package)
      case _         => Set()

    private def lower: Text = toString.tt.lower

    def modifier: Optional[Text] = this match
      case Inline | Transparent | Private | Protected | Final | Open | Sealed  => lower
      case Abstract | Lazy | Opaque | Infix | Erased | Implicit | Override     => lower
      case _                                                                   => Unset

    def text: Text = this match
      case CaseClass => t"case class"
      case _         => lower

  case class Name(text: Text, exact: Boolean)

  // A bare word of a format other than Scala, which its `vocabulary` has: the kinds it says,
  // or the modifier.
  case class Word(kinds: Set[Api.Kind] = Set(), modifier: Optional[Text] = Unset)

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
        case Unterminated(open)  => m"the ${open.show} is not closed"
        case EmptyOperand(sigil) => m"${sigil.show} needs a type after it"
        case BareSymbol(text)    => m"a symbolic name is written in backticks, as `$text`"
        case BadType(text)       => m"$text is not a type"

  case class Error(reason: Error.Reason, offset: Int)(using Diagnostics)
  extends soundness.Error(reason.message)

  // A query, or what is wrong with it.
  def read(text: Text, vocabulary: Map[Text, Word] = Map()): Query | Error =
    amalgamate[Error](parse(text, vocabulary))

  // The characters of a text, by index, as a parse reads them: a test of the character at an
  // index, which fails beyond either end, and the text between two indices.
  private class Reading(text: Text):
    val length: Int = text.length
    def char(index: Int): Optional[Char] = text.at(index.z)
    def is(index: Int)(predicate: Char => Boolean): Boolean = char(index).let(predicate).or(false)
    def slice(from: Int, until: Int): Text = text.segment(from.z till until.z)

    // The index of the first character from an index which is the one given, if any is.
    def find(char: Char, from: Int): Optional[Int] =
      if from >= length then Unset else if is(from)(_ == char) then from else find(char, from + 1)

  private def wordChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'

  // The text as tokens, for a field to show it coloured as it is typed: sigils as symbols,
  // keywords as keywords, types as types, names as terms.
  def tokens(text: Text, vocabulary: Map[Text, Word] = Map()): List[Token] =
    val reading: Reading = Reading(text)
    import reading.{char, is, length, slice}

    def add(from: Int, until: Int, accent: Token.Accent): List[Token] =
      if until > from then List(Token(slice(from, until), accent)) else Nil

    def space(index: Int): Int = if is(index)(_.isWhitespace) then space(index + 1) else index

    def word(index: Int, dotted: Boolean): Int =
      def part(char: Char): Boolean = wordChar(char) || (dotted && char == '.')
      if is(index)(part) then word(index + 1, dotted) else index

    def opens(char: Char): Boolean = char == '(' || char == '[' || char == '{'
    def closes(char: Char): Boolean = char == ')' || char == ']' || char == '}'

    def balanced(index: Int, depth: Int, quote: Optional[Char]): Int = char(index).lay(index):
      (char: Char) =>
        quote match
          case q: Char => balanced(index + 1, depth, if char == q then Unset else q)

          case _ =>
            if char.isWhitespace && depth == 0 then index
            else if opens(char) then balanced(index + 1, depth + 1, Unset)
            else if closes(char) then balanced(index + 1, (depth - 1).max(0), Unset)
            else if char == '"' || char == '`' then balanced(index + 1, depth, char)
            else balanced(index + 1, depth, Unset)

    def recur(index: Int): List[Token] = char(index).lay(Nil): (char: Char) =>
      val gap: Int = space(index)

      if gap > index then add(index, gap, Token.Accent.Unparsed) + recur(gap)
      else
        char match
          case '/' =>
            val start: Int = space(index + 1)
            val end: Int = word(start, true)

            add(index, index + 1, Token.Accent.Symbol) +
              add(index + 1, start, Token.Accent.Unparsed) + add(start, end, Token.Accent.Term) +
              recur(end)

          case '#' | '.' | '?' | '~' =>
            add(index, index + 1, Token.Accent.Symbol) + recur(index + 1)

          case '+' | '-' =>
            val start: Int = space(index + 1)
            val end: Int = balanced(start, 0, Unset)

            add(index, index + 1, Token.Accent.Symbol) +
              add(index + 1, start, Token.Accent.Unparsed) + add(start, end, Token.Accent.Typal) +
              recur(end)

          case '`' =>
            val end: Int = reading.find('`', index + 1).lay(length)(_ + 1)
            add(index, end, Token.Accent.String) + recur(end)

          case char if wordChar(char) =>
            val end: Int = word(index, false)
            val text: Text = slice(index, end)
            val keyword: Boolean = Keyword.parse(text).present || vocabulary.at(text).present
            val accent: Token.Accent = if keyword then Token.Accent.Keyword else Token.Accent.Term
            add(index, end, accent) + recur(end)

          case _ =>
            val end: Int = balanced(index, 0, Unset).max(index + 1)
            add(index, end, Token.Accent.Error) + recur(end)

    recur(0)

  private def parse(text: Text, vocabulary: Map[Text, Word]): Query raises Error =
    val reading: Reading = Reading(text)
    import reading.{char, is, slice}

    def fail(reason: Error.Reason, offset: Int): Nothing = abort(Error(reason, offset))
    def space(char: Char): Boolean = char.isWhitespace

    def skipSpace(index: Int): Int = if is(index)(space) then skipSpace(index + 1) else index

    def word(index: Int, dotted: Boolean): Int =
      def part(char: Char): Boolean = wordChar(char) || (dotted && char == '.')
      if is(index)(part) then word(index + 1, dotted) else index

    // The end of a run which whitespace ends only outside brackets, quotes and backticks.
    def balanced(index: Int, stack: List[Char]): Int = char(index) match
      case char: Char =>
        stack match
          case open :: rest if char == open      => balanced(index + 1, rest)
          case ('"' | '`') :: _                  => balanced(index + 1, stack)
          case _ if space(char) && stack.nil     => index

          case _ => char match
            case '(' => balanced(index + 1, ')' :: stack)
            case '[' => balanced(index + 1, ']' :: stack)
            case '{' => balanced(index + 1, '}' :: stack)
            case '"' => balanced(index + 1, '"' :: stack)
            case '`' => balanced(index + 1, '`' :: stack)
            case _   => balanced(index + 1, stack)

      case _ =>
        stack.head.lay(index) { (open: Char) => fail(Error.Reason.Unterminated(open), index) }

    // A type: a dotted name with optional arguments, a parenthesised tuple or function type.
    def typeQuery(from: Int, until: Int): TypeQuery =
      val raw: Text = slice(from, until)
      val offset: Int = from

      def bad(): Nothing = fail(Error.Reason.BadType(raw.trim), offset)

      // Splits at the top-level occurrences of a separator, respecting brackets.
      def split(text: Text, separator: Text): List[Text] =
        val reading: Reading = Reading(text)

        def recur(index: Int, depth: Int, start: Int, parts: List[Text]): List[Text] =
          reading.char(index) match
            case char: Char =>
              if char == '(' || char == '[' || char == '{'
              then recur(index + 1, depth + 1, start, parts)
              else if char == ')' || char == ']' || char == '}'
              then recur(index + 1, depth - 1, start, parts)
              else if depth == 0 && reading.slice(index, index + separator.length) == separator
              then
                val next: Int = index + separator.length
                recur(next, depth, next, reading.slice(start, index) :: parts)
              else recur(index + 1, depth, start, parts)

            case _ =>
              (reading.slice(start, text.length) :: parts).reverse

        recur(0, 0, 0, Nil)

      def parse(text0: Text): TypeQuery =
        val text: Text = text0.trim
        if text == t"" then bad()

        // A function type has the lowest precedence; a context function is `?=>`.
        val contextual: List[Text] = split(text, t"?=>")
        val plain: List[Text] = split(text, t"=>")

        if contextual.size > 1 then
          val params: List[TypeQuery] = tuple(contextual.head.or(t"").trim, true)
          val result: TypeQuery = parse(contextual.skip(1).join(t"?=>"))
          TypeQuery(t"scala.ContextFunction${params.size}", params + List(result))
        else if plain.size > 1 then
          val params: List[TypeQuery] = tuple(plain.head.or(t"").trim, true)
          val result: TypeQuery = parse(plain.skip(1).join(t"=>"))
          TypeQuery(t"scala.Function${params.size}", params + List(result))
        else if text.starts(t"(") then
          if !text.ends(t")") then bad()
          val elements: List[TypeQuery] = tuple(text, false)

          elements match
            case List(element) => element
            case _             => TypeQuery(t"scala.Tuple${elements.size}", elements)
        else
          val end: Int = text.where { c => !(wordChar(c) || c == '.') }.lay(text.length)(_.n0)
          val name: Text = text.keep(end)
          if name == t"" then bad()
          val rest: Text = text.skip(end).trim
          val bracketed: Boolean = rest.starts(t"[") && rest.ends(t"]")

          def args: List[TypeQuery] = split(rest.skip(1).skip(1, Rtl), t",").map(parse)

          if rest == t"" then TypeQuery(name)
          else if bracketed then TypeQuery(name, args)
          else bad()

      // The elements of a parenthesised group, or a single unparenthesised type as a group of
      // one (a function's sole parameter).
      def tuple(text: Text, loneAllowed: Boolean): List[TypeQuery] =
        if text.starts(t"(") && text.ends(t")") then
          val inner: Text = text.skip(1).skip(1, Rtl)
          if inner.trim == t"" then Nil else split(inner, t",").map(parse)
        else if loneAllowed then
          List(parse(text))
        else
          bad()

      parse(raw)

    def recur(index: Int, query: Query): Query = char(index).lay(query): (char: Char) =>
      def operand(sigil: Char): (Int, Int) =
        val start: Int = skipSpace(index + 1)
        val end: Int = balanced(start, Nil)
        if end == start then fail(Error.Reason.EmptyOperand(sigil), index)
        (start, end)

      if space(char) then recur(index + 1, query)
      else char match
        case '/' =>
          val start: Int = skipSpace(index + 1)
          val end: Int = word(start, true)
          if end == start then fail(Error.Reason.EmptyOperand('/'), index)
          recur(end, query.copy(packages = query.packages + List(slice(start, end))))

        case '#' => recur(index + 1, query.copy(form = Form.Type))
        case '.' => recur(index + 1, query.copy(form = Form.Term))
        case '?' => recur(index + 1, query.copy(kinds = query.kinds + Keyword.Given.kinds))
        case '~' => recur(index + 1, query.copy(kinds = query.kinds + Keyword.Extension.kinds))

        case '+' =>
          val (start, end) = operand('+')
          recur(end, query.copy(results = query.results + List(typeQuery(start, end))))

        case '-' =>
          val (start, end) = operand('-')
          recur(end, query.copy(parameters = query.parameters + List(typeQuery(start, end))))

        case '`' =>
          val close: Int = reading.find('`', index + 1).or:
            fail(Error.Reason.Unterminated('`'), index)

          val name: Name = Name(slice(index + 1, close), true)
          recur(close + 1, query.copy(names = query.names + List(name)))

        case char if wordChar(char) =>
          val end: Int = word(index, false)
          val text: Text = slice(index, end)

          // `case class` is one keyword.
          val next: Int = skipSpace(end)
          val afterNext: Int = word(next, false)

          if text == t"case" && next > end && slice(next, afterNext) == t"class" then
            recur(afterNext, query.copy(kinds = query.kinds + Keyword.CaseClass.kinds))
          else
            val named: Query = query.copy(names = query.names + List(Name(text, false)))

            // A word of another format's, where it is none of Scala's.
            def foreign: Query = vocabulary.at(text).lay(recur(end, named)): (word: Word) =>
              val modifiers: Set[Text] =
                word.modifier.lay(query.modifiers) { modifier => query.modifiers + Set(modifier) }

              recur(end, query.copy(kinds = query.kinds + word.kinds, modifiers = modifiers))

            Keyword.parse(text).lay(foreign): keyword =>
              val modifiers: Set[Text] =
                keyword.modifier.lay(query.modifiers): modifier =>
                  query.modifiers + Set(modifier)

              recur(end, query.copy(kinds = query.kinds + keyword.kinds, modifiers = modifiers))

        case _ =>
          val end: Int = balanced(index, Nil)
          fail(Error.Reason.BareSymbol(slice(index, end)), index)

    recur(0, Query())

case class Query
  ( packages:   List[Text]            = Nil,
    form:       Optional[Query.Form]  = Unset,
    kinds:      Set[Api.Kind]         = Set(),
    modifiers:  Set[Text]             = Set(),
    names:      List[Query.Name]      = Nil,
    results:    List[Query.TypeQuery] = Nil,
    parameters: List[Query.TypeQuery] = Nil ):

  def empty: Boolean =
    packages.nil && form.absent && kinds.nil && modifiers.nil && names.nil && results.nil &&
      parameters.nil

  // What can be said of a query before it is run.
  def warnings: List[Text] =
    if modifiers.has(t"private")
    then List(t"Private members are not documented, so nothing can match `private`.")
    else Nil
