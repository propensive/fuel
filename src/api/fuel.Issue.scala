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
import pyrocosm.{Block, Inline}
import soundness.sortingAlgorithms.timsort

// An issue of a tracker, as the API's pages need it: its number, title and address, whether it
// is open, when it last changed (the tracker's own timestamp, kept as written), and the names
// of the entities its body says it affects. An issue is attached to an entity only when the
// body says so, with the keyword `affects` followed by the names in backticks:
//
//   This affects `soundness.Foo`, `soundness.Bar#length` and `gossamer.Text`.
//
// The names are kept as written, and resolved against the API each time it is compiled, as a
// reference in prose is, so that an issue filed against a name which is then renamed is
// reported rather than silently dropped.
case class Issue
  ( number:  Int,
    title:   Text,
    url:     Text,
    open:    Boolean,
    updated: Text,
    affects: List[Text] )

object Issue:
  // The body without its HTML comments (each from `<!--` to the first `-->` after it) and its
  // fenced code (``` or ~~~, to the matching fence), where the keyword would be a sample or a
  // template's note rather than a statement.
  private def prose(body: Text): Text =
    def uncommented(text: Text): Text = text.offsetOf(t"<!--").lay(text): (open: Ordinal) =>
      val rest: Text = text.skip(open.n0 + 4)

      rest.offsetOf(t"-->").lay(text): (close: Ordinal) =>
        text.keep(open.n0) + uncommented(rest.skip(close.n0 + 3))

    def recur(lines: List[Text], fence: Optional[Text], kept: List[Text]): List[Text] =
      lines match
        case Nil => kept.reverse

        case line :: rest =>
          val trimmed: Text = line.trim

          fence match
            case open: Text =>
              if trimmed.starts(open) then recur(rest, Unset, kept) else recur(rest, open, kept)

            case _ =>
              if trimmed.starts(t"```") then recur(rest, t"```", kept)
              else if trimmed.starts(t"~~~") then recur(rest, t"~~~", kept)
              else recur(rest, Unset, line :: kept)

    recur(uncommented(body).cut(t"\n"), Unset, Nil).join(t"\n")

  // The names an issue's body says it affects: after each `affects` (on word boundaries, in any
  // case), the backticked names which follow, separated by commas, semicolons, colons, `&`,
  // `and`, `or` or lines, until something else; once each, in order, trimmed, and empty ones
  // dropped.
  def affected(body: Text): List[Text] =
    val text: Text = prose(body)
    val size: Int = text.length

    def at(index: Int): Optional[Char] = text.at(index.z)

    def wordly(index: Int): Boolean =
      at(index).let { (char: Char) => char.isLetterOrDigit || char == '_' }.or(false)

    // Whether a word is at an index, standing alone, in the case given or, if `anyCase`, in any.
    def word(index: Int, word: Text, anyCase: Boolean): Boolean =
      val end: Int = index + word.length
      val found: Text = if end > size then t"" else text.segment(index.z till end.z)
      val same: Boolean = if anyCase then found.lower == word else found == word
      same && !wordly(index - 1) && !wordly(end)

    def spaces(index: Int): Int =
      if at(index).let(_.isWhitespace).or(false) then spaces(index + 1) else index

    // Past the spaces and separators before a name.
    def separators(index: Int): Int =
      val next: Int = spaces(index)

      val separator: Boolean =
        at(next).let { (char: Char) => char == ',' || char == ';' || char == '&' || char == ':' }
        . or(false)

      if separator then separators(next + 1)
      else if word(next, t"and", false) then separators(next + 3)
      else if word(next, t"or", false) then separators(next + 2)
      else next

    // The backticked names from an index, each where the one before it ends.
    def names(index: Int): List[Text] =
      val open: Int = separators(index)

      if at(open) != '`' then Nil else
        def end(index: Int): Int = at(index) match
          case '`' | '\n' | Unset => index
          case _                  => end(index + 1)

        val close: Int = end(open + 1)

        if at(close) != '`' then Nil
        else text.segment((open + 1).z till close.z).trim :: names(close + 1)

    def recur(index: Int, found: List[Text]): List[Text] =
      if index >= size then found
      else if word(index, t"affects", true) then recur(index + 7, found + names(index + 7))
      else recur(index + 1, found)

    recur(0, Nil).filter(_ != t"").distinct

  // The names resolved to entity keys, as a reference in prose resolves (through the scope's
  // imports, an export followed to its target), and those nothing resolves, or which name
  // more than one entity.
  def resolve(names: List[Text], api: Api, scope: Scope): (List[Text], List[Text]) =
    resolve(names, Apis.of(api), scope)

  // The same, among the entities of every format, each by its address.
  def resolve(names: List[Text], apis: Apis, scope: Scope): (List[Text], List[Text]) =
    val resolver: Resolver = Resolver(apis.primary, scope, Unset, apis)

    val each: List[(Text, Optional[Text])] = names.map: (name: Text) =>
      val key: Optional[Text] = resolver.link(name) match
        case Inline.Destination.Local(path, _) => apis.locate(path.skip(5)).let(_.address)
        case _                                 => Unset

      (name, key)

    val keys: List[Text] = each.bind { (name, key) => key.let(List(_)).or(Nil) }
    val unresolved: List[Text] = each.filter(_(1).absent).map(_(0))
    (keys, unresolved)

  // The open issues by the key of each entity they affect, each entity's in order of number;
  // and every name of an issue which resolves to nothing, with the issue's number.
  def index(issues: List[Issue], api: Api, scope: Scope)
  :   (Map[Text, List[Issue]], List[(Int, Text)]) =

    index(issues, Apis.of(api), scope)

  def index(issues: List[Issue], apis: Apis, scope: Scope)
  :   (Map[Text, List[Issue]], List[(Int, Text)]) =

    val open: List[Issue] = issues.filter(_.open)

    val resolved: List[(Issue, List[Text], List[Text])] = open.map: (issue: Issue) =>
      val (keys, unresolved) = resolve(issue.affects, apis, scope)
      (issue, keys, unresolved)

    val pairs: List[(Text, Issue)] = resolved.bind: (issue, keys, _) =>
      keys.map((_, issue))

    val byKey: Map[Text, List[Issue]] = pairs.group(_(0)).map: (group: List[(Text, Issue)]) =>
      group.map(_(1)).deduplicate(_.number).order(_.number)

    val unresolved: List[(Int, Text)] = resolved.bind: (issue, _, names) =>
      names.map { (name: Text) => (issue.number, name) }

    (byKey, unresolved)
