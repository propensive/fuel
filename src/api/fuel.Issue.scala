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

import scala.collection.immutable as sci

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Inline}

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
  // The keyword, on word boundaries, in any case; and a name in backticks.
  private val keyword: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("(?i)\\baffects\\b").nn

  private val backticked: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("\\G\\s*(?:(?:,|;|&|\\band\\b|\\bor\\b|:)\\s*)*`([^`\\n]*)`").nn

  // The body without its fenced code (``` or ~~~, to the matching fence) and HTML comments,
  // where the keyword would be a sample or a template's note rather than a statement.
  private def prose(body: Text): String =
    val uncommented: String = body.s.replaceAll("(?s)<!--.*?-->", "").nn

    def recur(lines: scala.List[String], fence: Option[String], acc: scala.List[String])
    :   scala.List[String] =

      lines match
        case scala.Nil => acc.reverse

        case scala.::(line, rest) =>
          val trimmed: String = line.trim.nn

          fence match
            case Some(open) =>
              if trimmed.startsWith(open) then recur(rest, None, acc) else recur(rest, fence, acc)

            case None =>
              if trimmed.startsWith("```") then recur(rest, Some("```"), acc)
              else if trimmed.startsWith("~~~") then recur(rest, Some("~~~"), acc)
              else recur(rest, None, line :: acc)

    val lines: scala.List[String] =
      scala.collection.immutable.ArraySeq.unsafeWrapArray(uncommented.split("\n", -1).nn)
      . toList.map(_.nn)

    recur(lines, None, scala.Nil).mkString("\n")

  // The names an issue's body says it affects: after each `affects`, the backticked names
  // which follow, separated by commas, `and`, `or` or lines, until something else; once each,
  // in order, trimmed, and empty ones dropped.
  def affected(body: Text): List[Text] =
    val text: String = prose(body)
    val finder: java.util.regex.Matcher = keyword.matcher(text).nn
    var found: scala.List[String] = scala.Nil

    while finder.find() do
      val names: java.util.regex.Matcher = backticked.matcher(text).nn
      var at: Int = finder.end()

      while names.find(at) && names.start() == at do
        found = names.group(1).nn.trim.nn :: found
        at = names.end()

    List.from(found.reverse.filter(_.nonEmpty).distinct.map(_.tt))

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
  :   (sci.Map[Text, List[Issue]], List[(Int, Text)]) =

    index(issues, Apis.of(api), scope)

  def index(issues: List[Issue], apis: Apis, scope: Scope)
  :   (sci.Map[Text, List[Issue]], List[(Int, Text)]) =

    val open: List[Issue] = issues.filter(_.open)

    val resolved: List[(Issue, List[Text], List[Text])] = open.map: (issue: Issue) =>
      val (keys, unresolved) = resolve(issue.affects, apis, scope)
      (issue, keys, unresolved)

    val pairs: scala.List[(Text, Issue)] =
      resolved.stdlib.toList.flatMap { (issue, keys, _) => keys.stdlib.toList.map((_, issue)) }

    val byKey: sci.Map[Text, List[Issue]] = pairs.groupBy(_(0)).map: (key, group) =>
      (key, List.from(group.map(_(1)).distinctBy(_.number).sortBy(_.number)))

    val unresolved: List[(Int, Text)] = resolved.bind: (issue, _, names) =>
      names.map { (name: Text) => (issue.number, name) }

    (byKey, unresolved)
