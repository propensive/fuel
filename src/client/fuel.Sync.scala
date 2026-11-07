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

import java.util.concurrent as juc
import scala.collection.immutable as sci

import codepages.utf8Codepage

import dysasymptotics.{linearAccess, linearSize}
import probates.cancelProbate

// What the API documentation is attached to: a tracker whose issues the pages show, attached by
// `fuel sync github owner/repo` or a `sync github owner/repo` line of the configuration, and
// synced from then on. What a sync has learned of a target — the ETag and the time of the last
// change it saw, by which the next poll asks only for what changed, and the open issues — is
// kept in the project's cache, so that a daemon started afresh shows the issues at once and
// asks the tracker only for what is new.
object Sync:
  enum Target:
    case Github(owner: Text, repository: Text)

    // As the configuration and the command write it.
    def text: Text = this match
      case Github(owner, repository) => t"github $owner/$repository"

    // As a file is named for it.
    def slug: Text = this match
      case Github(owner, repository) => t"github-$owner-$repository"

  object Target:
    // The words of a `sync` line or command: the kind, then what it names.
    def parse(words: List[Text]): Optional[Target] = words.stdlib.toList match
      case scala.::(kind, scala.::(name, scala.Nil)) if kind == t"github" => name.cut(t"/").stdlib.toList match
        case scala.::(owner, scala.::(repository, scala.Nil)) if owner != t"" && repository != t"" =>
          Github(owner, repository)

        case _ => Unset

      case _ => Unset

  // What a sync of one target has learned, persisted whole after every successful fetch:
  // `since` is the latest change the tracker reported, written as it wrote it; `issues` are
  // the open ones alone, since a closure drops an issue as it arrives.
  case class State
    ( target: Text,
      etag:   Optional[Text] = Unset,
      since:  Optional[Text] = Unset,
      issues: List[Issue]    = Nil )

  object State:
    import Output.derivation.throwing

    def encode(state: State): Text = t"tel 1.0\n\n${state.tel.show}"
    def decode(text: Text): Optional[State] = safely(text.read[Tel].as[State])

  // The project's sync directory: within its cache, or beside its `.fuel/classes` where there
  // is no cache directory, as the compiled sources are kept.
  def directory(root: Text)(using Environment): Text =
    Fuel.projectCache(root).let { (cache: Path on Linux) => t"${cache.encode}/sync" }
    . or(t"$root/.fuel/sync")

  def file(directory: Text, target: Target): Text = t"$directory/${target.slug}.tel"

  def load(file: Text): Optional[State] =
    val path: java.nio.file.Path = java.nio.file.Path.of(file.s).nn
    if !java.nio.file.Files.exists(path) then Unset
    else safely(java.nio.file.Files.readString(path).nn.tt).let(State.decode(_))

  def save(file: Text, state: State): Unit =
    val path: java.nio.file.Path = java.nio.file.Path.of(file.s).nn
    java.nio.file.Files.createDirectories(path.getParent.nn)
    java.nio.file.Files.writeString(path, State.encode(state).s)

  // The targets attached: the configuration's, and those synced before, whose state is kept.
  def targets(configured: List[Target], directory: Text): List[Target] =
    val path: java.nio.file.Path = java.nio.file.Path.of(directory.s).nn

    val kept: List[Target] =
      if !java.nio.file.Files.isDirectory(path) then Nil else
        val files: scala.List[java.nio.file.Path] =
          scala.collection.immutable.ArraySeq.unsafeWrapArray(path.toFile.nn.listFiles().nn)
          . toList.map(_.nn.toPath.nn).filter(_.toString.endsWith(".tel"))

        List.from:
          files.flatMap: (file: java.nio.file.Path) =>
            load(file.toString.tt).let(_.target).let { text => Target.parse(text.cut(t" ")) }
            . let(scala.List(_)).or(scala.Nil)

    val all: List[Target] = configured + kept
    List.from(all.stdlib.distinct)

  // The state after a fetch: each issue fetched replaces the one of its number, or is dropped
  // when it is closed; `since` moves to the latest change seen (the tracker's timestamps are
  // ISO 8601 in UTC, so the latest is the greatest as text), and the ETag is the response's.
  def merge(state: State, fetched: List[Issue], etag: Optional[Text]): State =
    val numbers: sci.Set[Int] = fetched.stdlib.map(_.number).toSet
    val kept: List[Issue] = state.issues.filter { issue => !numbers.contains(issue.number) }
    val all: List[Issue] = kept + fetched.filter(_.open)
    val issues: List[Issue] = List.from(all.stdlib.sortBy(_.number))
    val seen: scala.List[String] = state.since.let(_.s).let(scala.List(_)).or(scala.Nil) ++ fetched.stdlib.map(_.updated.s)
    val latest: Optional[Text] = seen.maxOption.map(_.tt).getOrElse(Unset)
    State(state.target, etag.or(state.etag), latest, issues)

  // What a poll of a target found: the state after it, how many issues it fetched (none when
  // the tracker said nothing changed), and what went wrong, if it failed, in which case the
  // state is as it was.
  case class Fetched(state: State, changed: Optional[Int], failure: Optional[Text])

  // A target polled from its kept state (or from nothing), what changed merged and kept.
  def fetch(target: Target, directory: Text, token: Optional[Text]): Fetched =
    val path: Text = file(directory, target)
    val before: State = load(path).or(State(target.text))

    GitHubIssues.poll(target, token, before) match
      case GitHubIssues.Outcome.Unchanged               => Fetched(before, Unset, Unset)
      case GitHubIssues.Outcome.Failed(message)         => Fetched(before, Unset, message)
      case GitHubIssues.Outcome.Changed(issues, etag)   =>
        Fetched(merge(before, issues, etag).tap { merged => save(path, merged) }, issues.size, Unset)

  // The issues of every attached target, indexed against the API served, published to the
  // pages.
  def publish(states: List[State]): List[(Int, Text)] =
    val scope: Scope = Scope.document(Server.imports, Server.standard)
    val (index, unresolved) = Issue.index(states.bind(_.issues), Server.api, scope)
    Server.publishIssues(states, index)
    unresolved

  // One poll of every attached target, from the state the daemon holds: what changed is merged,
  // kept and published; a poll which fails leaves the last good state, with its failure noted.
  def refresh(directory: Text, token: Optional[Text]): Unit =
    val polled: List[State] = Server.syncs.map: (state: State) =>
      Target.parse(state.target.cut(t" ")).lay(state): (target: Target) =>
        GitHubIssues.poll(target, token, state) match
          case GitHubIssues.Outcome.Unchanged =>
            state

          case GitHubIssues.Outcome.Changed(issues, etag) =>
            merge(state, issues, etag).tap { merged => save(file(directory, target), merged) }

          case GitHubIssues.Outcome.Failed(message) =>
            Server.notice(t"the issues of ${target.text} could not be fetched: $message")
            state

    if polled != Server.syncs then publish(polled)

  val interval = 10*Minute

  private val watching: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)

  // The daemon's refresher, started once: every ten minutes, while the dashboard is served, the
  // attached targets are polled, which asks the tracker only for what changed since the last
  // time, and costs nothing of its rate limit when nothing has.
  def watch(directory: Text, token: Optional[Text])(using Monitor): Unit =
    if watching.compareAndSet(false, true) then
      async:
        while true do
          if pyrocosm.Tool.services.exists { (name, _) => name == t"fuel/serve" } then refresh(directory, token)
          snooze(interval)
