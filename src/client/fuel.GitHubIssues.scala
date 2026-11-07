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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging

// GitHub's issues, through its REST API, as `burdock.GitHub` lists releases: a page of a hundred
// at a time, with a token when one is found, and never raising — a failure is an outcome the
// caller reports. The first fetch of a repository asks for its open issues; every later one asks
// for what changed since the latest change seen (closures included), with the ETag of the last
// answer, so that a poll which finds nothing new is answered `304 Not Modified`, which GitHub
// does not count against the rate limit.
object GitHubIssues:
  // The subset of an issue's payload the pages need. The endpoint lists pull requests too, each
  // with a `pull_request` of its own, which is how they are told apart and left out.
  case class Payload
    ( number:  Int,
      title:   Text,
      state:   Text,
      @name[Json](t"html_url") url: Text,
      @name[Json](t"updated_at") updated: Text,
      body:    Optional[Text],
      @name[Json](t"pull_request") pullRequest: Optional[Json] )

  def issue(payload: Payload): Optional[Issue] =
    if payload.pullRequest.present then Unset
    else
      Issue
        ( payload.number,
          payload.title,
          payload.url,
          payload.state == t"open",
          payload.updated,
          Issue.affected(payload.body.or(t"")) )

  enum Outcome:
    case Unchanged
    case Changed(issues: List[Issue], etag: Optional[Text])
    case Failed(message: Text)

  private val pageSize: Int = 100
  private val jsonMedia: MediaType = media"application/vnd.github+json"

  // The issues of `target` which changed since `state` saw them, or all its open ones where
  // the state has seen none; `Unchanged` when the tracker says nothing has.
  def poll(target: Sync.Target, token: Optional[Text], state: State): Outcome =
    import strategies.throwUnsafely

    val (owner, repository) = target match
      case Sync.Target.Github(owner, repository) => (owner, repository)

    def fetch(url: HttpUrl, etag: Optional[Text]): Http.Response = (token, etag) match
      case (token: Text, etag: Text) =>
        url.fetch(accept = jsonMedia, authorization = Auth.Bearer(token), ifNoneMatch = etag)

      case (token: Text, _) => url.fetch(accept = jsonMedia, authorization = Auth.Bearer(token))
      case (_, etag: Text)  => url.fetch(accept = jsonMedia, ifNoneMatch = etag)
      case _                => url.fetch(accept = jsonMedia)

    val filter: Text = state.since.lay(t"state=open"): (since: Text) => t"state=all&since=$since"

    def url(page: Int): HttpUrl =
      t"https://api.github.com/repos/$owner/$repository/issues?$filter&per_page=${pageSize.show}&page=${page.show}"
      . as[HttpUrl]

    // The first page carries the ETag, and may be answered that nothing changed; the pages
    // follow until one is empty.
    def pages(page: Int, etag: Optional[Text]): Outcome =
      val response: Http.Response = fetch(url(page), if page == 1 then state.etag else Unset)

      if response.status == Http.Status.NotModified then Outcome.Unchanged
      else
        // The header by its name: telekinesis's `headers.eTag` would look for `e-tag`.
        val tag: Optional[Text] =
          if page == 1 then response.textHeaders.filter(_.key.lower == t"etag").map(_.value).head
          else etag
        val payloads: List[Payload] = mute[Http.Event](response.receive[Json]).as[List[Payload]]
        val issues: List[Issue] = payloads.bind { payload => issue(payload).let(List(_)).or(Nil) }

        if payloads.nil then Outcome.Changed(Nil, tag)
        else pages(page + 1, tag) match
          case Outcome.Changed(rest, _) => Outcome.Changed(issues + rest, tag)
          case other                    => other

    try pages(1, Unset) catch
      case error: Http.Error      => Outcome.Failed(t"HTTP ${error.status.toString.tt}")
      case error: Connect.Error   => Outcome.Failed(t"api.github.com could not be reached")
      case error: Json.Error      => Outcome.Failed(t"the issues were not in the expected format")
      case error: Parse.Error     => Outcome.Failed(t"the issues were not valid JSON")
      case error: Truncation.Error => Outcome.Failed(t"the issues were cut short")
      case error: Url.Error       => Outcome.Failed(t"the issues URL is not valid")
      case error: Exception       => Outcome.Failed(error.toString.tt)

  type State = Sync.State
