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

import environments.javaBaseEnvironment
import pyrocosm.Shares

// What Fuel knows of the API it last compiled, for another tool on this machine to show beside
// a name — Flame's completions, say: each documented entity's address (`gossamer.Text#length`)
// and its `info` line, as plain text. It is a share (`pyrocosm.Shares`): a RaTEL image in the
// project's cache, `$XDG_CACHE_HOME/fuel/<digest>/share/help.ratel`, which a `fuel compile`
// replaces whole and any other tool maps into memory, finding one entry by its address without
// reading the rest. Nothing is served and nothing announced: a tool reads the file whether or
// not Fuel is running, and sees a newer one the next time it looks.
//
// The image carries its schema, and a reader asks for `entry`, `address` and `info` by those
// keywords, so a member added here is one a reader may ignore.
object Help:
  case class Index(entry: List[Entry])
  case class Entry(@key address: Text, info: Text)

  // The share's name, under the project's cache.
  val share: Text = t"help"

  // The index of the model `Server` holds: every entity with an `info`, rendered to plain text
  // through its own scope, so that a sigilled reference reads as a name. An address is a key,
  // so the first entity to have one keeps it.
  def index(): Index =
    val apis: Apis = Server.apis

    // Every format's entities, each by its address.
    val entries: List[Entry] = apis.all.bind: (api: Api) =>
      api.entities.values.bind: (entity: Api.Entity) =>
        entity.doc.let(_.info).lay(Nil: List[Entry]): (info: Text) =>
          val standard: List[Locator] = api.format.standard(Server.standard)
          val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil), standard)
          val resolver: Resolver = Resolver(api, scope, Unset, apis)
          List(Entry(entity.address, Blocks.plain(Blocks.inline(info, resolver))))

    val distinct: Map[Text, Entry] =
      Map(entries.reverse.map { (entry: Entry) => entry.address -> entry }*)

    Index(distinct.values.to[List])

  // Where the index of the project at `root` is published.
  def file(root: Text): Optional[Path on Linux] = Shares.path(Fuel.name, root, share)

  // Publishes the index for the project at `root`; `false` if it could not be written, which
  // leaves the last one in place.
  def publish(root: Text): Boolean = file(root).lay(false)(Shares.publish(_, index()))
