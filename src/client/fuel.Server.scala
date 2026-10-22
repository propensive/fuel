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

import java.util.concurrent as juc

import soundness.*

import pyrocosm.Block

// The daemon's state: the corpus and the API model the latest `fuel compile` produced, from
// whichever shell, the pages it rendered for the project's own documented entities (whose
// examples it compiled; any other page is rendered when asked for, highlighted alone), the
// imports a document resolves through, and an edition the dashboard watches so that it
// rebuilds only when a compile has happened. The models of dependency jars are kept by path
// and modification time, since a jar seldom changes between compiles.
object Server:
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var corpus0: Optional[Corpus] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var api0: Api = Api.empty

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var pages0: scala.collection.immutable.Map[Text, List[Block]] = scala.collection.immutable.Map()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var imports0: List[Text] = Nil

  private val edition0: juc.atomic.AtomicLong = juc.atomic.AtomicLong(0L)
  private val jars: juc.ConcurrentHashMap[(Text, Long), Api] = juc.ConcurrentHashMap()

  def publish(corpus: Corpus, api: Api = Api.empty, pages: scala.collection.immutable.Map[Text, List[Block]] = scala.collection.immutable.Map(), imports: List[Text] = Nil): Unit =
    corpus0 = corpus
    api0 = api
    pages0 = pages
    imports0 = imports
    edition0.incrementAndGet()

  def corpus: Optional[Corpus] = corpus0
  def api: Api = api0
  def imports: List[Text] = imports0
  def edition: Long = edition0.get

  def page(key: Text): Optional[List[Block]] = pages0.get(key) match
    case Some(blocks) => blocks
    case _            => Unset

  // A jar's model, walked once per version of the jar.
  def jar(path: Text)(walk: => Api): Api =
    val modified: Long = java.nio.file.Files.getLastModifiedTime(java.nio.file.Path.of(path.s)).nn.toMillis
    jars.computeIfAbsent((path, modified), _ => walk).nn
