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

import scala.collection.immutable as sci

import soundness.*

import pyrocosm.{Block, Digests}

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
  private var formats0: List[Api] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var pages0: sci.Map[Text, List[Block]] = sci.Map()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var imports0: List[Text] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var standard0: List[Locator] = Scope.standard

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var glossary0: List[Block] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var terms0: Glossary = Glossary.none

  // The attached trackers' states, and their open issues by the key of each entity they affect:
  // published apart from the corpus, since issues change on their own, and kept across
  // compiles, which re-index them against the new API.
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var syncs0: List[Sync.State] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var issues0: sci.Map[Text, List[Issue]] = sci.Map()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var notices0: List[Text] = Nil

  def syncs: List[Sync.State] = syncs0
  def issues(key: Text): List[Issue] = issues0.get(key).getOrElse(Nil)

  // What the refresher could not do, for the next `fuel sync` to say; said once.
  def notice(text: Text): Unit = notices0 = notices0 + List(text)
  def notices(): List[Text] = notices0.tap { _ => notices0 = Nil }

  def publishIssues(states: List[Sync.State], index: sci.Map[Text, List[Issue]]): Unit =
    syncs0 = states
    issues0 = index
    edition0.incrementAndGet()

  private val edition0: juc.atomic.AtomicLong = juc.atomic.AtomicLong(0L)
  private val jars: juc.ConcurrentHashMap[(Text, Long), Api] = juc.ConcurrentHashMap()

  def publish
    ( corpus:   Corpus,
      api:      Api                        = Api.empty,
      pages:    sci.Map[Text, List[Block]] = sci.Map(),
      imports:  List[Text]                 = Nil,
      glossary: List[Block]                = Nil,
      standard: List[Locator]           = Scope.standard,
      terms:    Glossary                   = Glossary.none,
      formats:  List[Api]                  = Nil )
  :   Unit =

    terms0 = terms
    corpus0 = corpus
    api0 = api
    formats0 = formats
    pages0 = pages
    imports0 = imports
    glossary0 = glossary
    standard0 = standard
    edition0.incrementAndGet()

    // The search index is built now, apart, rather than at the first keystroke.
    val runnable: Runnable = () => { api.search; () }
    val warm: java.lang.Thread = java.lang.Thread(runnable)
    warm.setDaemon(true)
    warm.start()

  def corpus: Optional[Corpus] = corpus0
  def api: Api = api0

  // The APIs of every format, Scala's first: a page and an entity's issues are kept by the
  // entity's address among them.
  def apis: Apis = Apis(api0 :: formats0)
  def imports: List[Text] = imports0
  def standard: List[Locator] = standard0
  def glossary: List[Block] = glossary0
  def terms: Glossary = terms0
  def edition: Long = edition0.get

  def page(key: Text): Optional[List[Block]] = pages0.get(key) match
    case Some(blocks) => blocks
    case _            => Unset

  private val hashes: juc.ConcurrentHashMap[(Text, Long, Long), Text] = juc.ConcurrentHashMap()

  // A classpath entry's hash, computed once per size and modification time of the entry.
  def hash(entry: Text): Text =
    val path = java.nio.file.Path.of(entry.s).nn

    def tree: Text = Digests.tree(unsafely(entry.as[Path on Linux]))

    if !java.nio.file.Files.isRegularFile(path) then tree else
      val modified: Long = java.nio.file.Files.getLastModifiedTime(path).nn.toMillis
      val key: (Text, Long, Long) = (entry, java.nio.file.Files.size(path), modified)
      hashes.computeIfAbsent(key, _ => tree).nn

  // A jar's model, walked once per version of the jar.
  def jar(path: Text, shadowed: List[Text] = Nil)(walk: => Api): Api =
    val file: java.nio.file.Path = java.nio.file.Path.of(path.s).nn
    val modified: Long = java.nio.file.Files.getLastModifiedTime(file).nn.toMillis
    jars.computeIfAbsent((t"$path ${shadowed.join(t",")}", modified), _ => walk).nn

  // The JDK's model, walked once per runtime, which does not change while the daemon runs.
  def jdk(walk: => Api): Api =
    val version: Text = java.lang.Runtime.version.nn.toString.tt
    jars.computeIfAbsent((t"jdk $version", 0L), _ => walk).nn
