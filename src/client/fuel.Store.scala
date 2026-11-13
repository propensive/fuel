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

import java.util.concurrent as juc

import scala.collection.immutable as sci
import scala.jdk.CollectionConverters.*

import soundness.*

import pyrocosm.{Block, Filing, ObjectStore}

object Store:
  // The manifest of an output directory, if it has one.
  def manifest(output: Text): Optional[Output.Manifest] =
    safely(output.as[Path on Linux] / "index.fuel").let(Filing.data(_))
    . let(Output.Manifest.decode(_))

class Store(output: Text, fresh: Boolean) extends Cache:
  private val root: Path on Linux = unsafely(output.as[Path on Linux])
  private val index: Path on Linux = unsafely(root / "index.fuel")

  // The objects, content-addressed by Pyrocosm's store, as `<digest>.fuel`.
  private val objects: ObjectStore =
    val directory: Path on Linux = unsafely(root / "objects")
    ObjectStore.at(directory, t".fuel").or(ObjectStore(directory, t".fuel"))

  private val kept: sci.Map[Text, Text] =
    val manifest: Optional[Output.Manifest] = if fresh then Unset else Store.manifest(output)

    manifest.lay(sci.Map()): (manifest: Output.Manifest) =>
      val entries = manifest.cache.stdlib.map: entry => (entry.key, entry.record)
      val pages = manifest.pages.stdlib.map: page => (page.key, page.record)
      sci.Map.from(entries ++ pages)

  private val used: juc.ConcurrentHashMap[Text, Text] = juc.ConcurrentHashMap()

  private val documents: juc.ConcurrentLinkedQueue[Output.Manifest.Document] =
    juc.ConcurrentLinkedQueue()

  private val pages: juc.ConcurrentLinkedQueue[Output.Manifest.Page] = juc.ConcurrentLinkedQueue()

  val hits: Atomic[Int] = Atomic(0)
  val misses: Atomic[Int] = Atomic(0)

  private def write(data: Data): Text = objects.put(data)

  private def read[value](key: Text)(decode: Data => Optional[value]): Optional[value] =
    kept.get(key).fold(Unset)(identity).let: (hash: Text) =>
      objects.get(hash).let(decode(_)).let: (value: value) =>
        used.put(key, hash)
        hits.since(_ + 1)
        value

  private def keep(key: Text, data: Data): Unit =
    used.put(key, write(data))
    misses.since(_ + 1)

  def fragment(key: Text): Optional[Output.Fragment] = read(key)(Output.Fragment.decode)

  def fragment(key: Text, fragment: Output.Fragment): Unit =
    keep(key, Output.Fragment.encode(fragment))

  def prose(key: Text): Optional[Output.Prose] = read(key)(Output.Prose.decode)
  def prose(key: Text, prose: Output.Prose): Unit = keep(key, Output.Prose.encode(prose))

  // A page, rendered unless it is kept; recorded for the manifest either way.
  def page(key: Text, entity: Text, path: Text)(render: => (List[Block], List[Block]))
  :   Output.Page =

    val page: Output.Page = read(key)(Output.Page.decode).or:
      val (blocks, problems) = render
      val page: Output.Page = Output.Page(entity, path, blocks, problems)
      keep(key, Output.Page.encode(page))
      page

    pages.add(Output.Manifest.Page(entity, path, key, used.get(key).nn))
    page

  def document(section: Text, doc: Doc): Unit =
    val record: Output.Document = Output.Document(doc.path, doc.title, doc.blocks, doc.problems)
    val hash: Text = write(Output.Document.encode(record))
    documents.add(Output.Manifest.Document(section, doc.path, doc.title, hash))

  // Writes the manifest, and deletes every object it no longer names.
  def finish(fuel: Text, classpath: Text, environment: Text): Unit =
    val pageKeys: Set[Text] = Set.from(pages.asScala.map(_.key))
    val entries = used.asScala.toList.filter: entry => !pageKeys.has(entry(0))
    val cache = List.from(entries.sortBy(_(0).s).map(Output.Manifest.Entry(_, _)))
    val documented = List.from(documents.asScala.toList.sortBy(_.path.s).sortBy(_.section.s))
    val paged = List.from(pages.asScala.toList.sortBy(_.entity.s))

    val manifest: Output.Manifest =
      Output.Manifest(fuel, classpath, environment, documented, paged, cache)

    Filing.replace(index, Output.Manifest.encode(manifest))

    val named: Set[Text] =
      Set.from
        ( manifest.documents.stdlib.map(_.record) ++
          manifest.pages.stdlib.map(_.record) ++
          manifest.cache.stdlib.map(_.record) )

    objects.prune(named)
