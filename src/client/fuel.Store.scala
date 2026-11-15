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

import collations.codepointCollation
import pyrocosm.{Block, Filing, ObjectStore}
import soundness.collationComparable
import sortingAlgorithms.timsort

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

  private val kept: Map[Text, Text] =
    val manifest: Optional[Output.Manifest] = if fresh then Unset else Store.manifest(output)

    manifest.lay(Map[Text, Text]()): (manifest: Output.Manifest) =>
      val entries: List[(Text, Text)] = manifest.cache.map { entry => (entry.key, entry.record) }
      val pages: List[(Text, Text)] = manifest.pages.map { page => (page.key, page.record) }
      (entries + pages).to[Map]

  // What this compile used, by its key, and the documents and pages it recorded, latest first:
  // each is added to from many threads at once.
  private val used: Atomic.Ref[Map[Text, Text]] = Atomic.Ref(Map())
  private val documents: Atomic.Ref[List[Output.Manifest.Document]] = Atomic.Ref(Nil)
  private val pages: Atomic.Ref[List[Output.Manifest.Page]] = Atomic.Ref(Nil)

  private def use(key: Text, hash: Text): Unit =
    used.since(_.define(key, hash))
    ()

  val hits: Atomic[Int] = Atomic(0)
  val misses: Atomic[Int] = Atomic(0)

  private def write(data: Data): Text = objects.put(data)

  private def read[value](key: Text)(decode: Data => Optional[value]): Optional[value] =
    kept.at(key).let: (hash: Text) =>
      objects.get(hash).let(decode(_)).let: (value: value) =>
        use(key, hash)
        hits.since(_ + 1)
        value

  private def keep(key: Text, data: Data): Unit =
    use(key, write(data))
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

    val hash: Text = used().at(key).or(t"")
    val record: Output.Manifest.Page = Output.Manifest.Page(entity, path, key, hash)
    pages.since(record :: _)
    page

  def document(section: Text, doc: Doc): Unit =
    val record: Output.Document = Output.Document(doc.path, doc.title, doc.blocks, doc.problems)
    val hash: Text = write(Output.Document.encode(record))
    documents.since(Output.Manifest.Document(section, doc.path, doc.title, hash) :: _)
    ()

  // Writes the manifest, and deletes every object it no longer names.
  def finish(fuel: Text, classpath: Text, environment: Text): Unit =
    // In the order they were recorded, before they are sorted, as the sort is stable.
    val recorded: List[Output.Manifest.Page] = pages().reverse
    val pageKeys: Set[Text] = recorded.map(_.key).to[Set]

    val entries: List[(Text, Text)] =
      used().to[List].filter { (key, _) => !pageKeys.has(key) }

    val cache: List[Output.Manifest.Entry] =
      entries.order(_(0)).map(Output.Manifest.Entry(_, _))

    val documented: List[Output.Manifest.Document] =
      documents().reverse.order(_.path).order(_.section)

    val paged: List[Output.Manifest.Page] = recorded.order(_.entity)

    val manifest: Output.Manifest =
      Output.Manifest(fuel, classpath, environment, documented, paged, cache)

    Filing.replace(index, Output.Manifest.encode(manifest))

    val named: Set[Text] =
      ( manifest.documents.map(_.record) + manifest.pages.map(_.record)
        + manifest.cache.map(_.record) ).to[Set]

    objects.prune(named)
