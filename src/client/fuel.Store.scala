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

import java.nio.file as jnf
import java.util.concurrent as juc

import scala.collection.immutable as sci
import scala.jdk.CollectionConverters.*

import soundness.*

import pyrocosm.Block

object Store:
  // The manifest of an output directory, if it has one.
  def manifest(output: Text): Optional[Output.Manifest] =
    val index: jnf.Path = jnf.Path.of(output.s).nn.resolve("index.fuel").nn
    if !jnf.Files.exists(index) then Unset else Output.Manifest.decode(bytes(index))

  private def bytes(path: jnf.Path): Data = Array.unsafeFrozen(jnf.Files.readAllBytes(path).nn)

  // Bytes written to a temporary file beside `target`, then moved over it, so that a reader
  // sees the whole of one or the whole of the other.
  private def place(directory: jnf.Path, target: jnf.Path, data: Data): Unit =
    val temporary: jnf.Path = jnf.Files.createTempFile(directory, "write", ".tmp").nn
    jnf.Files.write(temporary, Array.unsafeJvm(data))

    jnf.Files.move
      ( temporary, target, jnf.StandardCopyOption.ATOMIC_MOVE,
        jnf.StandardCopyOption.REPLACE_EXISTING )

// The output directory as a cache: `index.fuel`, the manifest, and `objects/`, where every
// record is a BinTEL file named by the Blake3 hash of its bytes, so that two records never
// collide whatever they hold, and a record already written is not written again. What the last
// compile kept is read through the old manifest; what this compile uses or writes goes into the
// new one, which is written last, by a move, so that a reader sees one whole manifest or the
// other; then every record the new manifest does not name is deleted. `fresh` starts with
// nothing kept, which is what `--force` asks for.
class Store(output: Text, fresh: Boolean) extends Cache:
  private val root: jnf.Path = jnf.Path.of(output.s).nn
  private val objects: jnf.Path = root.resolve("objects").nn
  private val index: jnf.Path = root.resolve("index.fuel").nn

  jnf.Files.createDirectories(objects)

  // The last compile's cache and pages, from each key to the hash of its record.
  private val kept: sci.Map[Text, Text] =
    val manifest: Optional[Output.Manifest] = if fresh then Unset else Store.manifest(output)

    manifest.lay(sci.Map()): (manifest: Output.Manifest) =>
      val entries = manifest.cache.stdlib.map: entry => (entry.key, entry.record)
      val pages = manifest.pages.stdlib.map: page => (page.key, page.record)

      sci.Map.from(entries ++ pages)

  // This compile's cache, documents and pages.
  private val used: juc.ConcurrentHashMap[Text, Text] = juc.ConcurrentHashMap()

  private val documents: juc.ConcurrentLinkedQueue[Output.Manifest.Document] =
    juc.ConcurrentLinkedQueue()

  private val pages: juc.ConcurrentLinkedQueue[Output.Manifest.Page] = juc.ConcurrentLinkedQueue()

  // How many records were found kept, and how many were compiled, for the report.
  val hits: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)
  val misses: juc.atomic.AtomicInteger = juc.atomic.AtomicInteger(0)

  private def file(hash: Text): jnf.Path = objects.resolve(s"$hash.fuel").nn

  // A record's bytes, written under their hash unless they are there already; its hash.
  private def write(data: Data): Text =
    val hash: Text = Hashes.of(data)
    val target: jnf.Path = file(hash)
    if !jnf.Files.exists(target) then Store.place(objects, target, data)
    hash

  private def read[value](key: Text)(decode: Data => Optional[value]): Optional[value] =
    kept.get(key).fold(Unset)(identity).let: (hash: Text) =>
      val target: jnf.Path = file(hash)

      if !jnf.Files.exists(target) then Unset else
        decode(Store.bytes(target)).let: (value: value) =>
          used.put(key, hash)
          hits.incrementAndGet()
          value

  private def keep(key: Text, data: Data): Unit =
    used.put(key, write(data))
    misses.incrementAndGet()

  def fragment(key: Text): Optional[Output.Fragment] = read(key)(Output.Fragment.decode)

  def fragment(key: Text, fragment: Output.Fragment): Unit =
    keep(key, Output.Fragment.encode(fragment))

  def prose(key: Text): Optional[Output.Prose] = read(key)(Output.Prose.decode)
  def prose(key: Text, prose: Output.Prose): Unit = keep(key, Output.Prose.encode(prose))

  // A page, by the key of its inputs: kept, or rendered (with its problems) and kept; named in
  // the manifest.
  def page(key: Text, entity: Text, path: Text)(render: => (List[Block], List[Block]))
  :   Output.Page =

    val page: Output.Page = read(key)(Output.Page.decode).or:
      val (blocks, problems) = render
      val page: Output.Page = Output.Page(entity, path, blocks, problems)
      keep(key, Output.Page.encode(page))
      page

    pages.add(Output.Manifest.Page(entity, path, key, used.get(key).nn))
    page

  // A document as compiled, named in the manifest by its section and path.
  def document(section: Text, doc: Doc): Unit =
    val record: Output.Document = Output.Document(doc.path, doc.title, doc.blocks, doc.problems)
    val hash: Text = write(Output.Document.encode(record))
    documents.add(Output.Manifest.Document(section, doc.path, doc.title, hash))

  // The manifest, written whole, and every record it does not name deleted.
  def finish(fuel: Text, classpath: Text, environment: Text): Unit =
    val pageKeys: Set[Text] = Set.from(pages.asScala.map(_.key))
    val entries = used.asScala.toList.filter: entry => !pageKeys.has(entry(0))

    val cache = List.from(entries.sortBy(_(0).s).map(Output.Manifest.Entry(_, _)))

    // By section, then by path within it: a stable sort by path, then by section.
    val documented = List.from(documents.asScala.toList.sortBy(_.path.s).sortBy(_.section.s))

    val paged = List.from(pages.asScala.toList.sortBy(_.entity.s))

    val manifest: Output.Manifest =
      Output.Manifest(fuel, classpath, environment, documented, paged, cache)

    Store.place(root, index, Output.Manifest.encode(manifest))

    val named: Set[Text] =
      Set.from
        ( manifest.documents.stdlib.map(_.record) ++
          manifest.pages.stdlib.map(_.record) ++
          manifest.cache.stdlib.map(_.record) )

    val stream = jnf.Files.list(objects).nn

    try
      stream.iterator.nn.asScala.foreach: (path: jnf.Path) =>
        val name: String = path.getFileName.nn.toString

        if name.endsWith(".tmp") then jnf.Files.deleteIfExists(path)
        else if name.endsWith(".fuel") && !named.has(name.stripSuffix(".fuel").tt)
        then jnf.Files.delete(path)

    finally stream.close()
