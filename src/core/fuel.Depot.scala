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
import dysasymptotics.linearSize
import pyrocosm.{Block, Tone}
import soundness.collationComparable
import sortingAlgorithms.timsort

// An Orthant store as Fuel reads it. A store holds each document at every point of its axes —
// a language, a release — and Fuel reads one point for each language: the store's `base`, the
// value the configuration gives for each axis which is not the language's (`at`), moved to
// each value of the axis of languages in turn. What a point holds is the plain text of each
// document, as `orthant checkout` would write it into the working tree; so the store is read
// as if it were a tree of its documents, a document `api/gossamer.tel` being the file of that
// path in the tree's directory. That directory is the store's live tree, where it has one —
// the directory the documents are written in, which is committed — and its working tree
// otherwise, whether or not anything is checked out there. Nothing is read from either tree
// itself, and so nothing which is not in the store: an edit to the live tree is read once
// `orthant tag` has stored it, and until then is counted (`unstored`), so that it can be said.
//
// Fuel only reads a store (through `orthant-reader`). A translation is written, held to the
// text it translates and brought up to date with the `orthant` tool, which is also what says
// how much of one is done; Fuel reads each language's documents as they stand, and holds each
// to what a document of its kind must be.
object Depot:
  // A problem's sentence with its code.
  type Problem = (Text, fulminate.Bolt)

  // The code of a store's problem, `↯/f.25.15.<variant>`: a store which cannot be opened,
  // which carries Orthant's own code (1); an `at` which names no value of an axis, carrying
  // Orthant's code too (2); an axis which is not a line of values, and which no `at` names
  // (3); a value of the axis of languages which is not a language (4); and a document which
  // cannot be derived at a point, with Orthant's code (5).
  private[fuel] def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.core, FuelBolts.depot, variant)

  // The language a value of an axis of languages names: `fr-CA` for `fr/CA`.
  def tongue(value: Text): Text = value.cut(t"/").join(t"-")

  // A directory as it is named, without a `/` or a `/.` at its end.
  private def trimmed(path: Text): Text =
    if path.length > 2 && path.ends(t"/.") then trimmed(path.keep(path.length - 2))
    else if path.length > 1 && path.ends(t"/") then trimmed(path.keep(path.length - 1))
    else path

  // The store whose home — the directory which has `.orthant` in it — is given, read at the
  // values the terms name, each `axis=value` or a value alone where one axis has it; with
  // what is wrong with it, or with them. No store is read where it cannot be opened.
  def open(home: Text, terms: List[Text] = Nil): (Optional[Depot], List[Problem]) =
    import filesystemBackends.javaBaseFilesystem

    def unopened(error: orthant.Store.Error): orthant.Store | Problem =
      (t"the Orthant store in `$home` cannot be read: ${orthant.Orthant.prose(error)}",
       bolt(1).carrying(error.bolt))

    val missing: Problem = (t"`$home` is not a directory an Orthant store can be in", bolt(1))

    val opened: orthant.Store | Problem =
      safely(trimmed(home).as[Path on Linux]).lay(missing: orthant.Store | Problem):
        (path: Path on Linux) =>
          recover:
            case error: orthant.Store.Error => unopened(error)

          . protect[orthant.Store | Problem](orthant.Store.open(path))

    opened match
      case store: orthant.Store =>
        val (depot, problems) = read(trimmed(home), store, terms)
        (depot, problems)

      case problem: Problem @unchecked =>
        (Unset, List(problem))

  // The axis whose values are languages: the first of the kind `language`.
  private def spoken(store: orthant.Store): Optional[orthant.Axis] =
    store.axes.filter { (axis: orthant.Axis) => store.entry(axis.name).let(_.kind) == t"language" }
    . head

  // The last value of an axis whose values are a line, each beneath the one before: the
  // latest release, say, or what is not yet released on the branch which follows it. An axis
  // which branches has no such value.
  private def latest(axis: orthant.Axis): Optional[Text] =
    def recur(value: Text): Optional[Text] = axis.children(value) match
      case Nil         => value
      case next :: Nil => recur(next)
      case _           => Unset

    recur(axis.root)

  private def read(home: Text, store: orthant.Store, terms: List[Text])
  :   (Optional[Depot], List[Problem]) =

    val language: Optional[orthant.Axis] = spoken(store)

    // Each term, as the axis and the value it names, or what is wrong with it.
    val named: List[(orthant.Axis, Text) | Problem] = terms.map: (term: Text) =>
      recover:
        case error: orthant.Store.Error =>
          (t"`at $term` names no value of the store in `$home`: ${orthant.Orthant.prose(error)}",
           bolt(2).carrying(error.bolt)): (orthant.Axis, Text) | Problem

      . protect[(orthant.Axis, Text) | Problem](store.term(term))

    val chosen: List[(orthant.Axis, Text)] = named.bind:
      case (axis: orthant.Axis, value: Text) => List((axis, value))
      case _                                 => Nil

    val misnamed: List[Problem] = named.bind:
      case (text: Text, code: fulminate.Bolt) => List((text, code))
      case _                                  => Nil

    // Each axis but the language's, at the value a term names, or else its latest.
    val others: List[orthant.Axis] =
      store.axes.filter { (axis: orthant.Axis) => language.lay(true)(_.name != axis.name) }

    val placed: List[(orthant.Axis, Optional[Text])] = others.map: (axis: orthant.Axis) =>
      val said: Optional[Text] = chosen.filter(_(0).name == axis.name).reverse.head.let(_(1))
      (axis, said.or(latest(axis)))

    val branching: List[Problem] = placed.filter(_(1).absent).map: (axis: orthant.Axis, _) =>
      val message: Text =
        t"the axis `${axis.name}` of the store in `$home` has more than one line of values, " +
          t"so nothing says which the documents are read at: an `at` beneath `store` does, " +
          t"as `at ${axis.name}=${axis.root}`"

      (message, bolt(3))

    val base: orthant.Point = placed.fold(store.space.root):
      (point: orthant.Point, place: (orthant.Axis, Optional[Text])) =>
        place(1).lay(point)(point.move(place(0), _))

    // The languages, the one the documents are written in first.
    val values: List[Text] = language.lay(Nil): (axis: orthant.Axis) =>
      axis.root :: axis.values.map(_.name).filter(_ != axis.root)

    def speakable(value: Text): Boolean = Tongue.valid(tongue(value))

    val unspoken: List[Problem] = values.filter(!speakable(_)).map: (value: Text) =>
      val message: Text =
        t"`$value`, a value of the axis `${language.let(_.name).or(t"")}` of the store in " +
          t"`$home`, is not a language, as `fr` or `pt/BR`"

      (message, bolt(4))

    val languages: List[(Text, Text)] =
      values.filter(speakable(_)).map: (value: Text) => (tongue(value), value)

    (Depot(home, store, base, language, languages), misnamed + branching + unspoken)

// A store, opened: its home; the point each document is read at, but for its language; the
// axis of languages, if it has one; and each language it is read in, as a language and the
// value of that axis which names it, the one the documents are written in first.
case class Depot
  ( home:      Text,
    store:     orthant.Store,
    base:      orthant.Point,
    axis:      Optional[orthant.Axis],
    languages: List[(Text, Text)] ):

  // The directory every document's path is within: the store's live tree, where it has one,
  // and its working tree otherwise.
  val root: Text =
    val tree: Text = store.settings.live.or(store.settings.tree)
    if tree == t"." || tree == t"" then home else t"$home/$tree"

  // The point the live tree is written at, in a store which has one: the working value on
  // its axis, and the root of every other; the root point, where no value is the working one.
  val live: Optional[orthant.Point] = store.settings.live.let: (directory: Text) =>
    val origin: orthant.Point = store.space.root

    store.settings.working.lay(origin):
      (entry: orthant.AxisEntry, value: orthant.ValueEntry) =>
        store.axes.filter(_.name == entry.name).head.lay(origin)(origin.move(_, value.name))

  // The documents of the live tree which are not as the store has them — edited, added or
  // deleted since `orthant tag` last stored the tree — where the documents are read at the
  // point the live tree is written at: they are read as the store has them. None, where the
  // documents are read at another point, such as an earlier release; and none where the live
  // tree's directory is not there at all, since that is no edit of every document.
  def unstored: List[Text] =
    if live.absent || live != point() || !Library.Disk.directory(root) then Nil else
      val written: List[Text] = Library.Disk.files(root, t"").bind: (path: Text) =>
        document(path).let(List(_)).or(Nil)

      val held: List[Text] = store.documents.filter(text(_).present)

      val deleted: List[Text] = held.filter: (held: Text) =>
        !written.exists(_ == held)

      (written + deleted).filter: (document: Text) =>
        Library.Disk.text(path(document)) != text(document)

      . order(identity(_))

  // The language the documents are written in, and those they are translated into.
  def original: Optional[Text] = axis.let { (axis: orthant.Axis) => Depot.tongue(axis.root) }
  def tongues: List[Text] = languages.map(_(0)).filter { (tongue: Text) => original != tongue }

  // The point a language reads: the language the documents are written in, where none is said.
  def point(tongue: Optional[Text] = Unset): orthant.Point =
    val value: Optional[Text] = tongue.let: (tongue: Text) =>
      languages.seek(_(0) == tongue).let(_(1))

    axis.lay(base) { (axis: orthant.Axis) => value.lay(base)(base.move(axis, _)) }

  // The values the documents are read at, on each axis but the language's, as they are shown.
  def place: Text =
    store.axes.filter { (other: orthant.Axis) => axis.lay(true)(_.name != other.name) }.map:
      (other: orthant.Axis) => t"${other.name}=${store.title(other.name, base(other))}"

    . join(t", ")

  // The document a path is, if it is within the tree.
  def document(path: Text): Optional[Text] =
    if path.starts(t"$root/") then path.skip(root.length + 1) else Unset

  // The file a document would be, checked out.
  def path(document: Text): Text = t"$root/$document"

  // A document's text as a language reads it, if it has one there which can be derived.
  def text(document: Text, tongue: Optional[Text] = Unset): Optional[Text] =
    safely(store.content(document, point(tongue))).let(orthant.Store.text(_))

  // Whether a language reads a document as it was written, nobody having translated any of it.
  def untranslated(document: Text, tongue: Text): Boolean =
    original != tongue && text(document, tongue) == text(document)

  // The files as a language reads them: within the tree, the store's documents at
  // the language's point; anywhere else, the filesystem's.
  def library(tongue: Optional[Text] = Unset): Library =
    val depot: Depot = this

    new Library:
      def text(path: Text): Optional[Text] =
        depot.document(path).lay(Library.Disk.text(path))(depot.text(_, tongue))

      // The store's documents beneath a directory of the tree, which a language has.
      private def held(prefix: Text, suffix: Text): List[Text] =
        def has(document: Text): Boolean =
          document.starts(prefix) && document.ends(suffix) &&
            depot.text(document, tongue).present

        depot.store.documents.filter(has(_)).map(depot.path(_))

      def files(directory: Text, suffix: Text): List[Text] =
        if directory == depot.root then held(t"", suffix)
        else depot.document(directory).let { (within: Text) => held(t"$within/", suffix) }.or:
          val outside: List[Text] =
            Library.Disk.files(directory, suffix).filter(depot.document(_).absent)

          // A directory which the tree is within has the tree's documents too.
          if !depot.root.starts(t"$directory/") then outside
          else (outside + held(t"", suffix)).order(identity(_))

      def directory(path: Text): Boolean =
        if path == depot.root then true
        else depot.document(path).lay(Library.Disk.directory(path)): (within: Text) =>
          depot.store.documents.exists(_.starts(t"$within/"))

  // Each document which cannot be derived as a language reads it, as a failure at the file
  // it would be: a stored change which has no one place in the text it was written against.
  def failures(tongue: Optional[Text] = Unset): List[Block] =
    store.documents.bind: (document: Text) =>
      recover:
        case error: orthant.Store.Error =>
          val message: Text =
            t"This cannot be read from the Orthant store${in(tongue)}: " +
              t"${orthant.Orthant.prose(error)}. `orthant check` says what is wrong with the store."

          val code: fulminate.Bolt = Depot.bolt(5).carrying(error.bolt)
          val location: Block.Location = Block.Location(path(document))
          val said: List[Block] = List(Block.paragraph(message))
          List(Block.Diagnostic(Tone.Failure, location, said, code = code))

      . protect[List[Block]]:
          store.content(document, point(tongue))
          Nil

  private def in(tongue: Optional[Text]): Text =
    tongue.lay(t""): (tongue: Text) => t", in `$tongue`"

  // How much of a language's translation is done, as Orthant counts it: the lines there are
  // to translate, those nobody has, the passages written for text which has since changed,
  // and the changes which conflict.
  def work(tongue: Text): Optional[orthant.Survey.Sum] = safely(store.work(point(tongue)))

  // A problem found in a document as a language reads it, which says so: the file it names
  // is the one `orthant checkout` writes for that language.
  def attributed(block: Block, tongue: Text): Block = block match
    case diagnostic: Block.Diagnostic =>
      val value: Text = languages.seek(_(0) == tongue).let(_(1)).or(tongue)

      val note: Block =
        Block.paragraph
          ( t"This is the file as `$tongue` reads it, which `orthant checkout $value` checks out." )

      diagnostic.copy(detail = diagnostic.detail + List(note))

    case other =>
      other
