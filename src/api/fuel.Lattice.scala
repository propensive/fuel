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

import soundness.collations.codepointCollation
import dysasymptotics.linearSize
import soundness.collationComparable
import soundness.sortingAlgorithms.timsort

// The subtyping of every named type the model knows, for search to ask quickly whether one type is
// beneath another: each type's key has a dense id, and `supers(id)` is the sorted ids of every
// class it extends — `Entity.bases`, which is already the whole linearisation — so that `isSubtype`
// is one binary search and `beneath`/`above` walk a row each; `distance` is the base's place in the
// linearisation, nearest first, which orders what a type inherits. The universal types are
// predicates rather than rows (`Any` and `Matchable` are above everything; `AnyRef` and
// `java.lang.Object` above every type that is not a value type; `AnyVal` above the primitives and
// the classes whose bases say so; `Nothing` beneath everything, `Null` beneath every reference
// type), so no row is the length of the model. Besides the linearisation, each type keeps its
// *direct* parents as written (`parents`), and so its direct `children`, which a hierarchy is drawn
// from. Java classes have no TASTy, so their supertypes are known only where `Lattice.javaTypes`
// says them; any other Java key is a leaf beneath `AnyRef`.
object Lattice:
  val any: Text = t"scala.Any"
  val matchable: Text = t"scala.Matchable"
  val anyRef: Text = t"scala.AnyRef"
  val obj: Text = t"java.lang.Object"
  val anyVal: Text = t"scala.AnyVal"
  val nothing: Text = t"scala.Nothing"
  val nul: Text = t"scala.Null"

  val tops: Set[Text] = Set(any, matchable, anyRef, obj, anyVal)
  val bottoms: Set[Text] = Set(nothing, nul)
  val universal: List[Text] = List(any, matchable, anyRef, obj, anyVal, nothing, nul)

  val primitives: List[Text] =
    List
      ( t"scala.Int",
        t"scala.Long",
        t"scala.Short",
        t"scala.Byte",
        t"scala.Char",
        t"scala.Float",
        t"scala.Double",
        t"scala.Boolean",
        t"scala.Unit" )

  private val primitive: Set[Text] = primitives.to[Set]

  // The supertypes of the Java classes which Scala signatures commonly name, each row written
  // out transitively. This is the whole of what is known of Java's hierarchy.
  val javaTypes: List[(Text, List[Text])] =
    val serializable = t"java.io.Serializable"
    val comparable = t"java.lang.Comparable"
    val constable = t"java.lang.constant.Constable"
    val charSequence = t"java.lang.CharSequence"
    val throwable = t"java.lang.Throwable"
    val exception = t"java.lang.Exception"
    val number = t"java.lang.Number"
    val iterable = t"java.lang.Iterable"
    val collection = t"java.util.Collection"
    val constantDesc = t"java.lang.constant.ConstantDesc"

    val boxed: List[Text] =
      List
        ( t"java.lang.Integer",
          t"java.lang.Long",
          t"java.lang.Double",
          t"java.lang.Float",
          t"java.lang.Short",
          t"java.lang.Byte" )

    val stringSupers: List[Text] =
      List(charSequence, comparable, serializable, constable, constantDesc)

    val builderSupers: List[Text] =
      List(charSequence, comparable, serializable, t"java.lang.Appendable")

    val listed: List[(Text, List[Text])] =
      List
        ( t"java.lang.String"           -> stringSupers,
          t"java.lang.StringBuilder"    -> builderSupers,
          throwable                     -> List(serializable),
          exception                     -> List(throwable, serializable),
          t"java.lang.RuntimeException" -> List(exception, throwable, serializable),
          t"java.lang.Error"            -> List(throwable, serializable),
          number                        -> List(serializable),
          t"java.lang.Character"        -> List(comparable, serializable, constable),
          t"java.lang.Boolean"          -> List(comparable, serializable, constable),
          collection                    -> List(iterable),
          t"java.util.List"             -> List(collection, iterable),
          t"java.util.Set"              -> List(collection, iterable),
          t"java.util.Queue"            -> List(collection, iterable),
          t"java.util.Deque"            -> List(t"java.util.Queue", collection, iterable) )

    listed + boxed.map { key => (key, List(number, comparable, serializable, constable)) }

  // Every Java key the lattice knows: those with rows, and those the rows name.
  val javaKeys: List[Text] = javaTypes.bind { (key, supers) => key :: supers }.distinct

  // The key a shape is headed by: a named type's own, a type parameter's upper bound's (`Any`
  // when it has none), and nothing for a shape which names no type.
  def head(shape: Api.Shape): Optional[Text] = shape match
    case Api.Shape.Named(key, _)    => key
    case Api.Shape.Param(_, upper)  => upper.lay(any)(head(_).or(any))
    case Api.Shape.Other            => Unset

  // Soundness has no binary search (soundness#2501), so a sorted row is searched by the JDK's:
  // the index of the value in the row, or a negative number where it is not there.
  private[fuel] def search(row: Array[Int], value: Int): Int =
    java.util.Arrays.binarySearch(Array.unsafeJvm(row), value)

  def build(api: Api): Lattice =
    val entities: List[Api.Entity] = api.entities.values

    def heads(shapes: List[Api.Shape]): List[Text] =
      shapes.bind { shape => head(api.expand(shape)).lay(Nil)(List(_)) }

    // A template's direct parents, as written, by the key of each type; `java.lang.Object` is
    // `AnyRef`.
    def written(entity: Api.Entity): List[Text] = entity.template.lay(Nil): template =>
      heads(template.parentShapes).map { key => if key == obj then anyRef else key }.distinct

    // Every key, in the order it is first met, which is the order of the ids.
    val met: List[Text] =
      val named: List[Text] = entities.bind: entity =>
        val own: List[Text] =
          if entity.template.present || entity.declarations.exists(_.kind.isType)
          then List(entity.key)
          else Nil

        val declared: List[Text] = entity.declarations.bind: declaration =>
          heads(declaration.returns.lay(Nil)(List(_))) + heads(declaration.accepts) +
            heads(declaration.parentShapes)

        own + entity.bases + declared + written(entity)

      universal + primitives + javaKeys + named

    val keys: Array[Text] = met.distinct.to[Array]
    val size: Int = keys.length
    val ids: Map[Text, Int] = keys.to[List].indexed.map { (key, index) => (key, index.n0) }.to[Map]
    def id(key: Text): Int = ids.at(key).or(-1)

    // Each type's bases, nearest first: its entity's linearisation, or what `javaTypes` says.
    val bases: Map[Int, List[Text]] =
      val java: List[(Int, List[Text])] = javaTypes.map { (key, supers) => (id(key), supers) }

      val modelled: List[(Int, List[Text])] =
        entities.filter(!_.bases.nil).map { entity => (id(entity.key), entity.bases) }

      (java + modelled).filter(_(0) >= 0).to[Map]

    // A row of bases, nearest first, as ids sorted for searching, each with its distance (its
    // place in the row, from 1) alongside.
    def row(own: Int): List[(Int, Int)] = bases.at(own).lay(Nil): (bases: List[Text]) =>
      bases.map(id).indexed.deduplicate(_(0)).map { (base, index) => (base, index.n1) }
      . order(_(0))

    val rows: List[List[(Int, Int)]] = List.range(0, size).map(row)
    val supers: Array[Array[Int]] = rows.map(_.map(_(0)).to[Array]).to[Array]
    val distances: Array[Array[Int]] = rows.map(_.map(_(1)).to[Array]).to[Array]

    val parents: Map[Int, List[Int]] =
      entities.map { entity => (id(entity.key), written(entity).map(id)) }
      . filter { (own, parents) => own >= 0 && !parents.nil }
      . to[Map]

    val directSupers: List[List[Int]] = List.range(0, size).map(parents.at(_).or(Nil))

    // An inversion: for each type, the types whose rows name it, in order.
    def invert(rows: List[List[Int]]): List[List[Int]] =
      val pairs: List[(Int, Int)] =
        rows.indexed.bind { (row, index) => row.map { base => (base, index.n0) } }

      val inverted: Map[Int, List[Int]] = pairs.group(_(0)).map(_.map(_(1)))
      List.range(0, size).map(inverted.at(_).or(Nil))

    val subs: List[List[Int]] = invert(rows.map(_.map(_(0))))

    val directSubs: List[List[Int]] =
      invert(directSupers).map(_.order(keys.readUnchecked(_)))

    val anyValId: Int = id(anyVal)

    val value: List[Boolean] = List.range(0, size).map: index =>
      primitive.has(keys.readUnchecked(index)) || search(supers.readUnchecked(index), anyValId) >= 0

    def array(rows: List[List[Int]]): Array[Array[Int]] = rows.map(_.to[Array]).to[Array]

    new Lattice
      ( ids,
        keys,
        supers,
        distances,
        array(subs),
        array(directSupers),
        array(directSubs),
        value.to[Array] )

// Each row is read with `readUnchecked`, by an id, which is always below `size`, or by an index a
// search of the row found.
final class Lattice private
  ( ids:          Map[Text, Int],
    keys:         Array[Text],
    supers:       Array[Array[Int]],
    distances:    Array[Array[Int]],
    subs:         Array[Array[Int]],
    directSupers: Array[Array[Int]],
    directSubs:   Array[Array[Int]],
    value:        Array[Boolean] ):

  private val anyId: Int = ids.at(Lattice.any).or(-1)
  private val matchableId: Int = ids.at(Lattice.matchable).or(-1)
  private val anyRefId: Int = ids.at(Lattice.anyRef).or(-1)
  private val objectId: Int = ids.at(Lattice.obj).or(-1)
  private val anyValId: Int = ids.at(Lattice.anyVal).or(-1)
  private val nothingId: Int = ids.at(Lattice.nothing).or(-1)
  private val nullId: Int = ids.at(Lattice.nul).or(-1)

  def size: Int = keys.length

  // A type's direct parents, as written, and the types which name it as one; the universal
  // types are nobody's written parent.
  def parents(id: Int): List[Int] = directSupers.readUnchecked(id).to[List]
  def children(id: Int): List[Int] = directSubs.readUnchecked(id).to[List]
  def primaryParent(id: Int): Optional[Int] = parents(id).head

  def id(key: Text): Optional[Int] = ids.at(key)
  def key(id: Int): Text = keys.readUnchecked(id)

  def isTop(id: Int): Boolean = id == anyId || id == matchableId
  def isRefTop(id: Int): Boolean = id == anyRefId || id == objectId
  def isValTop(id: Int): Boolean = id == anyValId
  def isValue(id: Int): Boolean = value.readUnchecked(id)

  // Whether a type is a reference type: not a value type, and not above them all.
  def isReference(id: Int): Boolean =
    !isValue(id) && !isValTop(id) && !isTop(id) && id != nothingId

  // A reference type, or `AnyRef` (or `java.lang.Object`) itself: the types `Null` is beneath.
  private def isReferable(id: Int): Boolean = isReference(id) || isRefTop(id)

  def isSubtype(sub: Int, sup: Int): Boolean =
    sub == sup ||
      isTop(sup) ||
      sub == nothingId ||
      (sub == nullId && isReferable(sup)) ||
      (isRefTop(sup) && isReferable(sub)) ||
      (isValTop(sup) && isValue(sub)) ||
      Lattice.search(supers.readUnchecked(sub), sup) >= 0

  // How far a type is beneath another: 0 for itself, the base's place in its linearisation
  // (nearest first, from 1) for a base, beyond every base for a universal type, and -1 where it
  // is not beneath it at all.
  def distance(sub: Int, sup: Int): Int =
    if sub == sup then 0
    else
      val row: Array[Int] = supers.readUnchecked(sub)
      val found: Int = Lattice.search(row, sup)

      if found >= 0 then distances.readUnchecked(sub).readUnchecked(found)
      else if isSubtype(sub, sup) then row.length + 1
      else -1

  private def all: List[Int] = List.range(0, size)

  // Every type beneath a type, itself included — but not `Nothing` or `Null`, which are beneath
  // every type and would make every `throw` a result of any type asked for; they are found only
  // by asking for them.
  def beneath(id: Int): List[Int] =
    if isTop(id) then all.filter: i => i != nothingId && i != nullId
    else if isRefTop(id) then all.filter(isReferable(_))
    else if isValTop(id) then all.filter(isValue(_)) + List(anyValId)
    else if id == nothingId then List(nothingId)
    else if id == nullId then List(nullId)
    else id :: subs.readUnchecked(id).to[List]

  // Every type above a type, itself included, the universal ones among them.
  def above(id: Int): List[Int] =
    if isTop(id) then List(anyId, matchableId)
    else if id == nothingId then all
    else if id == nullId then all.filter: i => isReferable(i) || isTop(i) || i == nullId
    else
      val tops: List[Int] =
        if isValTop(id) then List(anyId, matchableId)
        else if isValue(id) then List(anyValId, anyId, matchableId)
        else List(anyRefId, objectId, anyId, matchableId)

      (id :: supers.readUnchecked(id).to[List]) + tops
