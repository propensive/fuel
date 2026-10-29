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

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import soundness.{Language as _, Token as _, *}

// The subtyping of every named type the model knows, for search to ask quickly whether one type
// is beneath another: each type's key has a dense id, and `supers(id)` is the sorted ids of
// every class it extends — `Entity.bases`, which is already the whole linearisation — so that
// `isSubtype` is one binary search and `beneath`/`above` walk a row each. The universal types
// are predicates rather than rows (`Any` and `Matchable` are above everything; `AnyRef` and
// `java.lang.Object` above every type that is not a value type; `AnyVal` above the primitives
// and the classes whose bases say so; `Nothing` beneath everything, `Null` beneath every
// reference type), so no row is the length of the model. Java classes have no TASTy, so their
// supertypes are known only where `Lattice.javaTypes` says them; any other Java key is a leaf
// beneath `AnyRef`.
object Lattice:
  val any: Text = t"scala.Any"
  val matchable: Text = t"scala.Matchable"
  val anyRef: Text = t"scala.AnyRef"
  val obj: Text = t"java.lang.Object"
  val anyVal: Text = t"scala.AnyVal"
  val nothing: Text = t"scala.Nothing"
  val nul: Text = t"scala.Null"

  val tops: sci.Set[Text] = sci.Set(any, matchable, anyRef, obj, anyVal)
  val bottoms: sci.Set[Text] = sci.Set(nothing, nul)
  val universal: sci.Set[Text] = tops ++ bottoms

  val primitives: sci.Set[Text] =
    sci.Set(t"scala.Int", t"scala.Long", t"scala.Short", t"scala.Byte", t"scala.Char", t"scala.Float", t"scala.Double", t"scala.Boolean", t"scala.Unit")

  // The supertypes of the Java classes which Scala signatures commonly name, each row written
  // out transitively. This is the whole of what is known of Java's hierarchy.
  val javaTypes: sci.Map[Text, sci.List[Text]] =
    val serializable = t"java.io.Serializable"
    val comparable = t"java.lang.Comparable"
    val constable = t"java.lang.constant.Constable"
    val charSequence = t"java.lang.CharSequence"
    val throwable = t"java.lang.Throwable"
    val exception = t"java.lang.Exception"
    val number = t"java.lang.Number"
    val iterable = t"java.lang.Iterable"
    val collection = t"java.util.Collection"

    val boxed: sci.List[Text] =
      sci.List(t"java.lang.Integer", t"java.lang.Long", t"java.lang.Double", t"java.lang.Float", t"java.lang.Short", t"java.lang.Byte")

    sci.Map
      ( t"java.lang.String"         -> sci.List(charSequence, comparable, serializable, constable, t"java.lang.constant.ConstantDesc"),
        t"java.lang.StringBuilder"  -> sci.List(charSequence, comparable, serializable, t"java.lang.Appendable"),
        throwable                   -> sci.List(serializable),
        exception                   -> sci.List(throwable, serializable),
        t"java.lang.RuntimeException" -> sci.List(exception, throwable, serializable),
        t"java.lang.Error"          -> sci.List(throwable, serializable),
        number                      -> sci.List(serializable),
        t"java.lang.Character"      -> sci.List(comparable, serializable, constable),
        t"java.lang.Boolean"        -> sci.List(comparable, serializable, constable),
        collection                  -> sci.List(iterable),
        t"java.util.List"           -> sci.List(collection, iterable),
        t"java.util.Set"            -> sci.List(collection, iterable),
        t"java.util.Queue"          -> sci.List(collection, iterable),
        t"java.util.Deque"          -> sci.List(t"java.util.Queue", collection, iterable) )
    ++ boxed.map { key => key -> sci.List(number, comparable, serializable, constable) }

  // The key a shape is headed by: a named type's own, a type parameter's upper bound's (`Any`
  // when it has none), and nothing for a shape which names no type.
  def head(shape: Api.Shape): Optional[Text] = shape match
    case Api.Shape.Named(key, _)    => key
    case Api.Shape.Param(_, upper)  => upper.lay(any)(head(_).or(any))
    case Api.Shape.Other            => Unset

  def build(api: Api): Lattice =
    val ids: scm.HashMap[Text, Int] = scm.HashMap()
    val keys: scm.ArrayBuffer[Text] = scm.ArrayBuffer()

    def id(key: Text): Int = ids.getOrElseUpdate(key, { keys += key; keys.length - 1 })

    universal.foreach(id)
    primitives.foreach(id)
    javaTypes.foreach { (key, supers) => id(key); supers.foreach(id) }

    def heads(shapes: scala.List[Api.Shape]): Unit = shapes.foreach { shape => head(api.expand(shape)).let(id(_)) }

    val entities: scala.List[Api.Entity] = api.entities.values.toList

    entities.foreach: entity =>
      if entity.template.present || entity.declarations.exists(_.kind.isType) then id(entity.key)
      entity.bases.stdlib.foreach(id)

      entity.declarations.stdlib.foreach: declaration =>
        heads(declaration.returns.let(scala.List(_)).or(scala.Nil))
        heads(declaration.accepts.stdlib)
        heads(declaration.parentShapes.stdlib)

    val size: Int = keys.length
    val supers: scala.Array[scala.Array[Int]] = new scala.Array(size)

    entities.foreach: entity =>
      if !entity.bases.nil then
        ids.get(entity.key).foreach: own =>
          supers(own) = entity.bases.stdlib.map(id).distinct.sorted.toArray

    javaTypes.foreach: (key, parents) =>
      val own: Int = id(key)
      if supers(own) == null then supers(own) = parents.map(id).distinct.sorted.toArray

    var index: Int = 0
    while index < size do
      if supers(index) == null then supers(index) = scala.Array.empty[Int]
      index += 1

    // The inversion, by counting each type's subtypes before placing them.
    val counts: scala.Array[Int] = new scala.Array(size)
    supers.foreach(_.foreach { base => counts(base) += 1 })
    val subs: scala.Array[scala.Array[Int]] = scala.Array.tabulate(size) { base => new scala.Array[Int](counts(base)) }
    val filled: scala.Array[Int] = new scala.Array(size)

    index = 0
    while index < size do
      supers(index).foreach: base =>
        subs(base)(filled(base)) = index
        filled(base) += 1
      index += 1

    val anyValId: Int = id(anyVal)
    val value: scala.Array[Boolean] = scala.Array.tabulate(size) { i => primitives.contains(keys(i)) || _root_.java.util.Arrays.binarySearch(supers(i), anyValId) >= 0 }

    new Lattice(ids.toMap, keys.toArray, supers, subs, value)

final class Lattice private
  ( ids:    sci.Map[Text, Int],
    keys:   scala.Array[Text],
    supers: scala.Array[scala.Array[Int]],
    subs:   scala.Array[scala.Array[Int]],
    value:  scala.Array[Boolean] ):

  private val anyId: Int = ids(Lattice.any)
  private val matchableId: Int = ids(Lattice.matchable)
  private val anyRefId: Int = ids(Lattice.anyRef)
  private val objectId: Int = ids(Lattice.obj)
  private val anyValId: Int = ids(Lattice.anyVal)
  private val nothingId: Int = ids(Lattice.nothing)
  private val nullId: Int = ids(Lattice.nul)

  def size: Int = keys.length
  def id(key: Text): Optional[Int] = ids.get(key).getOrElse(Unset)
  def key(id: Int): Text = keys(id)

  def isTop(id: Int): Boolean = id == anyId || id == matchableId
  def isRefTop(id: Int): Boolean = id == anyRefId || id == objectId
  def isValTop(id: Int): Boolean = id == anyValId
  def isValue(id: Int): Boolean = value(id)

  // Whether a type is a reference type: not a value type, and not above them all.
  def isReference(id: Int): Boolean = !value(id) && !isValTop(id) && !isTop(id) && id != nothingId

  def isSubtype(sub: Int, sup: Int): Boolean =
    sub == sup
    || isTop(sup)
    || sub == nothingId
    || (sub == nullId && (isReference(sup) || isRefTop(sup)))
    || (isRefTop(sup) && (isReference(sub) || isRefTop(sub)))
    || (isValTop(sup) && value(sub))
    || _root_.java.util.Arrays.binarySearch(supers(sub), sup) >= 0

  private def all: Iterator[Int] = Iterator.range(0, size)

  // Every type beneath a type, itself included — but not `Nothing` or `Null`, which are beneath
  // every type and would make every `throw` a result of any type asked for; they are found only
  // by asking for them.
  def beneath(id: Int): Iterator[Int] =
    if isTop(id) then all.filter { i => i != nothingId && i != nullId }
    else if isRefTop(id) then all.filter { i => isReference(i) || isRefTop(i) }
    else if isValTop(id) then all.filter(value(_)) ++ Iterator(anyValId)
    else if id == nothingId then Iterator(nothingId)
    else if id == nullId then Iterator(nullId)
    else Iterator(id) ++ subs(id).iterator

  // Every type above a type, itself included, the universal ones among them.
  def above(id: Int): Iterator[Int] =
    if isTop(id) then Iterator(anyId, matchableId)
    else if id == nothingId then all
    else if id == nullId then all.filter { i => isReference(i) || isRefTop(i) || isTop(i) || i == nullId }
    else
      val tops: Iterator[Int] =
        if isValTop(id) then Iterator(anyId, matchableId)
        else if value(id) then Iterator(anyValId, anyId, matchableId)
        else Iterator(anyRefId, objectId, anyId, matchableId)

      Iterator(id) ++ supers(id).iterator ++ tops
