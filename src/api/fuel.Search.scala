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

import soundness.{Query as _, *}

import caseSensitivity.caseInsensitive
import proximities.levenshteinProximity
import pyrocosm.Control

// A search of an `Api`, built once for it (`Api.search`): every declaration is an entry, with
// the id in the `Lattice` of the type it returns and of each type it accepts, and the entries
// are posted by those ids, so that `+T` is the union of the postings of every type beneath `T`
// and `-T` of every type above it — the cost of the hits, not of the model. Names are matched
// through a `Lexicon` (a BK-tree over every simple name) at a radius widening from 0 to 2 until
// something is found; a backticked name is looked up exactly. The remaining criteria — kind,
// form, modifiers, package — are predicates over what survives. Hits are ordered: those named
// exactly first, then the project's own before its dependencies', then by key.
//
// Type arguments are compared structurally, variance aside, where the heads coincide
// (`+List[Int]` against a `List[Int]`); where the hit's type is a strict subtype of the query's
// (`Doc` against `+Shape[Int]`), its direct parents are consulted (`Doc extends Shape[Int]`),
// and no further. A `-T[…]` against a strict supertype matches by head alone.
object Search:
  case class Hit(entity: Api.Entity, declaration: Api.Declaration, exact: Boolean)
  case class Outcome(hits: List[Hit], notices: List[Text])

  // The names of the universal types, which have no entity, and the keys they stand for.
  private val universal: sci.Map[Text, Text] =
    sci.Map
      ( t"Any" -> Lattice.any, t"Matchable" -> Lattice.matchable, t"AnyRef" -> Lattice.anyRef,
        t"Object" -> Lattice.obj, t"AnyVal" -> Lattice.anyVal, t"Nothing" -> Lattice.nothing,
        t"Null" -> Lattice.nul )

  private case class Entry
    ( entity:      Api.Entity,
      declaration: Api.Declaration,
      result:      Optional[Api.Shape],   // expanded
      resultHead:  Int,                   // -1 for none
      params:      scala.Array[Api.Shape],
      paramHeads:  scala.Array[Int] )

  // The most meanings a written type's arguments are tried in, and the most completions offered.
  private val alternatives: Int = 64
  private val completed: Int = 20

  def build(api: Api): Search = new Search(api, api.lattice)

final class Search private (api: Api, lattice: Lattice):
  import Search.*

  private def headId(shape: Api.Shape): Int = Lattice.head(shape).let(lattice.id(_)).or(-1)

  // Whether an entity is a type, or declares one; whether it is a package or an object.
  private def isType(e: Api.Entity): Boolean =
    e.template.present || e.declarations.exists(_.kind.isType)

  private def container(e: Api.Entity): Boolean =
    e.isPackage || e.definition.let(_.kind == Api.Kind.Object).or(false)

  private val entries: scala.Array[Entry] =
    val sorted: scala.List[Api.Entity] =
      api.entities.values.toList.filter { e => !e.hidden && !e.isParameter }.sortBy(_.key.s)

    sorted.flatMap: entity =>
      entity.declarations.stdlib.map: declaration =>
        val provided: Optional[Api.Shape] =
          if declaration.kind == Api.Kind.Given then declaration.provides else Unset

        val returns: Optional[Api.Shape] = declaration.returns.or(provided)
        val result: Optional[Api.Shape] = returns.let(api.expand(_))
        val params: scala.Array[Api.Shape] = declaration.accepts.stdlib.map(api.expand(_)).toArray
        Entry(entity, declaration, result, result.let(headId(_)).or(-1), params, params.map(headId))

    . toArray

  private def postings(heads: Entry => scala.Array[Int]): scala.Array[scala.Array[Int]] =
    val buffers: scala.Array[scm.ArrayBuffer[Int] | Null] = new scala.Array(lattice.size)

    entries.indices.foreach: index =>
      heads(entries(index)).distinct.foreach: head =>
        if head >= 0 then
          val buffer: scm.ArrayBuffer[Int] = buffers(head) match
            case null                         => scm.ArrayBuffer[Int]().tap: b => buffers(head) = b
            case buffer: scm.ArrayBuffer[Int] => buffer

          buffer += index

    buffers.map: b => if b == null then scala.Array.empty[Int] else b.toArray

  private val byResult: scala.Array[scala.Array[Int]] = postings: entry =>
    if entry.resultHead < 0 then scala.Array.empty[Int] else scala.Array(entry.resultHead)

  private val byParam: scala.Array[scala.Array[Int]] = postings(_.paramHeads)

  private val byEntity: sci.Map[Text, scala.Array[Int]] =
    entries.indices.groupBy { index => entries(index).entity.key }.map: (key, indices) =>
      (key, indices.toArray)

  private lazy val lexicon: Lexicon[List[Api.Entity]] =
    Lexicon[List[Api.Entity]]().tap: lexicon =>
      api.byName.foreach: (name, entities) => lexicon(name) = entities

  // The entities a name finds: exactly, or at the smallest radius which finds any.
  private def named(name: Query.Name): (scala.List[Api.Entity], Boolean) =
    if name.exact then (api.byName.get(name.text).map(_.stdlib).getOrElse(scala.Nil), true)
    else
      def search(radius: Int): (scala.List[Api.Entity], Boolean) =
        if radius > 2 then (scala.Nil, false)
        else
          val found: scala.List[Api.Entity] =
            lexicon.search(name.text, radius).stdlib.toList.flatMap(_.stdlib)

          if found.isEmpty then search(radius + 1) else (found, radius == 0)

      search(0)

  // The keys a written type name may mean: a universal type's, a qualified key as written (its
  // `#`s written as `.`), or every type entity of the simple name, aliases among them.
  private def keysOf(name: Text): scala.List[Text] =
    universal.get(name) match
      case Some(key) => scala.List(key)

      case None =>
        def leaf(key: Text): String = key.s.substring(key.s.lastIndexOf('.') + 1).nn
        val simple: Text = leaf(name).tt
        val dotted: Boolean = name.contains('.')

        val entities: scala.List[Text] =
          api.byName.get(simple).map(_.stdlib).getOrElse(scala.Nil)
          . filter(isType)
          . map(_.key)
          . filter: key => !dotted || key.sub(t"#", t".") == name

        val java: scala.List[Text] =
          (Lattice.javaTypes.keys ++ Lattice.javaTypes.values.flatten).toList.distinct
          . filter: key => key == name || (!dotted && leaf(key) == simple.s)

        val primitive: scala.List[Text] =
          Lattice.primitives.toList.filter: key =>
            key == name || (!dotted && key == t"scala.$simple")

        (entities ++ java ++ primitive).distinct

  // Every shape a written type may mean, expanded: each key its name may be, with each
  // combination of what its arguments may be — a simple name is ambiguous across packages, and
  // so is each argument — to a bound which keeps a query's work small.
  private def shapesOf(query: Query.TypeQuery): scala.List[Api.Shape] =
    val keys: scala.List[Text] = keysOf(query.name)

    val combinations: scala.List[scala.List[Api.Shape]] =
      query.args.stdlib.foldLeft(scala.List(scala.List.empty[Api.Shape])): (acc, arg) =>
        val options: scala.List[Api.Shape] = shapesOf(arg) match
          case scala.Nil => scala.List(Api.Shape.Other)
          case shapes    => shapes

        acc.flatMap { prefix => options.map(prefix :+ _) }.take(Search.alternatives)

    val shapes: scala.List[Api.Shape] = keys.flatMap: key =>
      combinations.map: args => api.expand(Api.Shape.Named(key, List.from(args)))

    shapes.distinct

  private def resolve(query: Query.TypeQuery): scala.List[(Int, Api.Shape)] =
    shapesOf(query).flatMap: shape =>
      val head: Int = headId(shape)
      if head < 0 then scala.Nil else scala.List((head, shape))

  // Whether two expanded shapes agree, argument by argument, variance aside.
  private def same(left: Api.Shape, right: Api.Shape): Boolean = (left, right) match
    case (Api.Shape.Named(k1, as1), Api.Shape.Named(k2, as2)) =>
      k1 == k2 && as1.stdlib.size == as2.stdlib.size &&
        as1.stdlib.zip(as2.stdlib).forall(same(_, _))

    case _ => false

  // Whether a hit's type satisfies a query's, with arguments: the same head with the same
  // arguments, or a strict subtype whose own direct parents say it is the query's type.
  private def satisfies(hit: Api.Shape, query: Api.Shape, queryHead: Int): Boolean = query match
    case Api.Shape.Named(_, Nil) => true

    case Api.Shape.Named(queryKey, _) => hit match
      case Api.Shape.Named(hitKey, _) if hitKey == queryKey => same(hit, query)

      case Api.Shape.Named(hitKey, _) =>
        val parents: scala.List[Api.Shape] =
          api(hitKey).let(_.template).let(_.parentShapes).or(Nil).stdlib.map(api.expand(_))

        parents.exists:
          case parent @ Api.Shape.Named(key, _) => key == queryKey && same(parent, query)
          case _                                => false

      case _ => false

    case _ => false

  // Every simple name, sorted, for completion by prefix; and every name a type has.
  private lazy val names: scala.List[Text] = api.byName.keys.toList.sortBy(_.s)

  private lazy val typeNames: sci.Set[Text] =
    val named: sci.Set[Text] = api.byName.filter { (_, entities) => entities.exists(isType) }.keySet
    named ++ universal.keySet ++ Lattice.primitives.map: key => Text(key.s.substring(6).nn)

  // What may complete the word before the caret: after `+` or `-`, or within the type there, a
  // type's name; after `/`, a package's or an object's; elsewhere a name or a keyword.
  def completions(text: Text, caret: Int): List[Control.Field.Completion] =
    val s: String = text.s
    val at: Int = caret.min(s.length).max(0)
    var start: Int = at
    def wordChar(char: Char): Boolean = char.isLetterOrDigit || char == '_'
    while start > 0 && wordChar(s.charAt(start - 1)) do start -= 1
    val stem: String = s.substring(start, at).nn

    if stem.isEmpty then Nil else
      // The sigil which governs the word: the nearest `+`, `-` or `/` before it, unless a space
      // at depth zero has ended its term.
      def governing(index: Int, depth: Int): Optional[Char] =
        if index < 0 then Unset else s.charAt(index) match
          case ']' | ')' | '}'         => governing(index - 1, depth + 1)
          case '[' | '(' | '{'         => governing(index - 1, (depth - 1).max(0))
          case c if c.isWhitespace     => if depth > 0 then governing(index - 1, depth) else Unset
          case '+' | '-' | '/'         => s.charAt(index)
          case _                       => governing(index - 1, depth)

      val sigil: Optional[Char] = governing(start - 1, 0)
      def prefixed(name: Text): Boolean = name.s.regionMatches(true, 0, stem, 0, stem.length)

      def ranked(found: scala.List[Text]): scala.List[Text] =
        found.sortBy { name => (!name.s.startsWith(stem), name.s) }.take(Search.completed)

      val found: scala.List[Control.Field.Completion] = sigil match
        case '+' | '-' =>
          ranked(typeNames.toList.filter(prefixed)).map: name =>
            val entity: Option[Api.Entity] =
              api.byName.get(name).flatMap(_.stdlib.find(_.template.present))

            val key: Text = entity.map(_.key).getOrElse(universal.getOrElse(name, t"scala.$name"))
            Control.Field.Completion(name, t"type", key)

        case '/' =>
          def holds(name: Text): Boolean = api.byName.get(name).exists(_.stdlib.exists(container))

          ranked(names.filter { name => prefixed(name) && holds(name) }).map: name =>
            val key: Text =
              api.byName.get(name).flatMap(_.stdlib.find(container)).map(_.key).getOrElse(name)

            Control.Field.Completion(name, t"package", key)

        case _ =>
          val keywords: scala.List[Control.Field.Completion] =
            Query.Keyword.all.keys.toList.filter(prefixed).sortBy(_.s).map: word =>
              Control.Field.Completion(word, t"keyword", t"")

          val entities: scala.List[Control.Field.Completion] =
            ranked(names.filter(prefixed)).map: name =>
              val first: Option[Api.Entity] = api.byName.get(name).flatMap(_.stdlib.headOption)

              val kind: Text =
                first.flatMap(_.declarations.stdlib.headOption).map(_.kind.keyword).getOrElse(t"")

              Control.Field.Completion(name, kind, first.map(_.key).getOrElse(t""))

          keywords ++ entities

      List.from(found)

  def apply(query: Query): Outcome =
    val notices: scm.ListBuffer[Text] = scm.ListBuffer.from(query.warnings.stdlib)

    // The entries which satisfy every criterion met so far; `None` before the first.
    var survivors: Option[java.util.BitSet] = None
    val exact: java.util.BitSet = java.util.BitSet()

    def narrow(found: java.util.BitSet): Unit = survivors match
      case Some(set) => set.and(found)
      case None      => survivors = Some(found)

    query.names.stdlib.foreach: name =>
      val (found, isExact) = named(name)
      val set: java.util.BitSet = java.util.BitSet()
      found.foreach: entity => byEntity.get(entity.key).foreach(_.foreach(set.set(_)))
      if isExact then exact.or(set)
      narrow(set)

    query.results.stdlib.foreach: typeQuery =>
      val resolved: scala.List[(Int, Api.Shape)] = resolve(typeQuery)
      if resolved.isEmpty then notices += t"There is no type called `${typeQuery.name}`."
      val set: java.util.BitSet = java.util.BitSet()

      resolved.foreach: (head, shape) =>
        lattice.beneath(head).foreach: sub =>
          byResult(sub).foreach: index =>
            if satisfies(entries(index).result.or(Api.Shape.Other), shape, head) then set.set(index)

      narrow(set)

    query.parameters.stdlib.foreach: typeQuery =>
      val resolved: scala.List[(Int, Api.Shape)] = resolve(typeQuery)
      if resolved.isEmpty then notices += t"There is no type called `${typeQuery.name}`."
      val set: java.util.BitSet = java.util.BitSet()

      resolved.foreach: (head, shape) =>
        lattice.above(head).foreach: sup =>
          byParam(sup).foreach: index =>
            val entry: Entry = entries(index)

            val fits: Boolean = shape match
              case Api.Shape.Named(_, Nil) => true

              case _ =>
                entry.params.indices.exists: i =>
                  entry.paramHeads(i) == sup && (sup != head || same(entry.params(i), shape))

            if fits then set.set(index)

      narrow(set)

    val candidates: java.util.BitSet = survivors.getOrElse:
      java.util.BitSet().tap(_.set(0, entries.length))

    def fits(entry: Entry): Boolean =
      val declaration: Api.Declaration = entry.declaration
      val key: Text = entry.entity.key.sub(t"#", t".")

      def kind: Boolean = query.kinds.isEmpty || query.kinds.contains(declaration.kind)

      def form: Boolean =
        query.form.lay(true): wanted => (wanted == Query.Form.Type) == declaration.kind.isType

      def modifiers: Boolean = query.modifiers.forall(declaration.modifiers.has(_))
      def within: Boolean = query.packages.stdlib.forall: pkg => key == pkg || key.starts(t"$pkg.")

      kind && form && modifiers && within

    val hits: scm.ListBuffer[Hit] = scm.ListBuffer()
    var index: Int = candidates.nextSetBit(0)

    while index >= 0 do
      val entry: Entry = entries(index)
      if fits(entry) then hits += Hit(entry.entity, entry.declaration, exact.get(index))
      index = candidates.nextSetBit(index + 1)

    // Exactly-named hits first, then the project's own before its dependencies', then by key.
    val ordered: scala.List[Hit] = hits.toList.sortBy: hit => (!hit.exact, !hit.entity.own)
    Outcome(List.from(ordered), List.from(notices))
