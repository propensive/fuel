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

import soundness.{Query as _, *}

import caseSensitivity.caseInsensitive
import proximities.levenshteinProximity
import pyrocosm.Control
import dysasymptotics.{linearAccess, linearSize}
import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.sortingAlgorithms.timsort

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
  // The declarations found, what could be said of the query, and the glossary's terms a name
  // of the query names.
  case class Outcome(hits: List[Hit], notices: List[Text], terms: List[Glossary.Term] = Nil)

  // The names of the universal types, which have no entity, and the keys they stand for.
  private val universal: Map[Text, Text] =
    Map
      ( t"Any" -> Lattice.any, t"Matchable" -> Lattice.matchable, t"AnyRef" -> Lattice.anyRef,
        t"Object" -> Lattice.obj, t"AnyVal" -> Lattice.anyVal, t"Nothing" -> Lattice.nothing,
        t"Null" -> Lattice.nul )

  private case class Entry
    ( entity:      Api.Entity,
      declaration: Api.Declaration,
      result:      Optional[Api.Shape],   // expanded
      resultHead:  Int,                   // -1 for none
      params:      List[Api.Shape],
      paramHeads:  List[Int] )

  // The most meanings a written type's arguments are tried in, and the most completions offered.
  private val alternatives: Int = 64
  private val completed: Int = 20

  def build(api: Api): Search = new Search(api, api.lattice)

  // The entries a criterion admits, by their indices. Soundness has no growable bit set
  // (soundness#2499), so they are marked in the JDK's, which this alone touches.
  private final class Marks():
    private val bits: java.util.BitSet = java.util.BitSet()

    def mark(index: Int): Unit = bits.set(index)
    def marked(index: Int): Boolean = bits.get(index)
    def fill(size: Int): Unit = bits.set(0, size)
    def narrow(other: Marks): Unit = bits.and(other.bits)
    def widen(other: Marks): Unit = bits.or(other.bits)

    // Every index marked, in order.
    def indices: List[Int] =
      def recur(index: Int, found: List[Int]): List[Int] =
        if index < 0 then found.reverse else recur(bits.nextSetBit(index + 1), index :: found)

      recur(bits.nextSetBit(0), Nil)

// The entries are read with `readUnchecked`, by an index below their count, which is where every
// posting comes from.
final class Search private (api: Api, lattice: Lattice):
  import Search.*

  private def headId(shape: Api.Shape): Int = Lattice.head(shape).let(lattice.id(_)).or(-1)

  // Whether an entity is a type, or declares one; whether it is a package or an object.
  private def isType(e: Api.Entity): Boolean =
    e.template.present || e.declarations.exists(_.kind.isType)

  private def container(e: Api.Entity): Boolean =
    e.isPackage || e.definition.let(_.kind == Api.Kind.Object).or(false)

  // The entities a name has, by the API's index of names.
  private def ofName(name: Text): List[Api.Entity] = api.byName.at(name).or(Nil)

  private val entries: Array[Entry] =
    val sorted: List[Api.Entity] =
      api.entities.values.filter { e => !e.hidden && !e.isParameter }.order(_.key)

    sorted.bind: entity =>
      entity.declarations.map: declaration =>
        val provided: Optional[Api.Shape] =
          if declaration.kind == Api.Kind.Given then declaration.provides else Unset

        val returns: Optional[Api.Shape] = declaration.returns.or(provided)
        val result: Optional[Api.Shape] = returns.let(api.expand(_))
        val params: List[Api.Shape] = declaration.accepts.map(api.expand(_))
        Entry(entity, declaration, result, result.let(headId(_)).or(-1), params, params.map(headId))

    . to[Array]

  private def entry(index: Int): Entry = entries.readUnchecked(index)

  // The indices of the entries, by each head they have.
  private def postings(heads: Entry => List[Int]): Map[Int, Array[Int]] =
    val pairs: List[(Int, Int)] = List.range(0, entries.length).bind: index =>
      heads(entry(index)).distinct.filter(_ >= 0).map((_, index))

    pairs.group(_(0)).map(_.map(_(1)).to[Array])

  private val byResult: Map[Int, Array[Int]] = postings: entry =>
    if entry.resultHead < 0 then Nil else List(entry.resultHead)

  private val byParam: Map[Int, Array[Int]] = postings(_.paramHeads)

  private val byEntity: Map[Text, List[Int]] =
    List.range(0, entries.length).group(entry(_).entity.key)

  private def posted(postings: Map[Int, Array[Int]], head: Int): List[Int] =
    postings.at(head).lay(Nil)(_.to[List])

  // The entities a name finds exactly: those of the name, and those with it as a keyword.
  private def exactly(name: Text): List[Api.Entity] =
    (ofName(name) + api.byKeyword.at(name.lower).or(Nil)).distinct

  // Built with the index, so that the daemon's warm-up pays for it rather than the first
  // keystroke which searches by name. A keyword is in it as a name is.
  private val lexicon: Lexicon[List[Api.Entity]] =
    Lexicon[List[Api.Entity]]().tap: lexicon =>
      api.byName.foreach: (name, entities) => lexicon(name) = entities

      api.byKeyword.foreach: (keyword, entities) =>
        lexicon(keyword) = ofName(keyword) + entities

  // The entities a name finds: exactly, or at the smallest radius which finds any.
  private def named(name: Query.Name): (List[Api.Entity], Boolean) =
    if name.exact then (exactly(name.text), true)
    else
      def search(radius: Int): (List[Api.Entity], Boolean) =
        if radius > 2 then (List.empty[Api.Entity], false)
        else
          val found: List[Api.Entity] = lexicon.search(name.text, radius).to[List].flat
          if found.nil then search(radius + 1) else (found, radius == 0)

      search(0)

  // The last segment of a dotted name.
  private def leaf(key: Text): Text = key.cut(t".").last

  // The keys a written type name may mean: a universal type's, a qualified key as written (its
  // `#`s written as `.`), or every type entity of the simple name, aliases among them.
  private def keysOf(name: Text): List[Text] = universal.at(name).lay(typesNamed(name))(List(_))

  // The keys a written type name which is no universal type's may mean.
  private def typesNamed(name: Text): List[Text] =
    val simple: Text = leaf(name)
    val dotted: Boolean = name.contains('.')

    val entities: List[Text] =
      ofName(simple).filter(isType).map(_.key).filter: key =>
        !dotted || key.sub(t"#", t".") == name

    val java: List[Text] =
      Lattice.javaKeys.filter: key => key == name || (!dotted && leaf(key) == simple)

    val primitive: List[Text] =
      Lattice.primitives.filter: key =>
        key == name || (!dotted && key == t"scala.$simple")

    (entities + java + primitive).distinct

  // Every shape a written type may mean, expanded: each key its name may be, with each
  // combination of what its arguments may be — a simple name is ambiguous across packages, and
  // so is each argument — to a bound which keeps a query's work small.
  private def shapesOf(query: Query.TypeQuery): List[Api.Shape] =
    val keys: List[Text] = keysOf(query.name)

    val combinations: List[List[Api.Shape]] =
      query.args.fold[List[List[Api.Shape]]](List(Nil)): (combinations, arg) =>
        val found: List[Api.Shape] = shapesOf(arg)
        val options: List[Api.Shape] = if found.nil then List(Api.Shape.Other) else found

        combinations.bind { prefix => options.map { option => prefix + List(option) } }
        . keep(Search.alternatives)

    val shapes: List[Api.Shape] = keys.bind: key =>
      combinations.map: args => api.expand(Api.Shape.Named(key, args))

    shapes.distinct

  private def resolve(query: Query.TypeQuery): List[(Int, Api.Shape)] =
    shapesOf(query).bind: shape =>
      val head: Int = headId(shape)
      if head < 0 then Nil else List((head, shape))

  // Whether two expanded shapes agree, argument by argument, variance aside.
  private def same(left: Api.Shape, right: Api.Shape): Boolean = (left, right) match
    case (Api.Shape.Named(k1, as1), Api.Shape.Named(k2, as2)) =>
      k1 == k2 && as1.size == as2.size && as1.zip(as2).all(same(_, _))

    case _ => false

  // Whether a hit's type satisfies a query's, with arguments: the same head with the same
  // arguments, or a strict subtype whose own direct parents say it is the query's type.
  private def satisfies(hit: Api.Shape, query: Api.Shape, queryHead: Int): Boolean = query match
    case Api.Shape.Named(_, Nil) => true

    case Api.Shape.Named(queryKey, _) => hit match
      case Api.Shape.Named(hitKey, _) if hitKey == queryKey => same(hit, query)

      case Api.Shape.Named(hitKey, _) =>
        val parents: List[Api.Shape] =
          api(hitKey).let(_.template).let(_.parentShapes).or(Nil).map(api.expand(_))

        parents.exists:
          case parent@Api.Shape.Named(key, _) => key == queryKey && same(parent, query)
          case _                              => false

      case _ => false

    case _ => false

  // Every simple name, sorted, for completion by prefix; and every name a type has.
  private val names: List[Text] = api.byName.keys.to[List].sort

  private val typeNames: Set[Text] =
    val named: List[Text] = api.byName.keys.to[List].filter(ofName(_).exists(isType))
    (named + universal.keys.to[List] + Lattice.primitives.map(_.skip(6))).to[Set]

  // What may complete the word before the caret: after `+` or `-`, or within the type there, a
  // type's name; after `/`, a package's or an object's; elsewhere a name or a keyword.
  def completions(text: Text, caret: Int): List[Control.Field.Completion] =
    val at: Int = caret.min(text.length).max(0)
    def char(index: Int): Optional[Char] = text.at(index.z)

    def wordly(index: Int): Boolean =
      char(index).let { (char: Char) => char.isLetterOrDigit || char == '_' }.or(false)

    def begin(index: Int): Int = if index > 0 && wordly(index - 1) then begin(index - 1) else index
    val start: Int = begin(at)
    val stem: Text = text.segment(start.z till at.z)

    if stem == t"" then Nil else
      // The sigil which governs the word: the nearest `+`, `-` or `/` before it, unless a space
      // at depth zero has ended its term.
      def governing(index: Int, depth: Int): Optional[Char] = char(index).let:
        case ']' | ')' | '}'     => governing(index - 1, depth + 1)
        case '[' | '(' | '{'     => governing(index - 1, (depth - 1).max(0))
        case c if c.isWhitespace => if depth > 0 then governing(index - 1, depth) else Unset
        case '+' | '-' | '/'     => char(index)
        case _                   => governing(index - 1, depth)

      val sigil: Optional[Char] = governing(start - 1, 0)
      val lowerStem: Text = stem.lower
      def prefixed(name: Text): Boolean = name.lower.starts(lowerStem)

      // Those which begin with the stem as it is written first, each part in order.
      def ranked(found: List[Text]): List[Text] =
        val sorted: List[Text] = found.sort
        (sorted.filter(_.starts(stem)) + sorted.filter(!_.starts(stem))).keep(Search.completed)

      sigil match
        case '+' | '-' =>
          ranked(typeNames.to[List].filter(prefixed)).map: name =>
            val entity: Optional[Api.Entity] = ofName(name).seek(_.template.present)
            val key: Text = entity.let(_.key).or(universal.at(name).or(t"scala.$name"))
            Control.Field.Completion(name, t"type", key)

        case '/' =>
          def holds(name: Text): Boolean = ofName(name).exists(container)

          ranked(names.filter { name => prefixed(name) && holds(name) }).map: name =>
            val key: Text = ofName(name).seek(container).let(_.key).or(name)
            Control.Field.Completion(name, t"package", key)

        case _ =>
          val keywords: List[Control.Field.Completion] =
            Query.Keyword.all.keys.to[List].filter(prefixed).sort.map: word =>
              Control.Field.Completion(word, t"keyword", t"")

          val entities: List[Control.Field.Completion] =
            ranked(names.filter(prefixed)).map: name =>
              val first: Optional[Api.Entity] = ofName(name).head
              val kind: Text = first.let(_.declarations.head).let(_.kind.keyword).or(t"")
              Control.Field.Completion(name, kind, first.let(_.key).or(t""))

          keywords + entities

  def apply(query: Query, glossary: Glossary = Glossary.none): Outcome =
    // The glossary's terms any name of the query names, once each.
    val terms: List[Glossary.Term] =
      query.names.bind { name => glossary.search(name.text) }.distinct

    // The entries each name admits, and which of them it names exactly.
    val byName: List[(Marks, Boolean)] = query.names.map: name =>
      val (found, isExact) = named(name)
      val marks: Marks = Marks()
      found.each { entity => byEntity.at(entity.key).let(_.each(marks.mark(_))) }
      (marks, isExact)

    val exact: Marks = Marks()
    byName.each { (marks, isExact) => if isExact then exact.widen(marks) }

    val results: List[(Marks, List[(Int, Api.Shape)])] = query.results.map: typeQuery =>
      val resolved: List[(Int, Api.Shape)] = resolve(typeQuery)
      val marks: Marks = Marks()

      resolved.each: (head, shape) =>
        lattice.beneath(head).each: sub =>
          posted(byResult, sub).each: index =>
            if satisfies(entry(index).result.or(Api.Shape.Other), shape, head)
            then marks.mark(index)

      (marks, resolved)

    val parameters: List[(Marks, List[(Int, Api.Shape)])] = query.parameters.map: typeQuery =>
      val resolved: List[(Int, Api.Shape)] = resolve(typeQuery)
      val marks: Marks = Marks()

      resolved.each: (head, shape) =>
        lattice.above(head).each: sup =>
          posted(byParam, sup).each: index =>
            val entry: Entry = this.entry(index)

            val fits: Boolean = shape match
              case Api.Shape.Named(_, Nil) => true

              case _ =>
                entry.paramHeads.zip(entry.params).exists: (param, shape0) =>
                  param == sup && (sup != head || same(shape0, shape))

            if fits then marks.mark(index)

      (marks, resolved)

    val unknown: List[Text] =
      (query.results.zip(results) + query.parameters.zip(parameters)).bind:
        case (typeQuery, (_, Nil)) => List(t"There is no type called `${typeQuery.name}`.")
        case _                     => Nil

    val notices: List[Text] = query.warnings + unknown

    // The entries which satisfy every criterion, or every entry where there is none.
    val criteria: List[Marks] = byName.map(_(0)) + results.map(_(0)) + parameters.map(_(0))

    val candidates: Marks = criteria.head.or(Marks().tap(_.fill(entries.length)))
    criteria.skip(1).each(candidates.narrow(_))

    def fits(entry: Entry): Boolean =
      val declaration: Api.Declaration = entry.declaration
      val key: Text = entry.entity.key.sub(t"#", t".")

      def kind: Boolean = query.kinds.nil || query.kinds.has(declaration.kind)

      def form: Boolean =
        query.form.lay(true): wanted => (wanted == Query.Form.Type) == declaration.kind.isType

      def modifiers: Boolean = query.modifiers.all(declaration.modifiers.has(_))
      def within: Boolean = query.packages.all: pkg => key == pkg || key.starts(t"$pkg.")

      kind && form && modifiers && within

    val hits: List[Hit] = candidates.indices.bind: index =>
      val entry: Entry = this.entry(index)
      if !fits(entry) then Nil else List(Hit(entry.entity, entry.declaration, exact.marked(index)))

    // Exactly-named hits first, then the project's own before its dependencies', then by key.
    val ordered: List[Hit] = hits.order: hit =>
      (if hit.exact then 0 else 2) + (if hit.entity.own then 0 else 1)

    Outcome(ordered, notices, terms)
