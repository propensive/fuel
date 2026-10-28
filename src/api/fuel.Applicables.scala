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

import soundness.{Language as _, Token as _, *}

import stenography.Designator

// What applies to a type beyond its own members, found in one pass over the whole model once
// the project and its dependencies are merged (an extension in one jar extends a type in
// another): every extension method whose receiver the type satisfies, and every given of or
// for the type. An extension reaches a type one of these ways, each a *tier*, the lower the
// closer, which is the order a page lists them in:
//
//   0  its receiver is the type, declared in the type or its companion (in implicit scope,
//      so no import is needed), or in a base's;
//   1  its receiver is the type, declared in the type's own package;
//   2  its receiver is the type, declared anywhere else;
//   3  its receiver is a base of the type, nearer bases first;
//   4  its receiver is a type parameter bounded by the type or a base;
//   5  its receiver is a type parameter with a context bound (or a `using` clause) which a
//      given *for this type* satisfies — the given which does is `via`;
//   6  as 5, but satisfied only by a given for every type, so it extends any value;
//   7  its receiver is unbounded (`Any`, or a parameter with no bound and no requirement), so
//      it extends any value.
//
// Tiers 6 and 7 are listed once, on the page of `scala.Any`, which is made if the model has
// none; every other page which lists extensions points there.
//
// A requirement — a `using` parameter's type, which a context bound is — must be satisfiable
// for the extension to be listed at all: some given of its type class, as far as the model
// knows, must unify with it (a given's own requirements are checked in turn, to a depth of 3).
// A type class with *no* given anywhere in the model is one the model cannot see provided
// (one the compiler synthesises, as `ClassTag`, or one a context function provides), so a
// requirement of it is taken as satisfied.
object Applicables:
  import Api.Shape

  val any: Text = t"scala.Any"

  // The types every value has: a receiver of one extends everything.
  val universal: sci.Set[Text] = sci.Set(any, t"scala.AnyRef", t"java.lang.Object", t"scala.Matchable", t"scala.AnyVal")

  private val depth: Int = 3

  // A type parameter's name to the key of the type it stands for, as unification binds it.
  private type Binding = sci.Map[Text, Text]

  // A given: its entity and the declaration which provides.
  private case class Provider(entity: Api.Entity, declaration: Api.Declaration):
    def key: Text = entity.key
    def provides: Shape = declaration.provides.or(Shape.Other)

  // An extension method: its entity, the index of the declaration among the entity's, and the
  // declaration; `within` the given it is declared inside, where it is.
  private case class Candidate(entity: Api.Entity, index: Int, declaration: Api.Declaration, within: Optional[Provider]):
    def key: Text = entity.key
    def importFrom: Optional[Text] = within.lay(entity.parent.let(Api.key(_))) { (w: Provider) => w.entity.parent.let(Api.key(_)) }

  private def keyOf(designator: Optional[Designator]): Optional[Text] = designator.let(Api.key(_))

  def decorate(api: Api): Api =
    val entities: scala.List[Api.Entity] = api.entities.values.toList

    // A shape with every alias it names expanded to what the alias stands for, its parameters
    // substituted, to a depth which ends a cycle.
    def expand(shape: Shape, depth: Int = 8): Shape = shape match
      case Shape.Named(key, args) =>
        val expanded: List[Shape] = args.map(expand(_, depth))
        val entity: Optional[Api.Entity] = api(key)
        val alias: Optional[Api.Declaration] = entity.let(_.template).let { (d: Api.Declaration) => if d.aliasOf.present then d else Unset }

        // An export forwarder's page is its target's, so its name is the target's.
        val exported: Optional[Text] = entity.let(_.alias).let(Api.key(_))

        if alias.absent && exported.present && depth > 0 then expand(Shape.Named(exported.or(key), expanded), depth - 1)
        else alias.lay(Shape.Named(key, expanded)): (d: Api.Declaration) =>
          if depth <= 0 then Shape.Other else
            val binding: sci.Map[Text, Shape] = sci.Map.from(d.aliasParams.stdlib.zip(expanded.stdlib))

            def over(shape: Shape): Shape = shape match
              case Shape.Param(name, _)   => binding.getOrElse(name, shape)
              case Shape.Named(k, as)     => Shape.Named(k, as.map(over))
              case other                  => other

            expand(over(d.aliasOf.or(Shape.Other)), depth - 1)

      case other => other

    def expanded(declaration: Api.Declaration): Api.Declaration =
      declaration.copy(receiver = declaration.receiver.let(expand(_)), requires = declaration.requires.map(expand(_)), provides = declaration.provides.let(expand(_)))

    // Every given, by the key of the type class it provides.
    val providers: scala.List[Provider] = entities.flatMap: entity =>
      entity.declarations.stdlib.filter { d => d.kind == Api.Kind.Given && d.provides.present && !d.exported }.map { d => Provider(entity, expanded(d)) }

    val byHead: sci.Map[Text, scala.List[Provider]] = providers.groupBy(_.provides.head.or(t""))

    def providerOf(key: Optional[Text]): Optional[Provider] =
      key.let(api(_)).let { (e: Api.Entity) => e.definition.let { (d: Api.Declaration) => if d.kind == Api.Kind.Given then Provider(e, d) else Unset } }

    val candidates: scala.List[Candidate] = entities.flatMap: entity =>
      val within: Optional[Provider] = providerOf(keyOf(entity.parent))

      entity.declarations.stdlib.zipWithIndex.collect:
        case (d, index) if d.kind == Api.Kind.Extension && d.receiver.present && !d.exported => Candidate(entity, index, expanded(d), within)

    // Every type's subtypes, each with its distance (the base's place in the subtype's bases).
    val subtypes: sci.Map[Text, scala.List[(Text, Int)]] =
      entities.flatMap { e => e.bases.stdlib.zipWithIndex.map { (base, i) => (base, (e.key, i + 1)) } }.groupMap(_(0))(_(1))

    // How far an entity declared beneath `from` is from `target`: 0 in the target or its
    // companion or a base's, 1 in its own package or enclosing object, 2 anywhere else.
    def reach(from: Optional[Text], target: Api.Entity): Int = from.lay(2): (from: Text) =>
      if from == target.key || target.bases.has(from) then 0
      else if keyOf(target.parent) == from then 1
      else 2

    def substitute(shape: Shape, binding: Binding): Shape = shape match
      case Shape.Param(name, _) => binding.get(name) match
        case Some(key) => Shape.Named(key)
        case None      => shape

      case Shape.Named(key, args) => Shape.Named(key, args.map(substitute(_, binding)))
      case other                  => other

    // Whether a type is another, or extends it.
    def beneathKey(key: Text, base: Text): Boolean =
      key == base || api(key).let(_.bases.has(base)).or(false)

    // A requirement against what a given provides: a parameter of the requirement is bound to
    // the type the given names there (and must be bound alike wherever it recurs), or to the
    // bound of the given's parameter there, where it has one; an unbounded parameter of the
    // given stands for anything, a bounded one for its bound and what extends it; `Other`
    // matches anything.
    def unify(req: Shape, prov: Shape, binding: Binding): Option[Binding] = (req, prov) match
      case (Shape.Other, _) | (_, Shape.Other) | (_, Shape.Param(_, Unset)) => Some(binding)

      case (Shape.Param(name, _), Shape.Named(key, _)) => binding.get(name) match
        case Some(bound) => if bound == key then Some(binding) else None
        case None        => Some(binding.updated(name, key))

      case (Shape.Param(name, _), Shape.Param(_, upper: Shape)) => upper.head match
        case Unset     => Some(binding)

        case key: Text => binding.get(name) match
          case Some(bound) => if beneathKey(bound, key) then Some(binding) else None
          case None        => Some(binding.updated(name, key))

      case (Shape.Named(k1, _), Shape.Param(_, upper: Shape)) =>
        upper.head.lay(Some(binding)) { (key: Text) => if beneathKey(k1, key) then Some(binding) else None }

      case (Shape.Named(k1, a1), Shape.Named(k2, a2)) =>
        if k1 != k2 then None
        else a1.stdlib.zip(a2.stdlib).foldLeft(Option(binding)) { (acc, pair) => acc.flatMap(unify(pair(0), pair(1), _)) }

    // The parameters of a requirement which a given's own parameters stand for, paired.
    def paired(req: Shape, prov: Shape): scala.List[(Text, Text)] = (req, prov) match
      case (Shape.Param(name, _), Shape.Param(other, _)) => scala.List((name, other))
      case (Shape.Named(_, a1), Shape.Named(_, a2))      => a1.stdlib.zip(a2.stdlib).flatMap(paired)
      case _                                             => scala.Nil

    // The givens which satisfy a requirement under a binding, each with the binding extended:
    // none at all when it cannot be satisfied; an empty list when nothing need satisfy it (a
    // type class the model has no given of, or a shape which is no type class). The answer
    // depends only on the requirement as bound, so it is kept by that.
    val answers: scala.collection.mutable.HashMap[(Shape, Int), Option[scala.List[(Provider, Binding)]]] = scala.collection.mutable.HashMap()

    def satisfy(req: Shape, binding: Binding, depth: Int): Option[scala.List[(Provider, Binding)]] =
      val bound: Shape = substitute(req, binding)

      // Not `getOrElseUpdate`: the answer recurs into `satisfy`, which must not update the map
      // beneath itself.
      val answer: Option[scala.List[(Provider, Binding)]] = answers.get((bound, depth)).getOrElse(bound match
        case Shape.Named(head, _) if byHead.contains(head) =>
          val found: scala.List[(Provider, Binding)] = byHead(head).flatMap: provider =>
            unify(bound, provider.provides, sci.Map()) match
              case None => scala.Nil
              case Some(extended) =>
                // The given's own requirements, with its parameters bound by what it provides
                // (the requirement's shape, once bound, binds them). Where a parameter of the
                // requirement is left standing for a parameter of the given, what the given's
                // own requirements bind that to (`[value: Textualizable] => value is Showable`
                // is a `Showable` for each `Textualizable`) binds it too, one answer each.
                val own: Option[Binding] = unify(provider.provides, substitute(bound, extended), sci.Map())

                own match
                  case None => scala.Nil

                  case Some(ownBinding) =>
                    val pairs: scala.List[(Text, Text)] =
                      paired(bound, provider.provides).filter { (r, _) => !extended.contains(r) }

                    // Every way the given's requirements can be met, as bindings of its parameters.
                    val ways: Option[scala.List[Binding]] =
                      if depth <= 0 then Some(scala.List(ownBinding))
                      else provider.declaration.requires.stdlib.foldLeft(Option(scala.List(ownBinding))): (acc, r) =>
                        acc.flatMap: bindings =>
                          val next: scala.List[scala.List[Binding]] = bindings.map { b => satisfy(r, b, depth - 1).map { found => if found.isEmpty then scala.List(b) else found.map(_(1)).distinct }.getOrElse(scala.Nil) }
                          val flat: scala.List[Binding] = next.flatten
                          if flat.isEmpty then None else Some(flat)

                    ways.getOrElse(scala.Nil).map: way =>
                      val carried: Binding = pairs.foldLeft(extended) { (acc, pair) => way.get(pair(1)).map { k => acc.updated(pair(0), k) }.getOrElse(acc) }
                      (provider, carried)
                    . distinct

          if found.isEmpty then None else Some(found)

        case _ => Some(scala.Nil))

      answers.update((bound, depth), answer)
      answer.map(_.map { (provider, extended) => (provider, binding ++ extended) })

    def mentions(shape: Shape, name: Text): Boolean = shape match
      case Shape.Param(n, _)      => n == name
      case Shape.Named(_, args)   => args.exists(mentions(_, name))
      case Shape.Other            => false

    // Every requirement satisfiable under the binding: the givens which satisfy each, in order.
    def allSatisfied(requires: List[Shape], binding: Binding): Option[scala.List[scala.List[Provider]]] =
      requires.stdlib.foldLeft(Option(scala.List[scala.List[Provider]]())): (acc, r) =>
        acc.flatMap { found => satisfy(r, binding, depth).map { more => found :+ more.map(_(0)) } }

    val found: juc.ConcurrentHashMap[Text, scala.List[Api.Applicable]] = juc.ConcurrentHashMap()

    def add(target: Text, extension: Api.Applicable): Unit =
      found.merge(target, scala.List(extension), (a, b) => a.nn ++ b.nn)

    def isUnbounded(shape: Shape): Boolean = shape match
      case Shape.Named(key, _)   => universal.contains(key)
      case Shape.Param(_, upper) => upper.lay(true)(isUnbounded(_))
      case Shape.Other           => false

    candidates.foreach: candidate =>
      val declaration: Api.Declaration = candidate.declaration
      val receiver: Shape = declaration.receiver.or(Shape.Other)
      val enclosing: Optional[Text] = candidate.within.let(_.key)

      def extension(target: Api.Entity, tier: Int, distance: Int, via: Optional[Text]): Api.Applicable =
        Api.Applicable(candidate.key, candidate.index, tier, distance, reach(candidate.importFrom, target), via, candidate.importFrom)

      // The type and every subtype beneath it, each with its distance, as entities.
      def beneath(key: Text): scala.List[(Api.Entity, Int)] =
        ((key, 0) :: subtypes.getOrElse(key, scala.Nil)).flatMap { (k, d) => api(k).let { (e: Api.Entity) => scala.List((e, d)) }.or(scala.Nil) }

      lazy val unconditional: Boolean = allSatisfied(declaration.requires, sci.Map()).isDefined

      receiver match
        case Shape.Named(key, _) if !universal.contains(key) =>
          if unconditional then beneath(key).foreach: (target, distance) =>
            val tier: Int = if distance == 0 then reach(candidate.importFrom, target) else 3
            add(target.key, extension(target, tier, distance, enclosing))

        case Shape.Param(name, upper: Shape.Named) if !isUnbounded(upper) =>
          beneath(upper.key).foreach: (target, distance) =>
            val binding: Binding = sci.Map(name -> target.key)
            allSatisfied(declaration.requires, binding).foreach: givens =>
              // The given which satisfies a requirement of the parameter, where one does.
              val via: Optional[Text] = enclosing.or:
                declaration.requires.stdlib.zip(givens).collectFirst { case (r, gs) if gs.nonEmpty && mentions(r, name) => gs.head.key }.getOrElse(Unset)

              add(target.key, extension(target, 4, distance, via))

        case Shape.Param(name, _) =>
          val (bounding, rest) = declaration.requires.stdlib.partition(mentions(_, name))

          bounding.headOption match
            case Some(first) =>
              // Each given of the first bounding requirement says which type the parameter is:
              // the type it is for, or any type.
              val narrowing: Option[scala.List[(Provider, Binding)]] = satisfy(first, sci.Map(), depth)

              narrowing.getOrElse(scala.Nil).foreach: (provider, binding) =>
                val others: Boolean = allSatisfied(List.from(bounding.tail ++ rest), binding).isDefined

                // The given's nearness to the type orders it: one in the companion first.
                if others then binding.get(name) match
                  case Some(key) => api(key).let { (target: Api.Entity) => add(key, extension(target, 5, reach(keyOf(provider.entity.parent), target), enclosing.or(provider.key))) }
                  case None      => add(any, Api.Applicable(candidate.key, candidate.index, 6, 0, 2, enclosing.or(provider.key), candidate.importFrom))

              // A requirement no given narrows (one the model sees no given of) leaves the
              // parameter unbounded.
              if narrowing.exists(_.isEmpty) && allSatisfied(List.from(rest), sci.Map()).isDefined then
                add(any, Api.Applicable(candidate.key, candidate.index, 7, 0, 2, enclosing, candidate.importFrom))

            case None =>
              if unconditional then add(any, Api.Applicable(candidate.key, candidate.index, 7, 0, 2, enclosing, candidate.importFrom))

        case Shape.Named(_, _) =>
          if unconditional then add(any, Api.Applicable(candidate.key, candidate.index, 7, 0, 2, enclosing, candidate.importFrom))

        case Shape.Other => ()

    // Every given, on the page of the type class it provides and on that of each type it
    // provides it for.
    val givens: juc.ConcurrentHashMap[Text, scala.List[Provider]] = juc.ConcurrentHashMap()

    providers.foreach: provider =>
      provider.provides match
        case Shape.Named(head, args) =>
          // A given for a bounded parameter (`[text <: Text] => text is Showable`) is for its bound.
          def named(shape: Shape): scala.List[Text] = shape match
            case Shape.Named(key, _)          => scala.List(key)
            case Shape.Param(_, upper: Shape) => named(upper)
            case _                            => scala.Nil

          val targets: scala.List[Text] = head :: args.stdlib.flatMap(named)
          targets.distinct.foreach { target => givens.merge(target, scala.List(provider), (a, b) => a.nn ++ b.nn) }

        case _ => ()

    // The entities with what applies to them, and `scala.Any` with its package where the model
    // has none and something extends every value.
    val withAny: sci.Map[Text, Api.Entity] =
      if api(any).present || !found.containsKey(any) then api.entities
      else
        val scalaKey: Text = t"scala"
        val base: sci.Map[Text, Api.Entity] =
          if api(scalaKey).present then api.entities
          else api.entities.updated(scalaKey, Api.Entity(Unset, scalaKey, false, Api.Declaration(Api.Kind.Package)))

        base.updated(any, Api.Entity(Designator.Top(scalaKey), t"Any", false, Unset, Api.Declaration(Api.Kind.Class, List(t"abstract"))))

    val decorated: sci.Map[Text, Api.Entity] = withAny.map: (key, entity) =>
      val extensions: List[Api.Applicable] =
        val all: scala.List[Api.Applicable] = Optional(found.get(key)).or(scala.Nil)
        // One satisfied by several givens is listed once, by the nearest.
        val sorted: scala.List[Api.Applicable] = all.sortBy { e => (e.tier, e.distance, e.reach, e.importFrom.or(t"").s, e.via.or(t"").s, e.key.s) }
        List.from(sorted.distinctBy { e => (e.key, e.index) })

      val known: List[Text] =
        val all: scala.List[Provider] = Optional(givens.get(key)).or(scala.Nil)
        List.from(all.distinctBy(_.key).sortBy { p => (reach(keyOf(p.entity.parent), entity), p.key.s) }.map(_.key))

      (key, entity.copy(extensions = extensions, givens = known))

    Api(decorated).relinked
