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

import soundness.collationComparable
import soundness.collations.codepointCollation
import soundness.sortingAlgorithms.timsort

import dysasymptotics.linearSize
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

  val any: Text = Lattice.any

  // The types every value has: a receiver of one extends everything.
  val universal: Set[Text] = Lattice.tops

  private val depth: Int = 3

  // A type parameter's name to the key of the type it stands for, as unification binds it.
  private type Binding = Map[Text, Text]

  // A given: its entity and the declaration which provides.
  private case class Provider(entity: Api.Entity, declaration: Api.Declaration):
    def key: Text = entity.key
    def provides: Shape = declaration.provides.or(Shape.Other)

  // What satisfies a requirement, as `satisfy` answers and keeps it: the givens which do, each
  // with its binding, or `Unset` where none can.
  private case class Answer(found: Optional[List[(Provider, Binding)]])

  // An extension method: its entity, the index of the declaration among the entity's, and the
  // declaration; `within` the given it is declared inside, where it is.
  private case class Candidate
    ( entity: Api.Entity, index: Int, declaration: Api.Declaration, within: Optional[Provider] ):

    def key: Text = entity.key

    def importFrom: Optional[Text] =
      within.lay(entity.parent.let(Api.key(_))): (w: Provider) => w.entity.parent.let(Api.key(_))

  private def keyOf(locator: Optional[Locator]): Optional[Text] = locator.let(Api.key(_))

  // The ids of the types beneath a type, as the lattice gives them.
  private def beneathIds(lattice: Lattice, id: Int): List[Int] = lattice.beneath(id)

  def decorate(api: Api): Api =
    val entities: List[Api.Entity] = api.entities.values
    val lattice: Lattice = api.lattice

    def expanded(declaration: Api.Declaration): Api.Declaration =
      declaration.copy
        ( receiver = declaration.receiver.let(api.expand(_)),
          requires = declaration.requires.map(api.expand(_)),
          provides = declaration.provides.let(api.expand(_)) )

    // Every given, by the key of the type class it provides.
    val providers: List[Provider] = entities.bind: (entity: Api.Entity) =>
      def providing(d: Api.Declaration): Boolean =
        d.kind == Api.Kind.Given && d.provides.present && !d.exported

      entity.declarations.filter(providing).map { (d: Api.Declaration) => Provider(entity, expanded(d)) }

    val byHead: Map[Text, List[Provider]] = providers.group(_.provides.head.or(t""))

    def providerOf(key: Optional[Text]): Optional[Provider] =
      key.let(api(_)).let: (e: Api.Entity) =>
        e.definition.let: (d: Api.Declaration) =>
          if d.kind == Api.Kind.Given then Provider(e, d) else Unset

    val candidates: List[Candidate] = entities.bind: (entity: Api.Entity) =>
      val within: Optional[Provider] = providerOf(keyOf(entity.parent))

      entity.declarations.indexed.bind: (d: Api.Declaration, index: Ordinal) =>
        if d.kind == Api.Kind.Extension && d.receiver.present && !d.exported
        then List(Candidate(entity, index.n0, expanded(d), within))
        else Nil

    // How far an entity declared beneath `from` is from `target`: 0 in the target or its
    // companion or a base's, 1 in its own package or enclosing object, 2 anywhere else.
    def reach(from: Optional[Text], target: Api.Entity): Int = from.lay(2): (from: Text) =>
      if from == target.key || target.bases.has(from) then 0
      else if keyOf(target.parent) == from then 1
      else 2

    def substitute(shape: Shape, binding: Binding): Shape = shape match
      case Shape.Param(name, _)   => binding.at(name).lay(shape) { (key: Text) => Shape.Named(key) }
      case Shape.Named(key, args) => Shape.Named(key, args.map(substitute(_, binding)))
      case other                  => other

    // Whether a type is another, or extends it, as the lattice says (a type the lattice has no
    // id for is beneath itself alone).
    def beneathKey(key: Text, base: Text): Boolean =
      val sub: Optional[Int] = lattice.id(key)
      val sup: Optional[Int] = lattice.id(base)
      key == base || sub.let { (sub: Int) => sup.let(lattice.isSubtype(sub, _)) }.or(false)

    // The type and every subtype beneath it, each with its distance, as entities.
    def beneath(key: Text): List[(Api.Entity, Int)] =
      lattice.id(key).lay(List[(Api.Entity, Int)]()): (sup: Int) =>
        beneathIds(lattice, sup).bind: (sub: Int) =>
          def distance: Int = lattice.distance(sub, sup)

          api(lattice.key(sub)).lay(List[(Api.Entity, Int)]()): (e: Api.Entity) =>
            List((e, distance))

    // A requirement against what a given provides: a parameter of the requirement is bound to
    // the type the given names there (and must be bound alike wherever it recurs), or to the
    // bound of the given's parameter there, where it has one; an unbounded parameter of the
    // given stands for anything, a bounded one for its bound and what extends it; `Other`
    // matches anything. `Unset` where they do not unify.
    def unify(req: Shape, prov: Shape, binding: Binding): Optional[Binding] = (req, prov) match
      case (Shape.Other, _) | (_, Shape.Other) => binding

      case (Shape.Param(name, _), Shape.Named(key, _)) =>
        binding.at(name).lay(binding.define(name, key)): (bound: Text) =>
          if bound == key then binding else Unset

      case (Shape.Param(name, _), Shape.Param(_, upper: Shape)) => upper.head match
        case key: Text =>
          binding.at(name).lay(binding.define(name, key)): (bound: Text) =>
            if beneathKey(bound, key) then binding else Unset

        case _ => binding

      case (Shape.Named(k1, _), Shape.Param(_, upper: Shape)) =>
        upper.head.lay(binding: Optional[Binding]): (key: Text) =>
          if beneathKey(k1, key) then binding else Unset

      case (Shape.Named(k1, a1), Shape.Named(k2, a2)) =>
        if k1 != k2 then Unset
        else
          a1.zip(a2).fold(binding: Optional[Binding]): (acc: Optional[Binding], pair) =>
            acc.let(unify(pair(0), pair(1), _))

      // A given's parameter without a bound stands for anything.
      case _ => binding

    // The parameters of a requirement which a given's own parameters stand for, paired.
    def paired(req: Shape, prov: Shape): List[(Text, Text)] = (req, prov) match
      case (Shape.Param(name, _), Shape.Param(other, _)) => List((name, other))
      case (Shape.Named(_, a1), Shape.Named(_, a2))      => a1.zip(a2).bind(paired(_, _))
      case _                                             => Nil

    // The givens which satisfy a requirement under a binding, each with the binding extended:
    // `Unset` when it cannot be satisfied; an empty list when nothing need satisfy it (a type
    // class the model has no given of, or a shape which is no type class). The answer depends
    // only on the requirement as bound, so it is kept by that.
    @scala.caps.unsafe.untrackedCaptures
    var answers: Map[(Shape, Int), Answer] = Map()

    def satisfy(req: Shape, binding: Binding, depth: Int): Optional[List[(Provider, Binding)]] =
      val bound: Shape = substitute(req, binding)

      def computed: Optional[List[(Provider, Binding)]] = bound match
        case Shape.Named(head, _) if byHead.at(head).present =>
          val found: List[(Provider, Binding)] = byHead.at(head).or(Nil).bind:
            (provider: Provider) =>
              unify(bound, provider.provides, Map()).lay(List[(Provider, Binding)]()):
                (extended: Binding) =>
                  // The given's own requirements, with its parameters bound by what it provides
                  // (the requirement's shape, once bound, binds them). Where a parameter of the
                  // requirement is left standing for a parameter of the given, what the given's
                  // own requirements bind that to (`[value: Textualizable] => value is
                  // Showable` is a `Showable` for each `Textualizable`) binds it too, one
                  // answer each.
                  val own: Optional[Binding] =
                    unify(provider.provides, substitute(bound, extended), Map())

                  own.lay(List[(Provider, Binding)]()): (ownBinding: Binding) =>
                    val pairs: List[(Text, Text)] =
                      paired(bound, provider.provides).filter: (r: Text, _) =>
                        extended.at(r).absent

                    // The bindings under which a requirement of the given is met, from one.
                    def meet(r: Shape, b: Binding): List[Binding] =
                      satisfy(r, b, depth - 1).lay(List[Binding]()):
                        (found: List[(Provider, Binding)]) =>
                          if found.nil then List(b) else found.map(_(1)).distinct

                    // Every way the given's requirements can be met, as bindings of its
                    // parameters.
                    val ways: Optional[List[Binding]] =
                      if depth <= 0 then List(ownBinding)
                      else
                        val start: Optional[List[Binding]] = List(ownBinding)

                        provider.declaration.requires.fold(start):
                          (acc: Optional[List[Binding]], r: Shape) =>
                            acc.let: (bindings: List[Binding]) =>
                              val flat: List[Binding] = bindings.bind(meet(r, _))
                              if flat.nil then Unset else flat

                    ways.or(Nil).map: (way: Binding) =>
                      val carried: Binding = pairs.fold(extended): (acc: Binding, pair) =>
                        way.at(pair(1)).lay(acc) { (k: Text) => acc.define(pair(0), k) }

                      (provider, carried)

                    . distinct

          if found.nil then Unset else found

        case _ => List()

      // Not computed in an update of the map: the answer recurs into `satisfy`, which must not
      // update the map beneath itself.
      val answer: Optional[List[(Provider, Binding)]] =
        answers.at((bound, depth)).lay(computed)(_.found)

      answers = answers.define((bound, depth), Answer(answer))

      answer.let: (found: List[(Provider, Binding)]) =>
        found.map { (provider, extended) => (provider, binding + extended) }

    def mentions(shape: Shape, name: Text): Boolean = shape match
      case Shape.Param(n, _)      => n == name
      case Shape.Named(_, args)   => args.exists(mentions(_, name))
      case Shape.Other            => false

    // Every requirement satisfiable under the binding: the givens which satisfy each, in order.
    def allSatisfied(requires: List[Shape], binding: Binding): Optional[List[List[Provider]]] =
      val start: Optional[List[List[Provider]]] = List()

      requires.fold(start): (acc: Optional[List[List[Provider]]], r: Shape) =>
        acc.let: (found: List[List[Provider]]) =>
          satisfy(r, binding, depth).let: (more: List[(Provider, Binding)]) =>
            found + List(more.map(_(0)))

    def isUnbounded(shape: Shape): Boolean = shape match
      case Shape.Named(key, _)   => universal.has(key)
      case Shape.Param(_, upper) => upper.lay(true)(isUnbounded(_))
      case Shape.Other           => false

    // Every extension each candidate is, by the key of the type it applies to, in the order
    // they are found.
    val applicable: List[(Text, Api.Applicable)] = candidates.bind: (candidate: Candidate) =>
      val declaration: Api.Declaration = candidate.declaration
      val receiver: Shape = declaration.receiver.or(Shape.Other)
      val enclosing: Optional[Text] = candidate.within.let(_.key)
      val from: Optional[Text] = candidate.importFrom

      def extension(target: Api.Entity, tier: Int, distance: Int, via: Optional[Text])
      :   Api.Applicable =

        val nearness: Int = reach(from, target)
        Api.Applicable(candidate.key, candidate.index, tier, distance, nearness, via, from)

      // An extension of every value.
      def everywhere(tier: Int, via: Optional[Text]): List[(Text, Api.Applicable)] =
        List((any, Api.Applicable(candidate.key, candidate.index, tier, 0, 2, via, from)))

      lazy val unconditional: Boolean = allSatisfied(declaration.requires, Map()).present

      receiver match
        case Shape.Named(key, _) if !universal.has(key) =>
          if !unconditional then Nil else beneath(key).map: (target, distance) =>
            val tier: Int = if distance == 0 then reach(candidate.importFrom, target) else 3
            (target.key, extension(target, tier, distance, enclosing))

        case Shape.Param(name, upper: Shape.Named) if !isUnbounded(upper) =>
          beneath(upper.key).bind: (target, distance) =>
            val binding: Binding = Map(name -> target.key)

            allSatisfied(declaration.requires, binding).lay(List[(Text, Api.Applicable)]()):
              (givens: List[List[Provider]]) =>
                // The given which satisfies a requirement of the parameter, where one does.
                val via: Optional[Text] = enclosing.or:
                  declaration.requires.zip(givens).reap:
                    case (r, gs) if !gs.nil && mentions(r, name) => gs.head.let(_.key)

                List((target.key, extension(target, 4, distance, via)))

        case Shape.Param(name, _) =>
          val bounding: List[Shape] = declaration.requires.filter(mentions(_, name))
          val rest: List[Shape] = declaration.requires.filter(!mentions(_, name))

          bounding.head.lay(if unconditional then everywhere(7, enclosing) else Nil):
            (first: Shape) =>
              // Each given of the first bounding requirement says which type the parameter
              // is: the type it is for, or any type.
              val narrowing: Optional[List[(Provider, Binding)]] = satisfy(first, Map(), depth)

              val narrowed: List[(Text, Api.Applicable)] = narrowing.or(Nil).bind:
                (provider: Provider, binding: Binding) =>
                  val others: Boolean = allSatisfied(bounding.skip(1) + rest, binding).present
                  val via: Optional[Text] = enclosing.or(provider.key)

                  // The given's nearness to the type orders it: one in the companion first.
                  if !others then Nil else binding.at(name).lay(everywhere(6, via)):
                    (key: Text) =>
                      api(key).lay(List[(Text, Api.Applicable)]()): (target: Api.Entity) =>
                        val nearness: Int = reach(keyOf(provider.entity.parent), target)
                        List((key, extension(target, 5, nearness, via)))

              // A requirement no given narrows (one the model sees no given of) leaves the
              // parameter unbounded.
              val unbounded: Boolean =
                narrowing.lay(false)(_.nil) && allSatisfied(rest, Map()).present

              narrowed + (if unbounded then everywhere(7, enclosing) else Nil)

        case Shape.Named(_, _) =>
          if unconditional then everywhere(7, enclosing) else Nil

        case Shape.Other =>
          Nil

    val found: Map[Text, List[(Text, Api.Applicable)]] = applicable.group(_(0))

    // Every given, on the page of the type class it provides and on that of each type it
    // provides it for.
    val givens: Map[Text, List[(Text, Provider)]] =
      val pairs: List[(Text, Provider)] = providers.bind: (provider: Provider) =>
        provider.provides match
          case Shape.Named(head, args) =>
            // A given for a bounded parameter (`[text <: Text] => text is Showable`) is for
            // its bound.
            def named(shape: Shape): List[Text] = shape match
              case Shape.Named(key, _)          => List(key)
              case Shape.Param(_, upper: Shape) => named(upper)
              case _                            => Nil

            (List(head) + args.bind(named)).distinct.map { (target: Text) => (target, provider) }

          case _ =>
            Nil

      pairs.group(_(0))

    // The entities with what applies to them, and `scala.Any` with its package where the model
    // has none and something extends every value.
    val withAny: Map[Text, Api.Entity] =
      if api(any).present || found.at(any).absent then api.entities
      else
        val scalaKey: Text = t"scala"
        val pkg: Api.Entity = Api.Entity(Unset, scalaKey, false, Api.Declaration(Api.Kind.Package))

        val base: Map[Text, Api.Entity] =
          if api(scalaKey).present then api.entities else api.entities.define(scalaKey, pkg)

        val anyClass: Api.Declaration = Api.Declaration(Api.Kind.Class, List(t"abstract"))
        base.define(any, Api.Entity(Designator.Top(scalaKey), t"Any", false, Unset, anyClass))

    // Each entity is replaced where it is, so the map keeps its order.
    val decorated: Map[Text, Api.Entity] =
      withAny.keys.to[List].fold(withAny): (acc: Map[Text, Api.Entity], key: Text) =>
        acc.at(key).lay(acc): (entity: Api.Entity) =>
          // One satisfied by several givens is listed once, by the nearest.
          val extensions: List[Api.Applicable] =
            found.at(key).or(Nil).map(_(1))
            . order: (e: Api.Applicable) =>
                (e.tier, (e.distance, (e.reach, (e.importFrom.or(t""), (e.via.or(t""), e.key)))))

            . deduplicate { (e: Api.Applicable) => (e.key, e.index) }

          val known: List[Text] =
            givens.at(key).or(Nil).map(_(1))
            . deduplicate(_.key)
            . order { (p: Provider) => (reach(keyOf(p.entity.parent), entity), p.key) }
            . map(_.key)

          acc.define(key, entity.copy(extensions = extensions, givens = known))

    api.copy(entities = decorated).relinked
