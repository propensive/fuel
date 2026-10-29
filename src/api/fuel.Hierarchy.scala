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

import soundness.{Language as _, Token as _, Query as _, *}

import rankings.balancedRanking

// A type's place in the hierarchy, drawn: the type itself, its ancestors up to `Any` through
// every parent it was written with, the types known to extend it (to a few, then a note of how
// many more), and a few of its peers — the other types under its first parent. The graph is
// laid out on acyclicity's `layered`, which ranks every type a layer below its deepest parent
// and orders each layer to cross as few edges as it can; what remains here is to give each
// layer's boxes positions (pulled towards the mean of their neighbours' without reordering), to
// route each edge with vertical and horizontal segments alone — up from the child to a track
// between the layers, along it, and up a stem shared by everything beneath that parent — and
// to write it as SVG: a box per type with its package above its name, both in the monospace
// font, every box with a page a link to it. The written form of a parent (`Shape[Int]`) labels
// the type's own edge to it. `Matchable` is left out, and `java.lang.Object` is `AnyRef`.
//
// honeycomb reads the SVG, and admits attribute names of letters and hyphens alone, so edges
// are paths (not lines, whose `x1` it rejects) and nothing is namespaced.
object Hierarchy:
  enum Role:
    case Current, Ancestor, Child, Peer, Universal, More

  case class Node(key: Text, name: Text, pkg: Text, href: Optional[Text], role: Role)
  case class Edge(child: Text, parent: Text, label: Optional[Text] = Unset)
  case class Graph(nodes: List[Node], edges: List[Edge], current: Text)

  case class Box(node: Node, x: Int, y: Int, w: Int, h: Int):
    def centre: Int = x + w/2
    def bottom: Int = y + h

  // An edge's points, from the child's top to the parent's track; the stem from the track to
  // the parent is shared, and drawn once.
  case class Route(edge: Edge, points: scala.List[(Int, Int)], labelAt: Optional[(Int, Int)])
  case class Stem(x: Int, top: Int, bottom: Int)
  case class Layout(boxes: List[Box], routes: List[Route], stems: List[Stem], width: Int, height: Int)

  // How many children and peers are drawn before a note stands for the rest.
  val childLimit: Int = 6
  val peerLimit: Int = 3

  // The geometry, in pixels: a monospace character at the inline size and at the small size,
  // the box's height (two lines of text), its horizontal padding and least width, the gap
  // between boxes, the gap between layers (which the tracks run through) and the margin.
  private val charWidth: Double = 7.3
  private val smallCharWidth: Double = 6.3
  private val boxHeight: Int = 38
  private val padding: Int = 10
  private val leastWidth: Int = 56
  private val gap: Int = 24
  private val layerGap: Int = 44
  private val margin: Int = 8
  private val trackStep: Int = 6
  private val labelStep: Int = 12

  private def universal(key: Text): Boolean = key == Lattice.any || key == Lattice.anyRef || key == Lattice.anyVal

  // The graph around an entity, or nothing where it is not a class, trait or enum, or where the
  // drawing would say only that it extends `AnyRef`.
  def of(api: Api, entity: Api.Entity, scope: Scope): Optional[Graph] =
    entity.template.let: template =>
      val drawable: Boolean = template.aliasOf.absent && template.kind != Api.Kind.Type && template.kind != Api.Kind.TypeParam

      if !drawable then Unset else api.lattice.id(entity.key).let: id =>
        val lattice: Lattice = api.lattice
        val anyId: Int = lattice.id(Lattice.any).or(-1)
        val anyRefId: Int = lattice.id(Lattice.anyRef).or(-1)
        val anyValId: Int = lattice.id(Lattice.anyVal).or(-1)

        val nodes: scm.LinkedHashMap[Text, Node] = scm.LinkedHashMap()
        val edges: scm.ListBuffer[Edge] = scm.ListBuffer()

        def node(key: Text, role: Role): Node =
          val found: Optional[Api.Entity] = api(key)
          val name: Text = found.let(_.name).or(Text(key.s.substring(key.s.lastIndexWhere { c => c == '.' || c == '#' } + 1).nn))
          val pkg: Text = Text(key.s.substring(0, key.s.length - name.s.length).nn.stripSuffix(".").stripSuffix("#"))
          Node(key, name, pkg, found.let(_.path), if universal(key) then Role.Universal else role)

        def place(key: Text, role: Role): Unit = if !nodes.contains(key) then nodes(key) = node(key, role)

        place(entity.key, Role.Current)

        // Ancestors, through the written parents; a type with none extends `AnyRef`, or
        // `AnyVal` if it is a value type, and those extend `Any`.
        val visited: scm.HashSet[Int] = scm.HashSet()

        def ancestors(from: Int, depth: Int): Unit = if depth < 32 && visited.add(from) then
          val written: scala.List[Int] = lattice.parents(from)
          val above: scala.List[Int] =
            if written.nonEmpty then written
            else if from == anyId then scala.Nil
            else if from == anyRefId || from == anyValId then scala.List(anyId)
            else scala.List(if lattice.isValue(from) then anyValId else anyRefId)

          above.foreach: parent =>
            place(lattice.key(parent), Role.Ancestor)
            edges += Edge(lattice.key(from), lattice.key(parent))
            ancestors(parent, depth + 1)

        ancestors(id, 0)

        def shown(ids: scala.List[Int]): scala.List[Api.Entity] =
          ids.flatMap { i => api(lattice.key(i)).let(scala.List(_)).or(scala.Nil) }.filter(!_.hidden).sortBy { e => (!e.own, e.key.s) }

        def group(all: scala.List[Api.Entity], limit: Int, role: Role, parent: Text): Int =
          all.take(limit).foreach: child =>
            place(child.key, role)
            edges += Edge(child.key, parent)

          val more: Int = all.size - limit

          if more > 0 then
            val key: Text = t"$parent more"
            nodes(key) = Node(key, t"…and ${more.toString.tt} more", t"", Unset, Role.More)
            edges += Edge(key, parent)

          more.max(0)

        val children: scala.List[Api.Entity] = shown(lattice.children(id))
        group(children, childLimit, Role.Child, entity.key)

        // Peers: the other types under the first parent as written.
        val peers: scala.List[Api.Entity] = lattice.primaryParent(id).lay(scala.Nil): (parent: Int) =>
          shown(lattice.children(parent).filter(_ != id))

        lattice.primaryParent(id).let { (parent: Int) => group(peers, peerLimit, Role.Peer, lattice.key(parent)) }

        // The written form of each parent, where it says more than the name.
        val labels: sci.Map[Text, Text] =
          template.parents.stdlib.zip(template.parentShapes.stdlib).flatMap: (syntax, shape) =>
            Lattice.head(api.expand(shape)).let: head =>
              val key: Text = if head == Lattice.obj then Lattice.anyRef else head
              val label: Text = Signatures.tokens(syntax, api, scope).map(_.text).join
              if nodes.get(key).exists(_.name == label) then scala.Nil else scala.List(key -> label)
            . or(scala.Nil)
          . toMap

        val labelled: scala.List[Edge] = edges.toList.map: edge =>
          if edge.child == entity.key then edge.copy(label = labels.get(edge.parent).getOrElse(Unset)) else edge

        val trivial: Boolean =
          children.isEmpty && peers.isEmpty && nodes.values.forall { n => n.role == Role.Current || n.role == Role.Universal }

        if trivial then Unset else Graph(List.from(nodes.values), List.from(labelled), entity.key)

  private def boxWidth(node: Node): Int =
    val wide: Double = (node.name.length*charWidth).max(node.pkg.length*smallCharWidth)
    (wide.toInt + 2*padding).max(leastWidth)

  def layout(graph: Graph): Optional[Layout] =
    val byKey: sci.Map[Text, Node] = graph.nodes.stdlib.map { n => n.key -> n }.toMap
    val parentsOf: sci.Map[Text, scala.List[Text]] = graph.edges.stdlib.groupMap(_.child)(_.parent)

    val dag: Optional[Dag[Text]] =
      safely(Dag(graph.nodes.map(_.key).to[Set]) { (key: Text) => List.from(parentsOf.getOrElse(key, scala.Nil)).to[Set] })

    dag.let: dag =>
      val layering: Layering[Text] = dag.layered
      val layers: scala.Array[scala.Array[Layering.Vertex[Text]]] = layering.layers.map(_.toArray).toArray
      val depth: Int = layers.length

      def widthOf(vertex: Layering.Vertex[Text]): Int = vertex match
        case Layering.Vertex.Real(key) => boxWidth(byKey(key))
        case _                         => 0

      val widths: scala.Array[scala.Array[Int]] = layers.map(_.map(widthOf))
      val xs: scala.Array[scala.Array[Double]] = layers.map { layer => new scala.Array[Double](layer.length) }

      def initial(layer: Int): Unit =
        var cursor: Double = 0.0
        layers(layer).indices.foreach: i =>
          xs(layer)(i) = cursor
          cursor += widths(layer)(i) + gap

      (0 until depth).foreach(initial)

      def centre(layer: Int, i: Int): Double = xs(layer)(i) + widths(layer)(i)/2.0

      // The neighbours of a vertex in the layer above (its parents) and below (its children), as
      // positions there.
      def above(layer: Int, i: Int): scala.List[Int] =
        if layer == 0 then scala.Nil else layering.links(layer - 1).filter(_(1) == i).map(_(0))

      def below(layer: Int, i: Int): scala.List[Int] =
        if layer == depth - 1 then scala.Nil else layering.links(layer).filter(_(0) == i).map(_(1))

      // Each vertex towards the mean of its neighbours' centres in the layer just settled, the
      // order kept and the gaps enforced from the left, then the layer shifted as one block to
      // sit where its vertices wanted to be.
      def settle(layer: Int, neighbours: Int => scala.List[Int], fixed: Int): Unit =
        val desired: scala.Array[Double] = layers(layer).indices.map: i =>
          val near: scala.List[Int] = neighbours(i)
          if near.isEmpty then centre(layer, i) else near.map(centre(fixed, _)).sum/near.length
        . toArray

        layers(layer).indices.foreach { i => xs(layer)(i) = desired(i) - widths(layer)(i)/2.0 }

        (1 until layers(layer).length).foreach: i =>
          xs(layer)(i) = xs(layer)(i).max(xs(layer)(i - 1) + widths(layer)(i - 1) + gap)

        if layers(layer).nonEmpty then
          val drift: Double = layers(layer).indices.map { i => centre(layer, i) - desired(i) }.sum/layers(layer).length
          layers(layer).indices.foreach { i => xs(layer)(i) -= drift }

      def down(): Unit = (1 until depth).foreach { layer => settle(layer, above(layer, _), layer - 1) }
      def up(): Unit = (depth - 2 to 0 by -1).foreach { layer => settle(layer, below(layer, _), layer + 1) }

      down(); up(); down()

      val least: Double = xs.iterator.flatMap(_.iterator).minOption.getOrElse(0.0)
      xs.foreach { row => row.indices.foreach { i => row(i) = row(i) - least + margin } }

      // The type's own labelled edges stack their labels up its vertical segment, so the gap
      // above its layer grows to hold them.
      val labelled: scala.List[Edge] = graph.edges.stdlib.filter { e => e.child == graph.current && e.label.present }
      val currentLayer: Int = layering.rank.getOrElse(graph.current, -1)
      def gapAbove(layer: Int): Int = if layer == currentLayer then layerGap.max(2*margin + labelStep*labelled.length) else layerGap

      val tops: scala.Array[Int] =
        (1 until depth).scanLeft(margin) { (top, layer) => top + boxHeight + gapAbove(layer) }.toArray

      // A box's centre is rounded once, and its left edge placed from it, so that boxes with one
      // centre share a column exactly.
      def layerTop(layer: Int): Int = tops(layer)
      def mid(layer: Int, i: Int): Int = (xs(layer)(i) + widths(layer)(i)/2.0).round.toInt
      def x(layer: Int, i: Int): Int = mid(layer, i) - widths(layer)(i)/2

      val boxes: scala.List[Box] = layers.indices.toList.flatMap: layer =>
        layers(layer).indices.toList.flatMap: i =>
          layers(layer)(i) match
            case Layering.Vertex.Real(key) => scala.List(Box(byKey(key), x(layer, i), layerTop(layer), widths(layer)(i), boxHeight))
            case _                         => scala.Nil

      // The track beneath each parent of a gap: a few pixels under the layer, one step lower for
      // each parent to its left which also has children, so their runs do not share a line.
      def track(layer: Int, i: Int): Int =
        val parents: scala.List[Int] = (0 until layers(layer).length).toList.filter(below(layer, _).nonEmpty).sortBy(mid(layer, _))
        val rank: Int = parents.indexOf(i).max(0).min((layerGap - 2*margin)/trackStep)
        layerTop(layer) + boxHeight + margin + rank*trackStep

      // Each layer's vertices by position: a long edge's virtual vertex is one value in every
      // layer it passes, so it is found by layer.
      val positions: scala.Array[sci.Map[Layering.Vertex[Text], Int]] = layers.map(_.iterator.zipWithIndex.toMap)

      def route(edge: Edge): Optional[Route] =
        (layering.rank.get(edge.child), layering.rank.get(edge.parent)) match
          case (Some(childLayer), Some(parentLayer)) =>
            val childAt: Int = positions(childLayer)(Layering.Vertex.Real(edge.child))
            val start: (Int, Int) = (mid(childLayer, childAt), layerTop(childLayer))

            // From the child upward, through the virtual vertex in each intermediate layer.
            def climb(layer: Int, i: Int, points: scala.List[(Int, Int)]): scala.List[(Int, Int)] =
              if layer == parentLayer then points else
                val upper: Layering.Vertex[Text] =
                  if layer - 1 == parentLayer then Layering.Vertex.Real(edge.parent) else Layering.Vertex.Virtual(edge.parent, edge.child)

                positions(layer - 1).get(upper) match
                  case Some(j) =>
                    val y: Int = track(layer - 1, j)
                    climb(layer - 1, j, points :+ (mid(layer, i), y) :+ (mid(layer - 1, j), y))

                  case None => points

            val points: scala.List[(Int, Int)] = climb(childLayer, childAt, scala.List(start))
            val label: Optional[(Int, Int)] = edge.label.let { _ => (start._1 + 4, start._2 - 6 - labelStep*labelled.indexOf(edge).max(0)) }
            Route(edge, points, label)

          case _ => Unset

      val routes: scala.List[Route] = graph.edges.stdlib.flatMap { e => route(e).let(scala.List(_)).or(scala.Nil) }

      // One stem for each parent with children, from its track to its bottom.
      val stems: scala.List[Stem] = layers.indices.toList.flatMap: layer =>
        layers(layer).indices.toList.flatMap: i =>
          layers(layer)(i) match
            case Layering.Vertex.Real(_) if below(layer, i).nonEmpty =>
              scala.List(Stem(mid(layer, i), layerTop(layer) + boxHeight, track(layer, i)))
            case _ => scala.Nil

      val width: Int = boxes.map { b => b.x + b.w }.maxOption.getOrElse(0) + margin
      val height: Int = layerTop(depth - 1) + boxHeight + margin
      Layout(List.from(boxes), List.from(routes), List.from(stems), width, height)

  private def escape(text: Text): Text =
    Text(text.s.replace("&", "&amp;").nn.replace("<", "&lt;").nn.replace(">", "&gt;").nn.replace("\"", "&quot;").nn)

  private val mono: Text = t"font-family: var(--pyro-font-mono)"

  def svg(layout: Layout): Text =
    def path(points: scala.List[(Int, Int)]): Text = points match
      case scala.Nil => t""
      case scala.::((x, y), rest) =>
        val moves: scala.List[Text] = rest.zip(points).flatMap: (to, from) =>
          if to == from then scala.Nil
          else if to._1 == from._1 then scala.List(t"V${to._2.toString.tt}")
          else scala.List(t"H${to._1.toString.tt}")
        t"M${x.toString.tt} ${y.toString.tt}${List.from(moves).join}"

    val edges: Text = layout.routes.map: route =>
      val own: Boolean = layout.boxes.exists { b => b.node.key == route.edge.child && b.node.role == Role.Current }
      val stroke: Text = if own then t"var(--pyro-muted)" else t"var(--pyro-border)"
      t"""<path d="${path(route.points)}" style="fill: none; stroke: $stroke; stroke-width: 1.2"/>"""
    . join

    val stems: Text = layout.stems.map: stem =>
      t"""<path d="M${stem.x.toString.tt} ${stem.bottom.toString.tt}V${stem.top.toString.tt}" style="fill: none; stroke: var(--pyro-border); stroke-width: 1.2"/>"""
    . join

    val boxes: Text = layout.boxes.map: box =>
      val node: Node = box.node
      val current: Boolean = node.role == Role.Current
      val more: Boolean = node.role == Role.More
      val stroke: Text = if current then t"stroke: var(--pyro-tone-accent); stroke-width: 2" else t"stroke: var(--pyro-border); stroke-width: 1"
      val dashes: Text = if more then t""" stroke-dasharray="4 3"""" else t""
      val nameFill: Text = if current then t"var(--pyro-fg)" else if node.href.present then t"var(--pyro-link)" else t"var(--pyro-muted)"
      val mid: Text = box.centre.toString.tt

      val rect: Text = t"""<rect x="${box.x.toString.tt}" y="${box.y.toString.tt}" width="${box.w.toString.tt}" height="${box.h.toString.tt}" style="fill: var(--pyro-surface); $stroke"$dashes/>"""
      val pkg: Text = if node.pkg == t"" then t"" else t"""<text x="$mid" y="${(box.y + 14).toString.tt}" text-anchor="middle" style="$mono; font-size: var(--pyro-text-small); fill: var(--pyro-muted)">${escape(node.pkg)}</text>"""
      val name: Text = t"""<text x="$mid" y="${(box.y + 30).toString.tt}" text-anchor="middle" style="$mono; font-size: var(--pyro-text-inline); fill: $nameFill">${escape(node.name)}</text>"""
      val inner: Text = rect + pkg + name

      if current then t"<g>$inner</g>"
      else node.href.lay(t"<g>$inner</g>") { (href: Text) => t"""<a class="pyro-locate" href="${escape(href)}">$inner</a>""" }
    . join

    val labels: Text = List.from(layout.routes.stdlib.flatMap { route =>
      route.labelAt.let { (x, y) =>
        route.edge.label.let { label =>
          scala.List(t"""<text x="${x.toString.tt}" y="${y.toString.tt}" text-anchor="start" style="$mono; font-size: var(--pyro-text-small); fill: var(--pyro-muted); paint-order: stroke; stroke: var(--pyro-surface); stroke-width: 3">${escape(label)}</text>""")
        }.or(scala.Nil)
      }.or(scala.Nil)
    }).join

    val w: Text = layout.width.toString.tt
    val h: Text = layout.height.toString.tt

    // Drawn at its own size where there is room, and scaled down to the width there is where
    // there is not; the inline style outranks the stylesheet's sizing of a drawing.
    t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 $w $h" role="img" style="width: min(100%, ${w}px); height: auto"><g>$edges$stems</g><g>$boxes</g><g>$labels</g></svg>"""

  // The drawing in a sentence, for a reader without it.
  def alt(graph: Graph): Text =
    val byKey: sci.Map[Text, Node] = graph.nodes.stdlib.map { n => n.key -> n }.toMap
    val current: Text = byKey.get(graph.current).map(_.name).getOrElse(graph.current)

    def names(keys: scala.List[Text]): Text = keys.map { k => byKey.get(k).map(_.name).getOrElse(k) } match
      case scala.Nil                => t""
      case scala.::(one, scala.Nil) => one
      case many                     => t"${List.from(many.init).join(t", ")} and ${many.last}"

    // The universal types go unsaid: every type extends them.
    val parents: scala.List[Text] =
      graph.edges.stdlib.filter { e => e.child == graph.current && !universal(e.parent) }
      . map { e => e.label.or(byKey.get(e.parent).map(_.name).getOrElse(e.parent)) }

    val children: scala.List[Text] = graph.edges.stdlib.filter { e => e.parent == graph.current && byKey.get(e.child).exists(n => n.role == Role.Child || n.role == Role.More) }.map(_.child)
    val peers: scala.List[Text] = graph.nodes.stdlib.filter { n => n.role == Role.Peer || (n.role == Role.More && !children.contains(n.key)) }.map(_.key)
    val under: Optional[Text] = graph.edges.stdlib.find(_.child == graph.current).map { e => byKey.get(e.parent).map(_.name).getOrElse(e.parent) }.getOrElse(Unset)

    val extending: Text = if parents.isEmpty then t"" else t" extends ${names(parents)}"
    val extended: Text = if children.isEmpty then t"" else t"; extended by ${names(children)}"
    val siblings: Text = if peers.isEmpty then t"" else t"; a sibling of ${names(peers)}${under.lay(t"") { p => t" under $p" }}"

    t"$current$extending$extended$siblings."
