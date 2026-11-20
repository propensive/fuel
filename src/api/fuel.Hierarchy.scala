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

import dysasymptotics.{linearAccess, linearSize}
import rankings.balancedRanking

// A type's place in the hierarchy, drawn: the type itself, its ancestors up to `Any` through
// every parent it was written with, the types known to extend it (to a few, then a note of how
// many more, at the end of the line they hang from, on the right of the drawing), and a few of
// its peers — the other types under its first parent. The graph is
// laid out on acyclicity's `layered`, which ranks every type a layer below its deepest parent
// and orders each layer to cross as few edges as it can; what remains here is to give each
// layer's boxes positions (pulled towards the mean of their neighbours' without reordering), to
// route each edge with vertical and horizontal segments alone — up from the child to a track
// between the layers, along it, and up a stem shared by everything beneath that parent — and
// to write it as SVG: a box per type with its package above its name, both in the monospace
// font, every box with a page a link to it. A package the page's scope names is not written; one
// with a page may be imported by the reader, so a box says how it reads either way, for Pyrocosm
// to choose under the reader's purview (`Purview.drawing`). `Matchable` is left out, and
// `java.lang.Object` is `AnyRef`. The written form of a parent (`Shape[Int]`) is left to the
// sentence which describes the drawing.
//
// honeycomb reads the SVG, and admits attribute names of letters and hyphens alone, so edges
// are paths (not lines, whose `x1` it rejects) and nothing is namespaced.
object Hierarchy:
  enum Role:
    case Current, Ancestor, Child, Peer, Universal, More

  case class Node
    ( key:      Text,
      name:     Text,
      pkg:      Text,
      href:     Optional[Text],
      role:     Role,
      pkgPath:  Optional[Text] = Unset )

  case class Edge(child: Text, parent: Text, label: Optional[Text] = Unset)
  case class Graph(nodes: List[Node], edges: List[Edge], current: Text)

  case class Box(node: Node, x: Int, y: Int, w: Int, h: Int):
    def centre: Int = x + w/2
    def bottom: Int = y + h

  // An edge's points, from the child's top to the parent's track; the stem from the track to
  // the parent is shared, and drawn once.
  case class Route(edge: Edge, points: List[(Int, Int)])
  case class Stem(x: Int, top: Int, bottom: Int)

  // A note of how many more hang from a parent's line, which runs from `from` up to the note.
  case class Remainder(text: Text, from: Int, to: Int, y: Int)

  case class Layout
    ( boxes:      List[Box],
      routes:     List[Route],
      stems:      List[Stem],
      width:      Int,
      height:     Int,
      remainders: List[Remainder] = Nil )

  // How many children and peers are drawn before a note stands for the rest.
  val childLimit: Int = 6
  val peerLimit: Int = 3

  // The geometry, in pixels: a monospace character at the inline size and at a package's size,
  // the box's height (two lines of text), its horizontal padding and least width, the gap
  // between boxes, the gap between layers (which the tracks run through) and the margin.
  private val charWidth: Double = 7.3
  private val pkgCharWidth: Double = 5.8
  private val boxHeight: Int = 38
  private val padding: Int = 10
  private val leastWidth: Int = 56
  private val gap: Int = 24
  private val layerGap: Int = 44
  private val margin: Int = 8
  private val trackStep: Int = 6

  private def universal(key: Text): Boolean =
    key == Lattice.any || key == Lattice.anyRef || key == Lattice.anyVal

  // The ids of a type's written parents, and of its known children, as the lattice gives them.
  private def parentIds(lattice: Lattice, id: Int): List[Int] = lattice.parents(id)
  private def childIds(lattice: Lattice, id: Int): List[Int] = lattice.children(id)

  // acyclicity's `Layering` gives its layers and the links between them as sequences, which
  // are read here, once each, as lists.
  private def layersOf(layering: Layering[Text]): List[List[Layering.Vertex[Text]]] =
    layering.layers.to[List].map(_.to[List])

  private def linksOf(layering: Layering[Text]): List[List[(Int, Int)]] =
    layering.links.to[List]

  private def ranksOf(layering: Layering[Text]): Map[Text, Int] = layering.rank

  // The greatest of some numbers, unless there are none.
  private def greatest(values: List[Int]): Optional[Int] =
    values.head.let { (first: Int) => values.fold(first)(_.max(_)) }

  // A drawing as it is gathered: its nodes, each placed once, in the order they were first
  // placed; its edges, in order; and the ids of the types whose ancestors are drawn.
  private case class Gathered
    ( nodes: List[Node] = Nil, edges: List[Edge] = Nil, visited: List[Int] = Nil ):

    def place(node: Node): Gathered =
      if nodes.exists(_.key == node.key) then this else copy(nodes = nodes + List(node))

    def edge(edge: Edge): Gathered = copy(edges = edges + List(edge))

  // The graph around an entity, or nothing where it is not a class, trait or enum, or where the
  // drawing would say only that it extends `AnyRef`.
  def of(api: Api, entity: Api.Entity, scope: Scope): Optional[Graph] =
    entity.template.let: template =>
      val drawable: Boolean =
        template.aliasOf.absent && template.kind != Api.Kind.Type &&
          template.kind != Api.Kind.TypeParam

      if !drawable then Unset else api.lattice.id(entity.key).let: id =>
        val lattice: Lattice = api.lattice
        val anyId: Int = lattice.id(Lattice.any).or(-1)
        val anyRefId: Int = lattice.id(Lattice.anyRef).or(-1)
        val anyValId: Int = lattice.id(Lattice.anyVal).or(-1)

        // A key's last name, after its last `.` or `#`.
        def leaf(key: Text): Text = key.cut(t".").last.cut(t"#").last

        def node(key: Text, role: Role): Node =
          val found: Optional[Api.Entity] = api(key)
          val name: Text = found.let(_.name).or(leaf(key))
          val prefix: Text = key.keep(key.length - name.length)
          val dotless: Text = if prefix.ends(t".") then prefix.skip(1, Rtl) else prefix
          val pkg0: Text = if dotless.ends(t"#") then dotless.skip(1, Rtl) else dotless
          val pkg: Text = if scope.names(pkg0) then t"" else pkg0
          val pkgPath: Optional[Text] = if pkg == t"" then Unset else api(pkg).let(_.url)
          val shown: Role = if universal(key) then Role.Universal else role
          Node(key, name, pkg, found.let(_.path), shown, pkgPath)

        def place(gathered: Gathered, key: Text, role: Role): Gathered =
          if gathered.nodes.exists(_.key == key) then gathered else gathered.place(node(key, role))

        // Ancestors, through the written parents; a type with none extends `AnyRef`, or
        // `AnyVal` if it is a value type, and those extend `Any`.
        def ancestors(gathered: Gathered, from: Int, depth: Int): Gathered =
          if depth >= 32 || gathered.visited.has(from) then gathered else
            val written: List[Int] = parentIds(lattice, from)

            val above: List[Int] =
              if !written.nil then written
              else if from == anyId then Nil
              else if from == anyRefId || from == anyValId then List(anyId)
              else List(if lattice.isValue(from) then anyValId else anyRefId)

            val visited: Gathered = gathered.copy(visited = gathered.visited + List(from))

            above.fold(visited): (gathered: Gathered, parent: Int) =>
              val placed: Gathered =
                place(gathered, lattice.key(parent), Role.Ancestor)
                . edge(Edge(lattice.key(from), lattice.key(parent)))

              ancestors(placed, parent, depth + 1)

        def shown(ids: List[Int]): List[Api.Entity] =
          ids.bind { (i: Int) => api(lattice.key(i)).lay(List[Api.Entity]())(List(_)) }
          . filter(!_.hidden)
          . order { (e: Api.Entity) => (!e.own, e.key) }

        def group
          ( gathered: Gathered, all: List[Api.Entity], limit: Int, role: Role, parent: Text )
        :   Gathered =

          val drawn: Gathered = all.keep(limit).fold(gathered): (gathered, child: Api.Entity) =>
            place(gathered, child.key, role).edge(Edge(child.key, parent))

          val more: Int = all.size - limit

          if more <= 0 then drawn else
            val key: Text = t"$parent more"
            val note: Node = Node(key, t"…and ${more.show} more", t"", Unset, Role.More)
            val nodes: List[Node] = drawn.nodes.filter(_.key != key) + List(note)
            drawn.copy(nodes = nodes).edge(Edge(key, parent))

        val children: List[Api.Entity] = shown(childIds(lattice, id))

        // Peers: the other types under the first parent as written.
        val peers: List[Api.Entity] =
          lattice.primaryParent(id).lay(List[Api.Entity]()): (parent: Int) =>
            shown(childIds(lattice, parent).filter(_ != id))

        val gathered: Gathered =
          val start: Gathered = place(Gathered(), entity.key, Role.Current)
          val ancestry: Gathered = ancestors(start, id, 0)
          val beneath: Gathered = group(ancestry, children, childLimit, Role.Child, entity.key)

          lattice.primaryParent(id).lay(beneath): (parent: Int) =>
            group(beneath, peers, peerLimit, Role.Peer, lattice.key(parent))

        val nodes: List[Node] = gathered.nodes

        // The written form of each parent, where it says more than the name.
        val labels: Map[Text, Text] =
          template.parents.zip(template.parentShapes).bind: (syntax, shape) =>
            Lattice.head(api.expand(shape)).lay(List[(Text, Text)]()): head =>
              val key: Text = if head == Lattice.obj then Lattice.anyRef else head
              val label: Text = Signatures.tokens(syntax, api, scope).map(_.text).join
              val same: Boolean = nodes.seek(_.key == key).lay(false)(_.name == label)
              if same then Nil else List((key, label))

          . to[Map]

        val labelled: List[Edge] = gathered.edges.map: (edge: Edge) =>
          if edge.child != entity.key then edge else edge.copy(label = labels.at(edge.parent))

        val trivial: Boolean =
          children.nil && peers.nil && nodes.all: (n: Node) =>
            n.role == Role.Current || n.role == Role.Universal

        if trivial then Unset else Graph(nodes, labelled, entity.key)

  private def boxWidth(node: Node): Int =
    val wide: Double = (node.name.length*charWidth).max(node.pkg.length*pkgCharWidth)
    (wide.toInt + 2*padding).max(leastWidth)

  def layout(graph: Graph): Optional[Layout] =
    // A note of how many more is no box in a layer: it is set beside its parent's line, below.
    val more: List[Node] = graph.nodes.filter(_.role == Role.More)
    val drawn: List[Node] = graph.nodes.filter(_.role != Role.More)
    val noted: List[Text] = more.map(_.key)
    val edges: List[Edge] = graph.edges.filter { (e: Edge) => !noted.has(e.child) }
    val byKey: Map[Text, Node] = drawn.map { (n: Node) => (n.key, n) }.to[Map]
    val parentsOf: Map[Text, List[Edge]] = edges.group(_.child)

    def parentSet(key: Text): Set[Text] = parentsOf.at(key).or(Nil).map(_.parent).to[Set]
    val dag: Optional[Dag[Text]] = safely(Dag(drawn.map(_.key).to[Set])(parentSet))

    dag.let: dag =>
      val layering: Layering[Text] = dag.layered
      val layers: List[List[Layering.Vertex[Text]]] = layersOf(layering)
      val linked: List[List[(Int, Int)]] = linksOf(layering)
      val rank: Map[Text, Int] = ranksOf(layering)
      val depth: Int = layers.size

      // The `index`th (from 0) of some values, which are each layer's or each vertex's.
      def at[value](values: List[value], index: Int, default: value): value =
        values.at(Ordinal.zerary(index)).or(default)

      def vertices(layer: Int): List[Layering.Vertex[Text]] = at(layers, layer, Nil)
      def indices(layer: Int): List[Int] = List.range(0, vertices(layer).size)

      def widthOf(vertex: Layering.Vertex[Text]): Int = vertex match
        case Layering.Vertex.Real(key) => byKey.at(key).lay(0)(boxWidth)
        case _                         => 0

      val widths: List[List[Int]] = layers.map(_.map(widthOf))
      def width(layer: Int, i: Int): Int = at(at(widths, layer, Nil), i, 0)

      // The left edge of each box, by layer, as the layers are settled in turn.
      type Positions = List[List[Double]]

      def x0(xs: Positions, layer: Int, i: Int): Double = at(at(xs, layer, Nil), i, 0.0)
      def centre(xs: Positions, layer: Int, i: Int): Double = x0(xs, layer, i) + width(layer, i)/2.0

      // Each layer's boxes side by side, from the left.
      val initial: Positions = widths.map: (row: List[Int]) =>
        row.fold((List[Double](), 0.0)) { (state: (List[Double], Double), w: Int) =>
          (state(0) + List(state(1)), state(1) + (w + gap))
        }(0)

      // The neighbours of a vertex in the layer above (its parents) and below (its children), as
      // positions there.
      def links(layer: Int): List[(Int, Int)] = at(linked, layer, Nil)

      def above(layer: Int, i: Int): List[Int] =
        if layer == 0 then Nil else links(layer - 1).filter(_(1) == i).map(_(0))

      def below(layer: Int, i: Int): List[Int] =
        if layer == depth - 1 then Nil else links(layer).filter(_(0) == i).map(_(1))

      // Each vertex towards the mean of its neighbours' centres in the layer just settled, the
      // order kept and the gaps enforced from the left, then the layer shifted as one block to
      // sit where its vertices wanted to be.
      def settle(xs: Positions, layer: Int, neighbours: Int => List[Int], fixed: Int): Positions =
        val desired: List[Double] = indices(layer).map: (i: Int) =>
          val near: List[Int] = neighbours(i)

          if near.nil then centre(xs, layer, i)
          else near.map(centre(xs, fixed, _)).fold(0.0)(_ + _)/near.size

        val wanted: List[Double] = indices(layer).map: (i: Int) =>
          at(desired, i, 0.0) - width(layer, i)/2.0

        val spaced: List[Double] = indices(layer).fold(List[Double]()):
          (row: List[Double], i: Int) =>
            val x: Double = at(wanted, i, 0.0)
            def least: Double = at(row, i - 1, 0.0) + width(layer, i - 1) + gap
            row + List(if i == 0 then x else x.max(least))

        if spaced.nil then xs else
          val total: Double = indices(layer).fold(0.0): (sum: Double, i: Int) =>
            sum + (at(spaced, i, 0.0) + width(layer, i)/2.0 - at(desired, i, 0.0))

          val drift: Double = total/spaced.size
          val row: List[Double] = spaced.map(_ - drift)

          xs.indexed.map: (old: List[Double], index: Ordinal) =>
            if index.n0 == layer then row else old

      def down(xs: Positions): Positions =
        List.range(1, depth).fold(xs): (xs: Positions, layer: Int) =>
          settle(xs, layer, above(layer, _), layer - 1)

      def up(xs: Positions): Positions =
        List.range(0, depth - 1).reverse.fold(xs): (xs: Positions, layer: Int) =>
          settle(xs, layer, below(layer, _), layer + 1)

      val settled: Positions = down(up(down(initial)))
      val everything: List[Double] = settled.bind { (row: List[Double]) => row }
      val least: Double = everything.head.lay(0.0): (first: Double) =>
        everything.fold(first) { (least: Double, x: Double) => least.min(x) }

      val xs: Positions = settled.map: (row: List[Double]) =>
        row.map { (x: Double) => x - least + margin }

      // A box's centre is rounded once, and its left edge placed from it, so that boxes with one
      // centre share a column exactly.
      def layerTop(layer: Int): Int = margin + layer*(boxHeight + layerGap)
      def mid(layer: Int, i: Int): Int = (x0(xs, layer, i) + width(layer, i)/2.0).round.toInt
      def x(layer: Int, i: Int): Int = mid(layer, i) - width(layer, i)/2

      val boxes: List[Box] = layers.indexed.bind: (row: List[Layering.Vertex[Text]], l: Ordinal) =>
        val layer: Int = l.n0

        row.indexed.bind: (vertex: Layering.Vertex[Text], index: Ordinal) =>
          val i: Int = index.n0

          vertex match
            case Layering.Vertex.Real(key) =>
              byKey.at(key).lay(List[Box]()): (node: Node) =>
                List(Box(node, x(layer, i), layerTop(layer), width(layer, i), boxHeight))

            case _ =>
              Nil

      // The track beneath each parent of a gap: a few pixels under the layer, one step lower for
      // each parent to its left which also has children, so their runs do not share a line.
      def track(layer: Int, i: Int): Int =
        val parents: List[Int] = indices(layer).filter(!below(layer, _).nil).order(mid(layer, _))
        val index: Int = parents.where(_ == i).lay(0)(_.n0)
        val rank: Int = index.max(0).min((layerGap - 2*margin)/trackStep)
        layerTop(layer) + boxHeight + margin + rank*trackStep

      // Each layer's vertices by position: a long edge's virtual vertex is one value in every
      // layer it passes, so it is found by layer.
      val positions: List[Map[Layering.Vertex[Text], Int]] =
        layers.map: (row: List[Layering.Vertex[Text]]) =>
          row.indexed.map { (vertex, index: Ordinal) => (vertex, index.n0) }.to[Map]

      def position(layer: Int, vertex: Layering.Vertex[Text]): Optional[Int] =
        at(positions, layer, Map[Layering.Vertex[Text], Int]()).at(vertex)

      def route(edge: Edge): Optional[Route] =
        rank.at(edge.child).let: (childLayer: Int) =>
          rank.at(edge.parent).let: (parentLayer: Int) =>
            position(childLayer, Layering.Vertex.Real(edge.child)).let: (childAt: Int) =>
              val start: (Int, Int) = (mid(childLayer, childAt), layerTop(childLayer))

              // From the child upward, through the virtual vertex in each intermediate layer.
              def climb(layer: Int, i: Int, points: List[(Int, Int)]): List[(Int, Int)] =
                if layer == parentLayer then points else
                  val upper: Layering.Vertex[Text] =
                    if layer - 1 == parentLayer then Layering.Vertex.Real(edge.parent)
                    else Layering.Vertex.Virtual(edge.parent, edge.child)

                  position(layer - 1, upper).lay(points): (j: Int) =>
                    val y: Int = track(layer - 1, j)
                    climb(layer - 1, j, points + List((mid(layer, i), y), (mid(layer - 1, j), y)))

              Route(edge, climb(childLayer, childAt, List(start)))

      val routes: List[Route] =
        edges.bind { (e: Edge) => route(e).lay(List[Route]())(List(_)) }

      // One stem for each parent with children, from its track to its bottom.
      val stems: List[Stem] = layers.indexed.bind: (row: List[Layering.Vertex[Text]], l: Ordinal) =>
        val layer: Int = l.n0

        row.indexed.bind: (vertex: Layering.Vertex[Text], index: Ordinal) =>
          val i: Int = index.n0

          vertex match
            case Layering.Vertex.Real(_) if !below(layer, i).nil =>
              List(Stem(mid(layer, i), layerTop(layer) + boxHeight, track(layer, i)))

            case _ =>
              Nil

      val right: Int = greatest(boxes.map { (b: Box) => b.x + b.w }).or(0)
      val noteAt: Int = right + gap/2

      // Each note on its parent's track, all of them in one column at the right of the drawing.
      val remainders: List[Remainder] = more.bind: (note: Node) =>
        graph.edges.seek(_.child == note.key).lay(List[Remainder]()): (edge: Edge) =>
          rank.at(edge.parent).lay(List[Remainder]()): (layer: Int) =>
            position(layer, Layering.Vertex.Real(edge.parent)).lay(List[Remainder]()):
              (i: Int) => List(Remainder(note.name, mid(layer, i), noteAt, track(layer, i)))

      val notes: Int =
        greatest(remainders.map { (r: Remainder) => (r.text.length*charWidth).toInt }).or(-gap/2)

      val wide: Int = noteAt + notes + margin
      val height: Int = layerTop(depth - 1) + boxHeight + margin
      Layout(boxes, routes, stems, wide, height, remainders)

  private def escape(text: Text): Text =
    text.sub(t"&", t"&amp;").sub(t"<", t"&lt;").sub(t">", t"&gt;").sub(t"\"", t"&quot;")

  private val mono: Text = t"font-family: var(--pyro-font-mono)"

  def svg(layout: Layout): Text =
    def path(points: List[(Int, Int)]): Text = points match
      case (x, y) :: rest =>
        val moves: List[Text] = rest.zip(points).bind: (to, from) =>
          if to == from then Nil
          else if to(0) == from(0) then List(t"V${to(1).show}")
          else List(t"H${to(0).show}")

        t"M${x.show} ${y.show}${moves.join}"

      case _ =>
        t""

    val line: Text = t"fill: none; stroke: var(--pyro-border); stroke-width: 1.2"

    val edges: Text = layout.routes.map: route =>
      t"""<path d="${path(route.points)}" style="$line"/>"""

    . join

    val stems: Text = layout.stems.map: stem =>
      val d: Text = t"M${stem.x.show} ${stem.bottom.show}V${stem.top.show}"
      t"""<path d="$d" style="$line"/>"""

    . join

    val boxes: Text = layout.boxes.map: box =>
      val node: Node = box.node
      val current: Boolean = node.role == Role.Current

      val stroke: Text =
        if current then t"stroke: var(--pyro-tone-accent); stroke-width: 2"
        else t"stroke: var(--pyro-border); stroke-width: 1"

      val nameFill: Text =
        if current then t"var(--pyro-fg)"
        else if node.href.present then t"var(--pyro-link)"
        else t"var(--pyro-muted)"

      val mid: Text = box.centre.show

      // A line of text in the box, centred, `y` pixels below its top.
      def text(y: Int, style: Text, content: Text): Text =
        val at: Text = t"""x="$mid" y="${(box.y + y).show}" text-anchor="middle""""
        t"""<text $at style="$mono; $style">${escape(content)}</text>"""

      val position: Text = t"""x="${box.x.show}" y="${box.y.show}""""
      val size: Text = t"""width="${box.w.show}" height="${box.h.show}""""
      val fill: Text = t"fill: var(--pyro-surface); $stroke"
      val rect: Text = t"""<rect $position $size style="$fill"/>"""

      val smaller: Text = t"font-size: calc(var(--pyro-text-small)*0.8); fill: var(--pyro-muted)"
      val pkg: Text = text(14, smaller, node.pkg)

      def name(y: Int): Text =
        text(y, t"font-size: var(--pyro-text-inline); fill: $nameFill", node.name)

      val named: Text = if node.pkg == t"" then name(24) else pkg + name(30)

      val lines: Text = node.pkgPath.lay(named): (path: Text) =>
        val at: Text = escape(path)
        t"""<g data-not-imported="$at">$named</g><g data-imported="$at">${name(24)}</g>"""

      val inner: Text = rect + lines

      if current then t"<g>$inner</g>"
      else
        node.href.lay(t"<g>$inner</g>"): (href: Text) =>
          t"""<a class="pyro-locate" href="${escape(href)}">$inner</a>"""

    . join

    // A parent's line, carried on to its note of how many more, which reads centred on it.
    val remainders: Text = layout.remainders.map: remainder =>
      val y: Text = remainder.y.show
      val d: Text = t"M${remainder.from.show} ${y}H${(remainder.to - 4).show}"
      val at: Text = t"""x="${remainder.to.show}" y="${(remainder.y + 4).show}""""
      val small: Text = t"font-size: var(--pyro-text-small); fill: var(--pyro-muted)"
      val note: Text = t"""<text $at style="$mono; $small">${escape(remainder.text)}</text>"""
      t"""<path d="$d" style="$line"/>$note"""

    . join

    val w: Text = layout.width.show
    val h: Text = layout.height.show

    // Drawn at its own size, as every hierarchy is, never scaled down to the width there is:
    // one wider than its column is panned across by the pointer (Pyrocosm's holder). The
    // inline style outranks the stylesheet's sizing of a drawing.
    val xmlns: Text = t"http://www.w3.org/2000/svg"
    val style: Text = t"width: ${w}px; height: auto"
    val open: Text = t"""<svg xmlns="$xmlns" viewBox="0 0 $w $h" role="img" style="$style">"""
    t"""$open<g>$edges$stems$remainders</g><g>$boxes</g></svg>"""

  // The drawing in a sentence, for a reader without it.
  def alt(graph: Graph): Text =
    val byKey: Map[Text, Node] = graph.nodes.map { (n: Node) => (n.key, n) }.to[Map]
    def name(key: Text): Text = byKey.at(key).lay(key)(_.name)
    val current: Text = name(graph.current)

    def names(keys: List[Text]): Text =
      val named: List[Text] = keys.map(name)

      if named.nil then t""
      else if named.size == 1 then named.head.or(t"")
      else t"${named.skip(1, Rtl).join(t", ")} and ${named.last.or(t"")}"

    // The universal types go unsaid: every type extends them.
    val parents: List[Text] =
      graph.edges.filter { (e: Edge) => e.child == graph.current && !universal(e.parent) }
      . map { (e: Edge) => e.label.or(name(e.parent)) }

    def below(key: Text): Boolean =
      byKey.at(key).lay(false) { (n: Node) => n.role == Role.Child || n.role == Role.More }

    val children: List[Text] =
      graph.edges.filter { (e: Edge) => e.parent == graph.current && below(e.child) }.map(_.child)

    def peer(node: Node): Boolean =
      node.role == Role.Peer || (node.role == Role.More && !children.has(node.key))

    val peers: List[Text] = graph.nodes.filter(peer).map(_.key)

    val under: Optional[Text] =
      graph.edges.seek(_.child == graph.current).let { (e: Edge) => name(e.parent) }

    val extending: Text = if parents.nil then t"" else t" extends ${names(parents)}"
    val extended: Text = if children.nil then t"" else t"; extended by ${names(children)}"
    val beneath: Text = under.lay(t""): p => t" under $p"
    val siblings: Text = if peers.nil then t"" else t"; a sibling of ${names(peers)}$beneath"

    t"$current$extending$extended$siblings."
