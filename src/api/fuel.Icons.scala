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

import dysasymptotics.linearSize
import pyrocosm.Inline

// The glyph beside a member's name: a drawing of its keyword, on the 14-unit grid of the
// original icons (a 10 mm box, every shape on multiples of a fourteenth of it), as SVG markup
// which Pyrocosm draws inline, in the text's colour and at its size. A modifier transforms the
// drawing: generically where it can — `final` adds a full stop beside it, `inline` shears it,
// `transparent` draws only its outline, as if it were seen through — and by a shape of its own
// where the drawings differ: an abstract `def` is nine small squares, an opaque type has a core,
// an enum case is a disc.
//
// Every modifier fuel records, and what the glyph does with it (one marked *undecided* is drawn
// plainly until it is decided; the drawings named are the originals to decide from):
//
//   final         a full stop at the glyph's lower right, placed for each shape: its own
//                 diameter away from the nearest part of it
//   inline        sheared to the right
//   transparent   only the outline of each shape
//   abstract      dotted: a def's square is nine small squares, a class's rings are dashes
//   opaque        type: a core ring (o-type)
//   sealed        class, trait: the two rings joined across each open end by a wall along
//                 its radius
//   lazy          an ellipsis at the glyph's lower left; not on an object, which is not
//                 written `lazy`
//   protected     a shelter over the glyph: a wide circumflex
//   erased        a slash through the glyph, at 45°, with a gap about it so that it shows
//   override      a line beneath the glyph
//   infix         a small square at mid-height either side, as if the glyph were placed infix,
//                 close to each shape's own edges
//   into          a narrow right-pointing triangle to the left of the glyph
//   annotation    an `@` in the top right corner, on what extends an annotation class
//   java          a `J` in the top left corner, on what was compiled from Java
//   implicit      nothing: it is deprecated
//   open          class: the inner ring only its western half, its mouth twice as wide
//   optional      a small `?` in the top right corner (a TEL schema's member); with
//                 `repeatable`, a small `*` there instead
//   repeatable    a small `+` in the top right corner: one or more; with `optional`, the `*`
//   key           a small key in the top left corner, hanging down the left-hand side
//   update        def: a notch cut from the top of the square, as a var's from a val's triangle
//   consume       def: a semicircle cut from the right-hand side of the square
//   extension     a def with a small plus at the top right (e-def)
//   enum case     a disc (c-case); on a class or object, a dot at the centre (c-class, c-object)
//
// A class is three quarters of two rings, open to the east, and a trait half of the same rings,
// from the south-east to the north-west. A type is four arcs of the outer ring, each an eighth of
// it, centred on the diagonals. A class, a case class, a trait or a type with a companion object
// is one glyph, a whole circle: its own part, and the object's ring in the sectors it leaves
// open, with a gap between them along each radius where they meet (to the north-east and
// south-east of a class, to the north-west and south-east of a trait, on either side of each
// of a type's arcs), as if a line were drawn there in the page's colour.
// A case class is a class with the disc of a `case` at its centre, small enough to stand a
// ring's breadth within its rings. The glyph of a pair has the modifiers of both.
object Icons:
  private val grid: Int = 14
  private val c: Double = 7.0

  // A drawing: its elements, and how far it reaches left and right, past the grid where a
  // decoration needs to.
  private case class Drawing(elements: List[Text], left: Double = 0, right: Double = grid):
    def add(more: Text*): Drawing = copy(elements = elements + more)

    def reach(from: Double, to: Double): Drawing =
      copy(left = left.min(from), right = right.max(to))

    def within(open: Text, close: Text): Drawing =
      copy(elements = List(open) + elements + List(close))

    def markup: Text =
      val box: Text = t"""viewBox="${num(left)} 0 ${num(right - left)} ${grid.show}""""
      t"""<svg xmlns="http://www.w3.org/2000/svg" $box>${elements.join}</svg>"""

  // A coordinate to two decimal places, without the zeros a fraction ends with, nor a point
  // with nothing after it; a negative one which rounds to nothing is `-0`. A half is rounded away
  // from zero as the coordinate's shortest decimal form writes it, as the drawings were first
  // measured: `Decimalizer` rounds the binary value, which falls just short of the half written
  // as `8.805`, so it is given the coordinate already rounded, in hundredths.
  private val decimalizer: Decimalizer = Decimalizer(decimalPlaces = 2, exponentThreshold = Unset)

  private def num(value: Double): Text =
    val rounded: Long = hundredths(value.abs)

    if rounded == 0 then (if value < 0 then t"-0" else t"0") else
      val text: Text = decimalizer.decimalize((if value < 0 then -rounded else rounded)/100.0)
      if !text.contains('.') then text else text.skip(_ == '0', Rtl).skip(_ == '.', Rtl)

  // A magnitude in hundredths, rounded half up from its shortest decimal form, which is written
  // without an exponent from a thousandth up to ten million, beyond any drawing's measure.
  private def hundredths(magnitude: Double): Long =
    import decimalConverters.javaDecimalConverter

    if magnitude < 0.001 then 0L else
      val written: List[Text] = magnitude.show.cut(t".")
      val fraction: Text = written.skip(1).join + t"00"
      val kept: Text = written.head.or(t"0") + fraction.keep(2)
      val up: Boolean = fraction.at(2.z).let(_ >= '5').or(false)
      kept.fold(0L) { (sum, digit) => sum*10 + (digit - '0') } + (if up then 1 else 0)

  // A point on a circle about the centre, by its radius and its angle in degrees, clockwise from
  // the right (the page's `y` grows downward).
  private def point(radius: Double, degrees: Double): Text =
    val radians: Double = degrees*π/180
    t"${num(c + radius*cos(radians).double)} ${num(c + radius*sin(radians).double)}"

  private def disc(radius: Double, x: Double = c, y: Double = c): Text =
    t"""<circle cx="${num(x)}" cy="${num(y)}" r="${num(radius)}"/>"""

  private def rect(x: Double, y: Double, width: Double, height: Double): Text =
    t"""<rect x="${num(x)}" y="${num(y)}" width="${num(width)}" height="${num(height)}"/>"""

  private def polygon(points: (Double, Double)*): Text =
    val listed: Text = points.map { (x, y) => t"${num(x)},${num(y)}" }.join(t" ")
    t"""<polygon points="$listed"/>"""

  // A ring between two radii, as one path with a hole.
  private def ring(outer: Double, inner: Double): Text =
    def circle(r: Double): Text =
      val d: Text = num(r)
      val left: Text = t"${num(c - r)} ${num(c)}"
      val right: Text = t"${num(c + r)} ${num(c)}"
      t"M $left A $d $d 0 1 0 $right A $d $d 0 1 0 $left Z"

    t"""<path fill-rule="evenodd" d="${circle(outer)} ${circle(inner)}"/>"""

  // The part of a ring from one angle clockwise to another, its ends radial.
  private def band(outer: Double, inner: Double, from: Double, to: Double): Text =
    val sweep: Double = ((to - from)%360 + 360)%360
    val large: Text = if sweep > 180 then t"1" else t"0"
    val o: Text = num(outer)
    val i: Text = num(inner)

    val outerArc: Text = t"M ${point(outer, from)} A $o $o 0 $large 1 ${point(outer, to)}"
    val innerArc: Text = t"L ${point(inner, to)} A $i $i 0 $large 0 ${point(inner, from)} Z"
    t"""<path d="$outerArc $innerArc"/>"""

  // The part of a ring from one angle clockwise to another, each end drawn back from its radius
  // by a distance, and parallel to it: two such parts which meet at a radius leave a gap of even
  // width along it.
  private def inset(outer: Double, inner: Double, from: Double, to: Double, back: Double): Text =
    t"""<path d="${outline(outer, inner, from, to, back)}"/>"""

  private def outline(outer: Double, inner: Double, from: Double, to: Double, back: Double)
  :   Text =
    def turn(radius: Double): Double = asin(back/radius).double*180/π
    val sweep: Double = ((to - from)%360 + 360)%360
    val large: Text = if sweep - 2*turn(inner) > 180 then t"1" else t"0"
    val o: Text = num(outer)
    val i: Text = num(inner)
    val outerStart: Text = point(outer, from + turn(outer))
    val outerEnd: Text = point(outer, to - turn(outer))
    val innerStart: Text = point(inner, from + turn(inner))
    val innerEnd: Text = point(inner, to - turn(inner))
    val outerArc: Text = t"M $outerStart A $o $o 0 $large 1 $outerEnd"
    t"$outerArc L $innerEnd A $i $i 0 $large 0 $innerStart Z"

  // The two rings of a class or a trait between two angles, sealed: joined across each end by
  // a wall along its radius, as thick as a ring; one shape, whose hole is the space between
  // the rings.
  private def walled(from: Double, to: Double, back: Double): Text =
    val whole: Text = outline(5, 2, from, to, back)
    val space: Text = outline(4, 3, from, to, back + 1)
    t"""<path fill-rule="evenodd" d="$whole $space"/>"""

  // A sealed, dotted end: one shape across both rings, between the radius at an angle and a
  // line a ring's breadth from it, on the side the rings are on (clockwise of it, or not).
  private def wall(at: Double, clockwise: Boolean, back: Double): Text =
    val sign: Double = if clockwise then 1 else -1
    def turn(radius: Double, back: Double): Double = sign*asin(back/radius).double*180/π
    val forth: Text = if clockwise then t"1" else t"0"
    val again: Text = if clockwise then t"0" else t"1"
    val start: Text = t"M ${point(2, at + turn(2, back))} L ${point(5, at + turn(5, back))}"
    val far: Text = t"A 5 5 0 0 $forth ${point(5, at + turn(5, back + 1))}"
    val near: Text = t"L ${point(2, at + turn(2, back + 1))} A 2 2 0 0 $again"
    t"""<path d="$start $far $near ${point(2, at + turn(2, back))} Z"/>"""

  // So many dashes of a ring between two such ends, a gap before the first and after the last.
  private def between(outer: Double, from: Double, to: Double, count: Int, back: Double): Text =
    val turn: Double = asin((back + 1)/(outer - 0.5)).double*180/π
    val step: Double = (((to - from)%360 + 360)%360 - 2*turn)/(2*count + 1)

    List.range(0, count).map: (index: Int) =>
      val start: Double = from + turn + (2*index + 1)*step
      band(outer, outer - 1, start, start + step)

    . join

  // The rings of a class, open to the east: whole or dotted, its inner ring's mouth wider if it
  // is open, its ends walled if it is sealed (which an open one is not).
  private def classRings(dotted: Boolean, open: Boolean, sealing: Boolean, back: Double)
  :   List[Text] =
    if !sealing || open then List(arc(5, 45, 315, dotted, back), core(dotted, open, back))
    else if !dotted then List(walled(45, 315, back))
    else
      List
        ( wall(45, true, back),
          wall(315, false, back),
          between(5, 45, 315, 8, back),
          between(3, 45, 315, 4, back) )

  // A part of a ring as so many dashes, the gaps between them as long as they are, and a dash at
  // each end; `back` draws the ends back from their radii as `inset` does, though radially.
  private def dashes
    ( outer: Double, inner: Double, from: Double, to: Double, count: Int, back: Double = 0 )
  :   Text =
    val turn: Double = asin(2*back/(outer + inner)).double*180/π
    val sweep: Double = ((to - from)%360 + 360)%360 - 2*turn
    val step: Double = sweep/(2*count - 1)

    List.range(0, count).map: (index: Int) =>
      val start: Double = from + turn + 2*index*step
      band(outer, inner, start, start + step)

    . join

  // A ring of a class between two angles: whole, or
  // dotted for an abstract one.
  private def arc(outer: Double, from: Double, to: Double, dotted: Boolean, back: Double): Text =
    if !dotted then inset(outer, outer - 1, from, to, back)
    else dashes(outer, outer - 1, from, to, if outer > 4 then 9 else 5, back)

  private def stroked(d: Text, width: Double): Text =
    val paint: Text = t"""fill="none" stroke="currentColor" stroke-width="${num(width)}""""
    t"""<path d="$d" $paint stroke-linejoin="round"/>"""

  // The radius of the full stop of a `final` one.
  private val stop: Double = 1.02

  private val square: Text = rect(4.5, 4.5, 5, 5)

  // The square of an `update def`, a notch cut from its top as a `var`'s is from a `val`'s
  // triangle, with the slope of that triangle's sides; and of a `consume def`, a semicircle cut
  // from its right-hand side, a quarter as wide again.
  private val notched: Text =
    polygon((4.5, 4.5), (6, 4.5), (7, 6.23), (8, 4.5), (9.5, 4.5), (9.5, 9.5), (4.5, 9.5))

  private val bitten: Text =
    t"""<path d="M 4.5 4.5 H 9.5 V 5.75 A 1.25 1.25 0 0 0 9.5 8.25 V 9.5 H 4.5 Z"/>"""

  private val dot: Text = disc(1)

  // The keyword's drawing, before any modifier.
  // The inner ring of a class: an open one's is only its western half, its
  // mouth twice as wide as the outer ring's.
  private def core(dotted: Boolean, open: Boolean, back: Double): Text =
    if !open then arc(3, 45, 315, dotted, back)
    else if dotted then dashes(3, 2, 90, 270, 3)
    else band(3, 2, 90, 270)

  private def base
    ( kind: Api.Kind.Scala, dotted: Boolean = false, open: Boolean = false,
      sealing: Boolean = false )
  :   Drawing =
    kind match
      case Api.Kind.Package =>
        // A cube, isometrically: a regular hexagon, with three edges meeting at its centre.
        val r: Double = 5.2
        val corners: List[Text] = List(-90, -30, 30, 90, 150, 210).map(point(r, _))
        val outline: Text = t"M ${corners.join(t" L ")} Z"
        val edges: Text = List(-30, 90, 210).map { a => t"M 7 7 L ${point(r, a)}" }.join(t" ")
        Drawing(List(stroked(t"$outline $edges", 1)))

      case Api.Kind.Object    => Drawing(List(ring(5, 3)))
      case Api.Kind.Class =>
        Drawing(classRings(dotted, open, sealing, 0))

      // A class, with the disc of a `case` at its centre, a ring's breadth within its rings.
      case Api.Kind.CaseClass =>
        Drawing(classRings(dotted, open, sealing, 0) + List(dot))

      case Api.Kind.Trait =>
        if sealing then Drawing(List(walled(45, 225, 0)))
        else Drawing(List(band(5, 4, 45, 225), band(3, 2, 45, 225)))


      case Api.Kind.Enum =>
        // Five discs about the centre, half a disc's breadth between each and the next.
        val spread: Double = 3.57
        val discs: List[Text] = List(0, 1, 2, 3, 4).map: (n: Int) =>
          val radians: Double = (n*72 - 90)*π/180
          disc(1.4, c + spread*cos(radians).double, c + spread*sin(radians).double)

        Drawing(discs)

      case Api.Kind.Case      => Drawing(List(disc(2.8)))
      // Four arcs of forty-five degrees, centred on the diagonals.
      case Api.Kind.Type =>
        Drawing(List(45, 135, 225, 315).map { (at: Int) => band(5, 4, at - 22.5, at + 22.5) })

      case Api.Kind.TypeParam => base(Api.Kind.Type)
      case Api.Kind.Def       => Drawing(List(square))
      case Api.Kind.Val       => Drawing(List(polygon((4.08, 4.95), (9.92, 4.95), (7, 10))))

      case Api.Kind.Var =>
        val chevron: Text =
          polygon((6.1, 4.67), (4.08, 4.67), (7, 9.72), (9.92, 4.67), (7.9, 4.67), (7, 6.22))

        Drawing(List(chevron))

      case Api.Kind.Given =>
        Drawing(List(polygon((7, 3.46), (10.54, 7), (7, 10.54), (3.46, 7))))

      case Api.Kind.Extension => Drawing(List(square, rect(10.5, 1.5, 1, 3), rect(9.5, 2.5, 3, 1)))
      case Api.Kind.Param     => Drawing(List(rect(3.5, 9.95, 7, 1.17)))

  // Whether a kind's glyph can be drawn as one with its companion object's.
  def pairs(kind: Api.Kind): Boolean =
    kind == Api.Kind.Class || kind == Api.Kind.CaseClass || kind == Api.Kind.Trait
    || kind == Api.Kind.Type

  // Half the width of the gap between a type's part of a circle and its companion's.
  private val parting: Double = 0.5

  // A type and its companion object as one circle: the type's rings in its own sector, and the
  // object's ring in the rest, each drawn back from the radii where they meet.
  private def paired(kind: Api.Kind.Scala, dotted: Boolean, open: Boolean, sealing: Boolean)
  :   Drawing =
    kind match
      case Api.Kind.Trait =>
        val rings: List[Text] =
          if sealing then List(walled(45, 225, parting))
          else List(inset(5, 4, 45, 225, parting), inset(3, 2, 45, 225, parting))

        Drawing(rings + List(inset(5, 3, 225, 45, parting)))

      // The type's arcs on the diagonals, and the object's ring in the four gaps between them.
      case Api.Kind.Type =>
        val arcs: List[Text] =
          List(45, 135, 225, 315).map { (at: Int) => inset(5, 4, at - 22.5, at + 22.5, parting) }

        val gaps: List[Text] =
          List(0, 90, 180, 270).map { (at: Int) => inset(5, 3, at - 22.5, at + 22.5, parting) }

        Drawing(arcs + gaps)

      case _ =>
        val rings: List[Text] =
          classRings(dotted, open, sealing, parting) + List(inset(5, 3, 315, 45, parting))

        Drawing(if kind == Api.Kind.CaseClass then rings + List(dot) else rings)

  // Where a shape's decorations go: the left and right edges it presents at mid-height, which the
  // infix squares and the `into` triangle stand against, and the centre of its full stop, a
  // stop's own diameter from its lower right.
  private case class Fit(left: Double, right: Double, stopX: Double, stopY: Double)

  private def fit(kind: Api.Kind.Scala): Fit = kind match
    case Api.Kind.Package                          => Fit(2, 12, 12.7, 12.7)
    case Api.Kind.Object | Api.Kind.Class          => Fit(2, 12, 12.7, 12.7)
    case Api.Kind.CaseClass                        => Fit(2, 12, 12.7, 12.7)
    case Api.Kind.Trait                            => Fit(2, 12, 12.7, 12.7)
    case Api.Kind.Enum                             => Fit(2.2, 11.8, 12.96, 12.12)
    case Api.Kind.Case                             => Fit(4.2, 9.8, 11.14, 11.14)
    case Api.Kind.Type | Api.Kind.TypeParam        => Fit(2, 12, 12.7, 12.7)
    case Api.Kind.Def | Api.Kind.Extension         => Fit(4.5, 9.5, 12.56, 8.48)
    case Api.Kind.Val                              => Fit(4.83, 9.17, 11.13, 8.98)
    case Api.Kind.Var                              => Fit(5, 9, 11.13, 8.7)
    case Api.Kind.Given                            => Fit(3.46, 10.54, 10.93, 10.93)
    case Api.Kind.Param                            => Fit(3.5, 10.5, 13.56, 10.1)

  // The drawing with its modifiers: the shapes of their own first, then the transformations of
  // the whole, then what stands beside it, placed for the shape.
  private def drawing(kind: Api.Kind.Scala, modifiers: List[Text], companion: Boolean = false)
  :   Drawing =
    def has(modifier: Text): Boolean = modifiers.has(modifier)

    // The square of a def, or of an extension, with what an `update` or a `consume` cuts from it.
    val body: Optional[Text] =
      if kind != Api.Kind.Def && kind != Api.Kind.Extension then Unset
      else if has(t"update") then notched
      else if has(t"consume") then bitten
      else Unset

    val shaped: Drawing =
      if companion then
        val pair: Drawing = paired(kind, has(t"abstract"), has(t"open"), has(t"sealed"))
        if kind == Api.Kind.Type && has(t"opaque") then pair.add(ring(2, 1)) else pair
      else if kind == Api.Kind.Def && has(t"abstract") then
        // The square cut five by five, its second and fourth rows and columns taken away.
        val places: List[Double] = List(4.5, 6.5, 8.5)

        val squares: List[Text] = places.bind: (y: Double) =>
          places.map { (x: Double) => t"M ${num(x)} ${num(y)} h 1 v 1 h -1 Z" }

        Drawing(List(t"""<path d="${squares.join(t" ")}"/>"""))
      else if (kind == Api.Kind.Type || kind == Api.Kind.TypeParam) && has(t"opaque") then
        base(kind).add(ring(2, 1))
      else
        body.lay(base(kind, has(t"abstract"), has(t"open"), has(t"sealed"))): (cut: Text) =>
          val drawn: Drawing = base(kind)
          drawn.copy(elements = cut :: drawn.elements.skip(1))

    val sheared: Drawing =
      if has(t"inline")
      then shaped.within(t"""<g transform="translate(7 0) skewX(-14) translate(-7 0)">""", t"</g>")
      else shaped

    // Only the outline of each shape, as if it were seen through; what is added after it (a
    // slash, an underline, a stop) is drawn solid.
    val outlined: Drawing =
      if !has(t"transparent") then sheared else
        val paint: Text = t"""fill="none" stroke="currentColor" stroke-width="0.3""""
        sheared.within(t"<g $paint>", t"</g>")

    // A slash is drawn over the glyph, with a gap cut beneath it so that it shows on a filled
    // shape, as a stroke lifts off the page.
    val slashed: Drawing =
      if !has(t"erased") then outlined else
        val gap: Text =
          t"""<defs><mask id="fuel-slashed"><rect width="100%" height="100%" fill="#fff"/>""" +
            t"""<rect x="6.6" y="0.6" width="1.6" height="12.8" fill="#000" """ +
            t"""transform="rotate(45 7 7)"/></mask></defs>"""

        val slash: Text =
          t"""<rect x="6.6" y="0.6" width="0.8" height="12.8" transform="rotate(45 7 7)"/>"""

        outlined.within(t"""$gap<g mask="url(#fuel-slashed)">""", t"</g>").add(slash)

    val underlined: Drawing = if has(t"override") then slashed.add(rect(3, 12.6, 8, 1)) else slashed

    // A shelter over the glyph: a wide circumflex, as thick as a ring.
    val sheltered: Drawing =
      if !has(t"protected") then underlined else
        val roof: Text =
          polygon((3.5, 1.14), (7, 0), (10.5, 1.14), (10.5, 2.14), (7, 1), (3.5, 2.14))

        underlined.add(roof)

    // A shear pivots on the top of the grid, so what stands beside a sheared shape moves with it.
    def lean(y: Double): Double = if has(t"inline") then -0.25*y else 0
    val shape: Fit = if companion then fit(Api.Kind.Object) else fit(kind)
    val gap: Double = 0.5
    val small: Double = 1.5
    val mid: Double = lean(c)
    val leftOfSquare: Double = shape.left - gap - small + mid
    val rightOfSquare: Double = shape.right + gap + mid

    val placed: Drawing =
      if !has(t"infix") then sheltered else
        sheltered
        . add(rect(leftOfSquare, 6.25, small, small), rect(rightOfSquare, 6.25, small, small))
        . reach(leftOfSquare, rightOfSquare + small)

    // Narrow, and two and a half times as tall as it is wide, outside any infix square.
    val converting: Drawing =
      if !has(t"into") then placed else
        val edge: Double = if has(t"infix") then leftOfSquare - gap else shape.left - gap + mid
        val tip: Text = polygon((edge - 2, 4.5), (edge, 7), (edge - 2, 9.5))
        placed.add(tip).reach(edge - 2, grid)

    val annotated: Drawing =
      if !has(t"annotation") then converting else
        val x: Double = 13.1
        val y: Double = 2.5
        val rim: Text = num(x + 0.8)
        val eye: Text = t"M $rim ${num(y + 0.2)} A 0.8 0.8 0 1 1 $rim ${num(y - 0.2)}"
        val outer: Text = t"A 2 2 0 1 0 ${num(x + 1.1)} ${num(y + 1.7)}"
        val at: Text = t"$eye V ${num(y + 0.4)} A 0.5 0.5 0 0 0 ${num(x + 2)} ${num(y)} $outer"
        converting.add(stroked(at, 0.6)).reach(0, x + 2.4)

    val java: Drawing =
      if !has(t"java") || kind == Api.Kind.Package || kind == Api.Kind.Param then annotated
      else annotated.add(stroked(t"M 2.6 0.4 V 3 A 1.05 1.05 0 0 1 0.5 3", 0.8))

    // How many times a member of a TEL schema may be said, in the top right corner, as a
    // grammar writes it: `?` for one which may be left out, `+` for one said once or more, and
    // `*` for one which is both.
    val counted: Drawing =
      val x: Double = 13.3
      val y: Double = 2.5
      val r: Double = 1.4

      def spoke(degrees: Double): Text =
        val radians: Double = degrees*π/180
        val dx: Double = r*cos(radians).double
        val dy: Double = r*sin(radians).double
        t"M ${num(x - dx)} ${num(y - dy)} L ${num(x + dx)} ${num(y + dy)}"

      def spokes(angles: Double*): Text = stroked(angles.map(spoke).join(t" "), 0.6)

      if has(t"optional") && has(t"repeatable") then
        java.add(spokes(90, 30, 150)).reach(0, x + r + 0.4)
      else if has(t"repeatable") then
        java.add(spokes(0, 90)).reach(0, x + r + 0.4)
      else if has(t"optional") then
        val hook: Text =
          t"M ${num(x - 0.95)} ${num(y - 0.75)} A 0.95 0.95 0 1 1 ${num(x)} ${num(y + 0.2)} " +
            t"V ${num(y + 0.7)}"

        java.add(stroked(hook, 0.6), disc(0.4, x, y + 1.5)).reach(0, x + r + 0.4)
      else
        java

    // A key, in the top left corner: its bow, and its shaft hanging down the left-hand side, clear
    // of the glyph, its two teeth pointing away from it.
    val marked: Drawing =
      if !has(t"key") then counted else
        val bow: Text = t"M 0.4 3.2 A 1 1 0 1 1 0.4 1.2 A 1 1 0 1 1 0.4 3.2"
        val shaft: Text = t"M 0.4 3.2 V 6.2 H -0.7 M 0.4 5.1 H -0.5"
        counted.add(stroked(t"$bow $shaft", 0.6)).reach(-1, grid)

    // An ellipsis, at the lower left: beside the foot of the shape, its last dot nearest it.
    val pending: Drawing =
      if !has(t"lazy") || kind == Api.Kind.Object || companion then marked else
        val place: (Double, Double) = kind match
          case Api.Kind.Val                      => (5.1, 9.55)
          case Api.Kind.Var                      => (5.1, 9.27)
          case Api.Kind.Given                    => (4.6, 10.09)
          case Api.Kind.Def | Api.Kind.Extension => (3.55, 9.05)
          case _                                 => (15.49 - shape.stopX, shape.stopY - 1.49)

        val y: Double = place(1)
        val x: Double = place(0) + lean(y)
        val dots: List[Text] = List(2.4, 1.2, 0.0).map { (back: Double) => disc(0.45, x - back, y) }
        marked.add(dots*).reach(x - 2.85, grid)

    if !has(t"final") then pending else
      // Below the right infix square, where there is one and the stop would otherwise touch it.
      val y: Double = if has(t"infix") then shape.stopY.max(9.35) else shape.stopY
      val x: Double = shape.stopX + lean(y)
      pending.add(disc(stop, x, y)).reach(x - stop, x + stop)

  // What a declaration is, in the words which declare it: its written modifiers and its keyword,
  // or `export` for an export.
  def keywords(declaration: Api.Declaration, exported: Boolean = false): Text =
    if exported then t"export" else
      val word: Text =
        if declaration.kind == Api.Kind.Extension then t"extension def"
        else declaration.kind.keyword

      (declaration.written + List(word)).join(t" ")

  // The glyph for a declaration, as an icon described by its keywords, its tooltip on the web.
  def of(declaration: Api.Declaration, exported: Boolean = false): Inline =
    Inline.Icon(markup(declaration.kind, declaration.modifiers), keywords(declaration, exported))

  // The one glyph of a type and its companion object, described by the keywords of both; unset
  // unless they are a pair which is drawn as one.
  def of(template: Api.Declaration, companion: Api.Declaration, exported: Boolean)
  :   Optional[Inline] =
    template.kind match
      case kind: Api.Kind.Scala if pairs(kind) && companion.kind == Api.Kind.Object =>
        val modifiers: List[Text] = (template.modifiers + companion.modifiers).distinct
        val words: Text = t"${keywords(template, exported)} ${keywords(companion, exported)}"
        Inline.Icon(drawing(kind, modifiers, true).markup, words)

      case _ =>
        Unset

  // The markup of that glyph alone.
  def markup(kind: Api.Kind.Scala, modifiers: List[Text], companion: Boolean): Text =
    drawing(kind, modifiers, companion).markup

  // The markup alone: of a Scala kind, these being Scala's glyphs; any other kind has none here.
  def markup(kind: Api.Kind, modifiers: List[Text]): Text = kind match
    case kind: Api.Kind.Scala => drawing(kind, modifiers).markup
    case _                    => t""
