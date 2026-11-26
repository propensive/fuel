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
// drawing: generically where it can — `final` adds a pilcrow beside it, `inline` shears it,
// `transparent` draws only its outline, as if it were seen through — and by a shape of its own
// where the drawings differ: an abstract `def` is nine small squares, an opaque type a circle,
// an enum case is a disc.
//
// Every modifier fuel records, and what the glyph does with it:
//
//   final         a pilcrow at the glyph's lower right, on the baseline
//   inline        sheared to the right
//   transparent   only the outline of each shape
//   abstract      dotted: a def's square is nine small squares, and a given's diamond; a val's
//                 triangle six small triangles, a var's five; a class's rings hatched with
//                 radial lines as thin as an outline; nothing on anything else. A member
//                 without a body is abstract, though it is not written so
//   opaque        type: enclosed in a thin circle a little beyond an object's ring
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
//   kotlin        a `K` there instead, on what was compiled from Kotlin
//   implicit      nothing: it is deprecated
//   open          class: the inner ring only its western half, its mouth twice as wide
//   optional      a small `?` in the top right corner (a TEL schema's member); with
//                 `repeatable`, a small `*` there instead
//   repeatable    a small `+` in the top right corner: one or more; with `optional`, the `*`
//   key           a small key in the top left corner, hanging down the left-hand side
//   update        def: a notch cut from the top of the square, as a var's from a val's triangle
//   consume       def: a semicircle cut from the right-hand side of the square
//   extension     a def with a small plus at the top right
//   enum case     a disc
//   tracked       val: crossed by a vertical and a horizontal line through its centre
//
// A class is three quarters of two rings, open to the east, and a trait half of the same rings,
// from the south-east to the north-west. A type is four arcs of the outer ring, each an eighth of
// it, centred on the diagonals. A class, a case class, a trait or a type with a companion object
// is one glyph, a whole circle: its own part, and the object's ring in the sectors it leaves
// open, with a gap between them along each radius where they meet (to the north-east and
// south-east of a class, to the north-west and south-east of a trait, on either side of each
// of a type's arcs), as if a line were drawn there in the page's colour. An enum is five discs
// on the middle of an object's ring; with a companion, that ring threads them, cut back from
// each disc by the same gap.
// A case class is a class with the disc of a `case` at its centre, small enough to stand a
// ring's breadth within its rings. The glyph of a pair has the modifiers of both.
object Icons:
  private val grid: Int = 14
  private val c: Double = 7.0

  // A drawing: its elements, on the grid, whatever stands beside it. Every glyph has the same box,
  // so that it is drawn the same size, in the same place, and on the same baseline, whatever its
  // modifiers; a decoration beyond the box is drawn over the space about it, as overflow.
  private case class Drawing(elements: List[Text]):
    def add(more: Text*): Drawing = copy(elements = elements + more)

    def within(open: Text, close: Text): Drawing =
      copy(elements = List(open) + elements + List(close))

    def markup: Text =
      val box: Text = t"""viewBox="0 0 ${grid.show} ${grid.show}" overflow="visible""""
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

  // The rings of a sealed abstract class, hatched, their ends closed by one line each, which
  // both rings share and which spans the space between them: a line parallel to the radius at
  // each end, its edge on the end, and the rings' own lines spaced evenly between the two.
  private def sealedHatch(back: Double): Text =
    val inset: Double = back + 0.15
    def angle(radius: Double): Double = asin(inset/radius).double*180/π

    def ends(at: Double, sign: Double): Text =
      t"M ${point(2, at + sign*angle(2))} L ${point(5, at + sign*angle(5))}"

    def ring(outer: Double): List[Text] =
      val middle: Double = outer - 0.5
      val from: Double = 45 + angle(middle)
      val sweep: Double = 270 - 2*angle(middle)
      val count: Int = (sweep*π/180*middle/pitch).round.toInt.max(2)

      List.range(1, count).map: (index: Int) =>
        val at: Double = from + index*sweep/count
        t"M ${point(outer - 1, at)} L ${point(outer, at)}"

    val lines: Text = (List(ends(45, 1), ends(315, -1)) + ring(5) + ring(3)).join(t" ")
    t"""<path d="$lines" fill="none" stroke="currentColor" stroke-width="0.3"/>"""

  // The rings of a class, open to the east: whole or dotted, its inner ring's mouth wider if it
  // is open, its ends walled if it is sealed (which an open one is not).
  private def classRings(dotted: Boolean, open: Boolean, sealing: Boolean, back: Double)
  :   List[Text] =
    if !sealing || open then List(arc(5, 45, 315, dotted, back), core(dotted, open, back))
    else if !dotted then List(walled(45, 315, back))
    else List(sealedHatch(back))

  // How far apart the lines of a hatched ring are, at its middle.
  private val pitch: Double = 0.59

  // A part of a ring hatched with radial lines as thin as an outline, `pitch` apart at the middle
  // of the ring, the first and last half a step in from the ends; `back` draws
  // the ends back from their radii as `inset` does, though radially.
  private def hatch(outer: Double, inner: Double, from: Double, to: Double, back: Double = 0)
  :   Text =
    val middle: Double = (outer + inner)/2
    val turn: Double = asin(back/middle).double*180/π
    val sweep: Double = ((to - from)%360 + 360)%360 - 2*turn
    val count: Int = (sweep*π/180*middle/pitch).round.toInt.max(1)
    val step: Double = sweep/count

    val lines: Text = List.range(0, count).map: (index: Int) =>
      val at: Double = from + turn + (index + 0.5)*step
      t"M ${point(inner, at)} L ${point(outer, at)}"

    . join(t" ")

    t"""<path d="$lines" fill="none" stroke="currentColor" stroke-width="0.3"/>"""

  // A ring of a class between two angles: whole, or hatched for an abstract one.
  private def arc(outer: Double, from: Double, to: Double, dotted: Boolean, back: Double): Text =
    if !dotted then inset(outer, outer - 1, from, to, back)
    else hatch(outer, outer - 1, from, to, back)

  private def stroked(d: Text, width: Double): Text =
    val paint: Text = t"""fill="none" stroke="currentColor" stroke-width="${num(width)}""""
    t"""<path d="$d" $paint stroke-linejoin="round"/>"""

  // The text's baseline, where a glyph held at the height of the text, a quarter of a unit in
  // fourteen below it, meets the line: what stands on it, stands on it whatever the shape.
  private val baseline: Double = 12.25

  private val square: Text = rect(4.5, 4.5, 5, 5)

  // A tracked val's triangle, crossed by a vertical and a horizontal line in the page's colour
  // through its centre: four pieces, half a unit apart.
  private val crosshairs: List[Text] =
    val top: Double = 4.95
    val bottom: Double = 10
    val half: Double = 0.25
    val centre: Double = top + (bottom - top)/3
    val upper: Double = centre - half
    val lower: Double = centre + half
    def edge(y: Double): Double = 4.08 + (y - top)*2.92/(bottom - top)
    def at(x: Double): Double = top + (x - 4.08)*(bottom - top)/2.92

    def mirrored(points: (Double, Double)*): List[Text] =
      List(polygon(points*), polygon(points.map { (x, y) => (14 - x, y) }*))

    mirrored((4.08, top), (7 - half, top), (7 - half, upper), (edge(upper), upper))
    + mirrored((edge(lower), lower), (7 - half, lower), (7 - half, at(7 - half)))

  // An extension's plus, one shape, so that it is one outline when it is transparent.
  private val plus: Text =
    polygon
      ( (10.5, 1.5), (11.5, 1.5), (11.5, 2.5), (12.5, 2.5), (12.5, 3.5), (11.5, 3.5),
        (11.5, 4.5), (10.5, 4.5), (10.5, 3.5), (9.5, 3.5), (9.5, 2.5), (10.5, 2.5) )

  // The nine small squares of an abstract def's square: cut five by five, its second and fourth
  // rows and columns taken away; turned a quarter about the centre, an abstract given's.
  private val nine: Text =
    val places: List[Double] = List(4.5, 6.5, 8.5)

    val squares: List[Text] = places.bind: (y: Double) =>
      places.map { (x: Double) => t"M ${num(x)} ${num(y)} h 1 v 1 h -1 Z" }

    t"""<path d="${squares.join(t" ")}"/>"""

  // A triangle's six small triangles, as a square's nine: the triangle cut five by five along
  // its sides into twenty-five, of which those at even places along both sides are kept, all
  // pointing as it does; an abstract val's, and, without the one its notch takes, a var's.
  private def pyramid
    ( a: (Double, Double), b: (Double, Double), c: (Double, Double), notched: Boolean )
  :   Text =
    def at(i: Int, j: Int): Text =
      t"${num(a(0) + (i*(b(0) - a(0)) + j*(c(0) - a(0)))/5)} " +
        t"${num(a(1) + (i*(b(1) - a(1)) + j*(c(1) - a(1)))/5)}"

    val kept: List[(Int, Int)] =
      List((0, 0), (2, 0), (4, 0), (0, 2), (2, 2), (0, 4)).filter: (i, j) =>
        !notched || (i, j) != (2, 0)

    val small: List[Text] = kept.map: (i, j) =>
      t"M ${at(i, j)} L ${at(i + 1, j)} L ${at(i, j + 1)} Z"

    t"""<path d="${small.join(t" ")}"/>"""

  // The letters which say what compiled a member, Java or Kotlin, and the `@` of an annotation,
  // as JetBrains Mono's extra-bold draws them, a capital 3.6 units tall, each drawn from the top
  // left corner of its bounds: the widths of the letters, and the outlines.
  //
  // JetBrains Mono: Copyright 2020 The JetBrains Mono Project Authors
  // (https://github.com/JetBrains/JetBrainsMono), licensed under the SIL Open Font License,
  // Version 1.1 (https://scripts.sil.org/OFL).
  private val javaWidth: Double = 2.47
  private val kotlinWidth: Double = 2.52

  private val javaJ: Text =
    t"M 1.23 3.65 Q 0.67 3.65 0.33 3.34 Q 0 3.03 0 2.51 L 0.74 2.51 Q 0.74 2.74 0.87 2.87 " +
    t"Q 1.01 3.01 1.23 3.01 Q 1.46 3.01 1.59 2.88 Q 1.73 2.75 1.73 2.52 L 1.73 0 L 2.47 0 " +
    t"L 2.47 2.52 Q 2.47 3.04 2.14 3.35 Q 1.8 3.65 1.23 3.65 Z"

  private val kotlinK: Text =
    t"M 0 3.6 L 0 0 L 0.74 0 L 0.74 1.41 L 1.08 1.41 L 1.69 0 L 2.49 0 L 1.74 1.7 " +
    t"L 2.52 3.6 L 1.71 3.6 L 1.09 2.06 L 0.74 2.06 L 0.74 3.6 Z"

  private val annotationAt: Text =
    t"M 1.48 4.54 Q 1.02 4.54 0.69 4.36 Q 0.36 4.19 0.18 3.87 Q 0 3.55 0 3.1 L 0 1.43 " +
    t"Q 0 0.98 0.17 0.66 Q 0.34 0.34 0.66 0.17 Q 0.99 0 1.43 0 Q 1.84 0 2.13 0.15 " +
    t"Q 2.43 0.31 2.58 0.57 Q 2.74 0.84 2.74 1.18 L 2.74 2.52 Q 2.74 2.91 2.5 3.15 " +
    t"Q 2.26 3.38 1.85 3.38 Q 1.45 3.38 1.22 3.15 Q 0.99 2.91 0.99 2.52 L 0.99 2.02 " +
    t"Q 0.99 1.65 1.16 1.43 Q 1.33 1.21 1.62 1.21 Q 1.87 1.21 2.01 1.33 " +
    t"Q 2.15 1.46 2.15 1.68 L 2.07 1.53 L 2.15 1.53 L 2.15 1.18 Q 2.15 0.86 1.95 0.69 " +
    t"Q 1.76 0.52 1.4 0.52 Q 1.02 0.52 0.81 0.75 Q 0.59 0.99 0.59 1.43 L 0.59 3.1 " +
    t"Q 0.59 3.53 0.82 3.76 Q 1.05 3.99 1.48 3.99 L 2.02 3.99 L 2.02 4.54 Z M 1.85 2.91 " +
    t"Q 1.99 2.91 2.07 2.81 Q 2.15 2.7 2.15 2.52 L 2.15 2.02 Q 2.15 1.83 2.07 1.73 " +
    t"Q 1.99 1.63 1.85 1.63 Q 1.71 1.63 1.63 1.73 Q 1.55 1.83 1.55 2.02 L 1.55 2.52 " +
    t"Q 1.55 2.7 1.63 2.81 Q 1.71 2.91 1.85 2.91 Z"

  // The pilcrow of a `final` one, smaller, in the same face, drawn from the top left corner of
  // its bounds.
  private val pilcrow: Text =
    t"M 0.61 2.9 L 0.61 1.34 Q 0.33 1.34 0.17 1.19 Q 0 1.04 0 0.8 L 0 0.54 " +
    t"Q 0 0.3 0.17 0.15 Q 0.33 0 0.61 0 L 0.96 0 L 0.96 2.9 Z M 1.18 2.9 L 1.18 0 L 1.53 0 " +
    t"L 1.53 2.9 Z"

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
    else if dotted then hatch(3, 2, 90, 270)
    else band(3, 2, 90, 270)

  private def base
    ( kind: Api.Kind.Scala, dotted: Boolean = false, open: Boolean = false,
      sealing: Boolean = false )
  :   Drawing =
    kind match
      case Api.Kind.Package =>
        // A cube, isometrically: its three faces, the rhombi of a regular hexagon whose corners
        // are 5.2 from the centre, a unit apart along the edges they share.
        val top: Text = polygon((7, 6.42), (3, 4.11), (7, 1.8), (11, 4.11))
        val right: Text = polygon((7.5, 7.29), (11.5, 4.98), (11.5, 9.6), (7.5, 11.91))
        val left: Text = polygon((6.5, 7.29), (6.5, 11.91), (2.5, 9.6), (2.5, 4.98))
        Drawing(List(top, right, left))

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
        Drawing(cases)

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

      case Api.Kind.Extension => Drawing(List(square, plus))
      case Api.Kind.Param     => Drawing(List(rect(3.5, 9.95, 7, 1.17)))

  // The angles of an enum's five discs, the first at the top.
  private val pentagon: List[Double] = List(0, 1, 2, 3, 4).map { (n: Int) => n*72.0 - 90 }

  // An enum's five discs, centred on the middle of an object's ring, so that they stand where
  // they are whether its companion's ring is drawn about them or not.
  private val cases: List[Text] = pentagon.map: (at: Double) =>
    val radians: Double = at*π/180
    disc(1, c + 4*cos(radians).double, c + 4*sin(radians).double)

  // An object's ring, threaded with an enum's discs: five pieces of it, each cut back from the
  // discs either side of it by a circle half a ring's breadth wider than a disc.
  private val threaded: List[Text] =
    val cut: Double = 1.5

    // Where the cut meets a circle of the radius, in degrees from the disc's own angle.
    def meet(radius: Double): Double =
      acos((radius*radius + 16 - cut*cut)/(8*radius)).double*180/π

    pentagon.map: (at: Double) =>
      val next: Double = at + 72
      val start: Text = point(5, at + meet(5))
      val outer: Text = t"A 5 5 0 0 1 ${point(5, next - meet(5))}"
      val inward: Text = t"A ${num(cut)} ${num(cut)} 0 0 0 ${point(3, next - meet(3))}"
      val inner: Text = t"A 3 3 0 0 0 ${point(3, at + meet(3))}"
      val outward: Text = t"A ${num(cut)} ${num(cut)} 0 0 0 $start"
      t"""<path d="M $start $outer $inward $inner $outward Z"/>"""

  // Whether a kind's glyph can be drawn as one with its companion object's.
  def pairs(kind: Api.Kind): Boolean =
    kind == Api.Kind.Class || kind == Api.Kind.CaseClass || kind == Api.Kind.Trait
    || kind == Api.Kind.Type || kind == Api.Kind.Enum

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

      case Api.Kind.Enum =>
        Drawing(threaded + cases)

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
  // infix squares and the `into` triangle stand against; and where those which stand a gap of
  // 1.4 units from the nearest part of it go: the right edge of the `J` or `K` in the top left
  // corner, the left edge of the `@` in the top right, and on the baseline, the left edge of a
  // `final` one's pilcrow and the right edge of a `lazy` one's ellipsis.
  private case class Fit
    ( left: Double, right: Double, letter: Double = 0, at: Double = 14, stop: Double = 14,
      lazily: Double = 0 )

  private def fit(kind: Api.Kind.Scala): Fit = kind match
    case Api.Kind.Package                    => Fit(2, 12)
    case Api.Kind.Object                     => Fit(2, 12, 1.38, 13.02, 12.68, 2)
    case Api.Kind.Class                      => Fit(2, 12, 1.38, 11.92, 12, 2)
    case Api.Kind.CaseClass                  => Fit(2, 12, 1.38, 11.92, 12, 2)
    case Api.Kind.Trait                      => Fit(2, 12, 1.38, 6.76, 12, 2)
    case Api.Kind.Enum                       => Fit(2.2, 11.8, 1.63, 13, 11.8, 2.2)
    case Api.Kind.Case                       => Fit(4.2, 9.8, 4.15, 10.6, 9.98, 4.2)
    case Api.Kind.Type | Api.Kind.TypeParam  => Fit(2, 12, 1.38, 12.98, 12.68, 2)
    case Api.Kind.Def                        => Fit(4.5, 9.5, 3.24, 10.88, 10.81, 4.5)
    case Api.Kind.Extension                  => Fit(4.5, 9.5, 3.24, 13.88, 10.81, 4.5)
    case Api.Kind.Val                        => Fit(4.83, 9.17, 3.15, 11.27, 9.17, 4.83)
    case Api.Kind.Var                        => Fit(5, 9, 2.93, 11.28, 9, 5)
    case Api.Kind.Given                      => Fit(3.46, 10.54, 4.55, 10.34, 10.54, 3.46)
    case Api.Kind.Param                      => Fit(3.5, 10.5)

  // The drawing with its modifiers: the shapes of their own first, then the transformations of
  // the whole, then what stands beside it, placed for the shape.
  private def drawing
    ( kind: Api.Kind.Scala, modifiers: List[Text], partner: Optional[Api.Kind.Scala] = Unset )
  :   Drawing =
    def has(modifier: Text): Boolean = modifiers.has(modifier)

    // Whether it is drawn as one with its companion object; with a val or var of the same
    // name, it is drawn about it instead.
    val companion: Boolean = partner == Api.Kind.Object

    // The square of a def, or of an extension, with what an `update` or a `consume` cuts from it.
    val body: Optional[Text] =
      if kind != Api.Kind.Def && kind != Api.Kind.Extension then Unset
      else if has(t"update") then notched
      else if has(t"consume") then bitten
      else Unset

    // An opaque type is enclosed: a circle about it, as thin as an outline, a little beyond an
    // object's ring.
    val opaque: Boolean =
      (kind == Api.Kind.Type || kind == Api.Kind.TypeParam) && has(t"opaque")

    val enclosure: Text =
      t"""<circle cx="7" cy="7" r="5.75" fill="none" stroke="currentColor" stroke-width="0.3"/>"""

    val formed: Drawing =
      if kind == Api.Kind.Type && (partner == Api.Kind.Val || partner == Api.Kind.Var) then
        // The type's arcs about a val or var two thirds its size, centred within them.
        val small: Text =
          if partner == Api.Kind.Val then polygon((5.01, 5.85), (8.99, 5.85), (7, 9.3))
          else polygon((6.39, 5.85), (5.01, 5.85), (7, 9.3), (8.99, 5.85), (7.61, 5.85), (7, 6.91))

        base(kind).add(small)
      else if companion then
        paired(kind, has(t"abstract"), has(t"open"), has(t"sealed"))
      else if kind == Api.Kind.Def && has(t"abstract") then Drawing(List(nine))
      else if kind == Api.Kind.Extension && has(t"abstract") then Drawing(List(nine, plus))
      else if kind == Api.Kind.Given && has(t"abstract") then
        Drawing(List(t"""<g transform="rotate(45 7 7)">$nine</g>"""))
      else if kind == Api.Kind.Val && has(t"tracked") && !has(t"abstract") then
        Drawing(crosshairs)
      else if kind == Api.Kind.Val && has(t"abstract") then
        Drawing(List(pyramid((4.08, 4.95), (9.92, 4.95), (7, 10), false)))
      else if kind == Api.Kind.Var && has(t"abstract") then
        Drawing(List(pyramid((4.08, 4.67), (9.92, 4.67), (7, 9.72), true)))
      else
        body.lay(base(kind, has(t"abstract"), has(t"open"), has(t"sealed"))): (cut: Text) =>
          val drawn: Drawing = base(kind)
          drawn.copy(elements = cut :: drawn.elements.skip(1))

    val shaped: Drawing = if opaque then formed.add(enclosure) else formed

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

    // The underline of an `override` one, from 3 to 11 across; below an opaque type's circle.
    val underlined: Drawing =
      if !has(t"override") then slashed
      else slashed.add(rect(3, if opaque then 13.3 else 12.6, 8, 1))

    // A shelter over the glyph: a wide circumflex, as thick as a ring.
    val sheltered: Drawing =
      if !has(t"protected") then underlined else
        // Narrower over an extension, so as to clear its plus.
        val half: Double = if kind == Api.Kind.Extension then 3 else 3.5
        val rise: Double = half*1.14/3.5

        val roof: Text =
          polygon
            ( (7 - half, rise), (7, 0), (7 + half, rise), (7 + half, rise + 1), (7, 1),
              (7 - half, rise + 1) )

        // Above an opaque type's circle.
        if opaque then underlined.add(t"""<g transform="translate(0 -0.6)">$roof</g>""")
        else underlined.add(roof)

    // A shear pivots on the top of the grid, so what stands beside a sheared shape moves with it.
    def lean(y: Double): Double = if has(t"inline") then -0.25*y else 0
    // An opaque type's circle presents its edges at mid-height.
    val shape: Fit =
      if companion then fit(Api.Kind.Object)
      else if opaque then fit(kind).copy(left = 1.1, right = 12.9)
      else fit(kind)
    val gap: Double = 0.5
    val small: Double = 1.5
    val mid: Double = lean(c)
    val leftOfSquare: Double = shape.left - gap - small + mid
    val rightOfSquare: Double = shape.right + gap + mid

    val placed: Drawing =
      if !has(t"infix") then sheltered else
        val squares: List[Text] = List(leftOfSquare, rightOfSquare).map(rect(_, 6.25, small, small))
        sheltered.add(squares*)

    // Narrow, and two and a half times as tall as it is wide, outside any infix square.
    val converting: Drawing =
      if !has(t"into") then placed else
        val edge: Double = if has(t"infix") then leftOfSquare - gap else shape.left - gap + mid
        val tip: Text = polygon((edge - 2, 4.5), (edge, 7), (edge - 2, 9.5))
        placed.add(tip)

    // A letter, its outline drawn from the top left corner of its bounds, placed with that corner
    // at a point.
    def letter(outline: Text, x: Double): Text =
      t"""<path transform="translate(${num(x)} 0.3)" d="$outline"/>"""

    val annotated: Drawing =
      if !has(t"annotation") then converting
      else converting.add(letter(annotationAt, shape.at))

    val java: Drawing =
      if kind == Api.Kind.Package || kind == Api.Kind.Param then annotated
      else if has(t"java") then annotated.add(letter(javaJ, shape.letter - javaWidth))
      else if has(t"kotlin") then annotated.add(letter(kotlinK, shape.letter - kotlinWidth))
      else annotated

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
        java.add(spokes(90, 30, 150))
      else if has(t"repeatable") then
        java.add(spokes(0, 90))
      else if has(t"optional") then
        val hook: Text =
          t"M ${num(x - 0.95)} ${num(y - 0.75)} A 0.95 0.95 0 1 1 ${num(x)} ${num(y + 0.2)} " +
            t"V ${num(y + 0.7)}"

        java.add(stroked(hook, 0.6), disc(0.4, x, y + 1.5))
      else
        java

    // A key, in the top left corner: its bow, and its shaft hanging down the left-hand side, clear
    // of the glyph, its two teeth pointing away from it.
    val marked: Drawing =
      if !has(t"key") then counted else
        val bow: Text = t"M 0.4 3.2 A 1 1 0 1 1 0.4 1.2 A 1 1 0 1 1 0.4 3.2"
        val shaft: Text = t"M 0.4 3.2 V 6.2 H -0.7 M 0.4 5.1 H -0.5"
        counted.add(stroked(t"$bow $shaft", 0.6))

    // An ellipsis, on the baseline at the lower left.
    val pending: Drawing =
      if !has(t"lazy") || kind == Api.Kind.Object || companion then marked else
        val x: Double = shape.lazily + lean(baseline)
        val dots: List[Text] = List(2.85, 1.65, 0.45).map(x - _).map(disc(0.45, _, baseline - 0.45))
        marked.add(dots*)

    // A pilcrow, on the baseline at the lower right.
    if !has(t"final") then pending else
      // Clear of an underline, which reaches 11 across, by half a unit.
      val stop: Double = if has(t"override") then shape.stop.max(11.5) else shape.stop
      val x: Double = stop + lean(baseline)
      val top: Double = baseline - 2.33
      val paint: Text = t"""stroke="currentColor" stroke-width="0.12" stroke-linejoin="round""""
      val place: Text = t"translate(${num(x)} ${num(top)})"
      val mark: Text = t"""<path transform="$place" $paint d="$pilcrow"/>"""
      pending.add(mark)

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

  // The one glyph of a type and its companion object, or of a type and a val or var of its name,
  // described by the keywords of both; unset unless they are a pair which is drawn as one.
  def of(template: Api.Declaration, companion: Api.Declaration, exported: Boolean)
  :   Optional[Inline] =
    val typal: Boolean = template.kind == Api.Kind.Type

    val partner: Optional[Api.Kind.Scala] = companion.kind match
      case Api.Kind.Object         => Api.Kind.Object
      case Api.Kind.Val if typal   => Api.Kind.Val
      case Api.Kind.Var if typal   => Api.Kind.Var
      case _                       => Unset

    template.kind match
      case kind: Api.Kind.Scala if partner.present && (pairs(kind) || typal) =>
        val modifiers: List[Text] = (template.modifiers + companion.modifiers).distinct
        val words: Text = t"${keywords(template, exported)} ${keywords(companion, exported)}"
        Inline.Icon(drawing(kind, modifiers, partner).markup, words)

      case _ =>
        Unset

  // The markup of that glyph alone: with a companion object, or with a val or var.
  def markup(kind: Api.Kind.Scala, modifiers: List[Text], companion: Boolean): Text =
    drawing(kind, modifiers, if companion then Api.Kind.Object else Unset).markup

  def markup(kind: Api.Kind.Scala, modifiers: List[Text], partner: Api.Kind.Scala): Text =
    drawing(kind, modifiers, partner).markup

  // The markup alone: of a Scala kind, these being Scala's glyphs; any other kind has none here.
  def markup(kind: Api.Kind, modifiers: List[Text]): Text = kind match
    case kind: Api.Kind.Scala => drawing(kind, modifiers).markup
    case _                    => t""
