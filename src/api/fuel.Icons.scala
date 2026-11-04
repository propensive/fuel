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

import soundness.*

import pyrocosm.Inline

// The glyph beside a member's name: a drawing of its keyword, on the 14-unit grid of the
// original icons (a 10 mm box, every shape on multiples of a fourteenth of it), as SVG markup
// which Pyrocosm draws inline, in the text's colour and at its size. A modifier transforms the
// drawing: generically where it can — `final` ends it with a full stop, `inline` shears it,
// `transparent` stripes it as if translucent — and by a shape of its own where the drawings
// differ: an abstract `def` is hollow, an opaque type has a core, an enum case is a dot.
//
// Every modifier fuel records, and what the glyph does with it (one marked *undecided* is drawn
// plainly until it is decided; the drawings named are the originals to decide from):
//
//   final         a full stop after the glyph
//   inline        sheared to the right
//   transparent   horizontal stripes
//   abstract      def: the square hollowed (a-def); class, case class: undecided (a-class,
//                 a-cclass look like class and cclass at a glance)
//   opaque        type: a core ring (o-type)
//   sealed        undecided (s-trait: the open side walled; sa-class, sa-cclass: the two rings
//                 one band; st-trait)
//   lazy          undecided (l-val, l-given: the corners rounded; l-object, l-package: as they are)
//   protected     undecided (p-given: a bar on the left; p-val, p-var, p-type: as they are)
//   erased        a slash through the glyph, at 45°, with a gap about it so that it shows
//   override      a line beneath the glyph
//   infix         a small square at mid-height either side, as if the glyph were placed infix
//   implicit      nothing: it is deprecated
//   open          undecided (nothing drawn for it)
//   extension     a def with a small plus at the top right (e-def)
//   enum case     a dot (c-case); on a class or object, a dot at the centre (c-class, c-object)
object Icons:
  private val grid: Int = 14
  private val c: Double = 7.0

  // A drawing: its elements, and its width, which grows past the grid once a full stop follows.
  private case class Drawing(elements: List[Text], width: Int = grid):
    def add(more: Text*): Drawing = copy(elements = elements + List.from(more))

    def within(open: Text, close: Text): Drawing =
      copy(elements = List(open) + elements + List(close))

    def markup: Text =
      val box: Text = t"""viewBox="0 0 ${width.toString.tt} ${grid.toString.tt}""""
      t"""<svg xmlns="http://www.w3.org/2000/svg" $box>${elements.join}</svg>"""

  private def num(value: Double): Text =
    val boxed: java.lang.Double = java.lang.Double.valueOf(value).nn
    val text: String = java.lang.String.format(java.util.Locale.ROOT, "%.2f", boxed).nn

    val trimmed: String =
      if text.indexOf('.') >= 0 then text.replaceAll("0+$", "").nn.replaceAll("\\.$", "").nn
      else text

    trimmed.tt

  // A point on a circle about the centre, by its radius and its angle in degrees, clockwise from
  // the right (the page's `y` grows downward).
  private def point(radius: Double, degrees: Double): Text =
    val radians: Double = degrees*scala.math.Pi/180
    t"${num(c + radius*scala.math.cos(radians))} ${num(c + radius*scala.math.sin(radians))}"

  private def disc(radius: Double, x: Double = c, y: Double = c): Text =
    t"""<circle cx="${num(x)}" cy="${num(y)}" r="${num(radius)}"/>"""

  private def rect(x: Double, y: Double, width: Double, height: Double): Text =
    t"""<rect x="${num(x)}" y="${num(y)}" width="${num(width)}" height="${num(height)}"/>"""

  private def polygon(points: (Double, Double)*): Text =
    val listed: Text = List.from(points).map { (x, y) => t"${num(x)},${num(y)}" }.join(t" ")
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

  // The part of a ring to the right of a vertical line, its ends flat.
  private def half(outer: Double, inner: Double, x: Double): Text =
    def dy(r: Double): Double = scala.math.sqrt((r*r - (x - c)*(x - c)).max(0.0))
    val o: Text = num(outer)
    val i: Text = num(inner)
    val top = t"${num(x)} ${num(c - dy(outer))}"
    val bottom = t"${num(x)} ${num(c + dy(outer))}"
    val innerTop = t"${num(x)} ${num(c - dy(inner))}"
    val innerBottom = t"${num(x)} ${num(c + dy(inner))}"
    t"""<path d="M $top A $o $o 0 0 1 $bottom L $innerBottom A $i $i 0 0 0 $innerTop Z"/>"""

  private val square: Text = rect(4.5, 4.5, 5, 5)
  private val dot: Text = disc(1)

  // The keyword's drawing, before any modifier.
  private def base(kind: Api.Kind): Drawing = kind match
    case Api.Kind.Package =>
      val top: List[Text] = List(rect(2.5, 2.5, 4, 4), rect(7.5, 2.5, 4, 4))
      val bottom: List[Text] = List(rect(2.5, 7.5, 4, 4), rect(7.5, 7.5, 4, 4))
      Drawing(top + bottom)

    case Api.Kind.Object    => Drawing(List(ring(5, 3)))
    case Api.Kind.Class     => Drawing(List(band(5, 4, 45, 315), band(3, 2, 45, 315)))
    case Api.Kind.CaseClass => Drawing(List(band(5, 4, 322.5, 317.5), band(3, 2, 45, 315)))
    case Api.Kind.Trait     => Drawing(List(half(5, 4, 6), half(3, 2, 6), half(1.5, 0, 6)))

    case Api.Kind.Enum =>
      val discs: List[Text] = List(0, 1, 2, 3, 4).map: (n: Int) =>
        val radians: Double = (n*72 - 90)*scala.math.Pi/180
        disc(1.4, c + 2.38*scala.math.cos(radians), c + 2.38*scala.math.sin(radians))

      Drawing(discs)

    case Api.Kind.Case      => Drawing(List(disc(1.4)))
    case Api.Kind.Type      => Drawing(List(band(4, 3, -45, 45), band(4, 3, 135, 225)))
    case Api.Kind.TypeParam => base(Api.Kind.Type)
    case Api.Kind.Def       => Drawing(List(square))
    case Api.Kind.Val       => Drawing(List(polygon((4.08, 4.95), (9.92, 4.95), (7, 10))))

    case Api.Kind.Var =>
      val chevron: Text =
        polygon((6.1, 4.67), (4.08, 4.67), (7, 9.72), (9.92, 4.67), (7.9, 4.67), (7, 6.22))

      Drawing(List(chevron))

    case Api.Kind.Given     => Drawing(List(polygon((7, 3.46), (10.54, 7), (7, 10.54), (3.46, 7))))
    case Api.Kind.Extension => Drawing(List(square, rect(10.5, 1.5, 1, 3), rect(9.5, 2.5, 3, 1)))
    case Api.Kind.Param     => Drawing(List(rect(3.5, 9.95, 7, 1.17)))

  // The drawing with its modifiers: the shapes of their own first, then the transformations of
  // the whole, and the full stop last, outside them.
  private def drawing(kind: Api.Kind, modifiers: List[Text]): Drawing =
    def has(modifier: Text): Boolean = modifiers.has(modifier)

    val shaped: Drawing =
      if kind == Api.Kind.Def && has(t"abstract") then
        val hollow: Text = t"M 4.5 4.5 h 5 v 5 h -5 Z M 6 6 h 2 v 2 h -2 Z"
        Drawing(List(t"""<path fill-rule="evenodd" d="$hollow"/>"""))
      else if (kind == Api.Kind.Type || kind == Api.Kind.TypeParam) && has(t"opaque") then
        base(kind).add(ring(2, 1))
      else
        base(kind)

    val sheared: Drawing =
      if has(t"inline")
      then shaped.within(t"""<g transform="translate(7 0) skewX(-14) translate(-7 0)">""", t"</g>")
      else shaped

    val striped: Drawing =
      if !has(t"transparent") then sheared else
        val stripes: Text =
          t"""<defs><pattern id="fuel-stripes" width="14" height="2" """ +
            t"""patternUnits="userSpaceOnUse"><rect width="14" height="1" fill="#fff"/>""" +
            t"""</pattern><mask id="fuel-striped"><rect width="100%" height="100%" """ +
            t"""fill="url(#fuel-stripes)"/></mask></defs>"""

        sheared.within(t"""$stripes<g mask="url(#fuel-striped)">""", t"</g>")

    // A slash is cut through the glyph before it is drawn, so that it shows on a filled shape.
    val slashed: Drawing =
      if !has(t"erased") then striped else
        val gap: Text =
          t"""<defs><mask id="fuel-slashed"><rect width="100%" height="100%" fill="#fff"/>""" +
            t"""<rect x="5.8" y="0.2" width="2.4" height="13.6" fill="#000" """ +
            t"""transform="rotate(45 7 7)"/></mask></defs>"""

        val slash: Text =
          t"""<rect x="6.6" y="0.6" width="0.8" height="12.8" transform="rotate(45 7 7)"/>"""

        striped.within(t"""$gap<g mask="url(#fuel-slashed)">""", t"</g>").add(slash)

    val underlined: Drawing = if has(t"override") then slashed.add(rect(3, 12.6, 8, 1)) else slashed

    val placed: Drawing =
      if has(t"infix") then underlined.add(rect(0, 6.25, 1.5, 1.5), rect(12.5, 6.25, 1.5, 1.5))
      else underlined

    if has(t"final") then placed.copy(width = grid + 4).add(disc(1.2, 16, 11.3)) else placed

  // The glyph for a declaration, as an icon with no description of its own: the keyword beside
  // it says what it is, so a plain rendering says nothing twice.
  def of(kind: Api.Kind, modifiers: List[Text]): Inline =
    Inline.Icon(drawing(kind, modifiers).markup, t"")

  // The markup alone, for inspection.
  def markup(kind: Api.Kind, modifiers: List[Text]): Text = drawing(kind, modifiers).markup
