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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Inline, Tone}

// A presentation: a TEL document of `slide`s, each holding Markdown or a block of code, shown in
// a browser one slide at a time, to the schema `presentation` (`Presentation.schemaText`).
//
//   title  Effects, gently        # what the page is called; the file's name otherwise
//   slide
//     markdown
//         # Effects, gently
//         A *short* talk.
//     transcript
//         Welcome. This is a short talk about effects.
//   slide
//     syntax scala
//         val x = 1
//   slide
//     syntax scala                # a fragment's preamble beneath, its code in `code`
//       highlight y  a new name
//       label  the second step
//       code
//           val x = 1
//           val y = x + 1
//
// A slide's `syntax` names the language its code is highlighted as, and holds the code itself,
// or says what a fence's preamble says — marks, `context`, `errors`, how far it is compiled,
// `frame`s and the rest — beneath it, with the code in a `code` child, as a frame's is. A slide
// holds its parts in order, and may have a `transcript`: what is said while it is shown, from
// which `fuel transcript` makes the presentation's. Consecutive slides which each hold one block
// of code without frames of its own, in the same language, are one animated block of code, its
// frames those slides — each with the slide's marks, and its `label`, `hold` and `at` — so that
// stepping from one to the next moves what stays and fades what arrives and departs, as a
// fence's frames do; the deck steps through the frames before moving on. What else the block
// says is the first slide's.
object Presentation:
  // The schema a presentation is checked against, as a TEL schema document: what a slide may
  // hold, and what a slide's code may say, which is what a fence's preamble may say. The
  // schema is the formal account of the format; `Reading` reads what it admits. A member
  // which may be written as an atom on the keyword's line, or as a child beneath it, is listed
  // before those which may not, since atoms are assigned to members in order.
  val schemaText: Text = t"""tel 1.0

name presentation

# A slide's code: `syntax <language>` holds the code itself as its indented text, or says what
# a fence's preamble says beneath it, with the code in a `code` child. `language` names the
# language the code is compiled as, where that is not the one it is highlighted as.

record Code
  field language String
  field code String optional
  field label String optional
  field hold String optional
  field at String optional
  field context String optional
  field id String optional
  field follows String optional repeatable
  field errors String optional
  field parse Flag optional
  field typecheck Flag optional
  field compile Flag optional
  field autoplay Flag optional
  field loop Flag optional
  field restart Flag optional
  field run Flag optional
  field feature String optional repeatable
  field path String optional
  field highlight Mark optional repeatable
  field error Mark optional repeatable
  field caution Mark optional repeatable
  field param Mark optional repeatable
  field focus Mark optional repeatable
  field substitute Substitution optional repeatable
  field frame Frame optional repeatable
  field alternative Alternative optional repeatable

# A mark on a selection of the code: the text it starts at, and the text it ends at, with its
# caption after them on the line or beneath; `line` and `occurrence` say which match is meant.

record Mark
  field selection String
  field caption String optional
  field line String optional
  field occurrence String optional

# A substitution: the text shown over a selection of the code, in place of it.

record Substitution
  field selection String
  field text String optional
  field line String optional
  field occurrence String optional

# A frame of an animated block: its code, and what is said of it.

record Frame
  field code String optional
  field label String optional
  field hold String optional
  field at String optional
  field language String optional
  field highlight Mark optional repeatable
  field error Mark optional repeatable
  field caution Mark optional repeatable
  field param Mark optional repeatable
  field focus Mark optional repeatable
  field substitute Substitution optional repeatable

# An alternative version of the code, shown as a tab: its label on the line, and its code,
# frames and the rest beneath.

record Alternative
  field label String
  field code String optional
  field language String optional
  field highlight Mark optional repeatable
  field error Mark optional repeatable
  field caution Mark optional repeatable
  field param Mark optional repeatable
  field focus Mark optional repeatable
  field substitute Substitution optional repeatable
  field frame Frame optional repeatable

# A slide's parts, in any order: Markdown, or code in a language.

select Part
  variant markdown String
  variant syntax Code

# A slide: its parts, and what is said while it is shown.

record Slide
  select Part optional repeatable
  field transcript String optional

document
  field title String optional
  field slide Slide optional repeatable
"""

  // The schema, read from its document.
  lazy val schema: stratiform.Tels =
    import strategies.throwUnsafely
    schemaText.read[Tel].as[stratiform.Tels]

  enum Part:
    case Prose(markdown: Text)

    case Code
      ( language: Text,
        code:     Text,
        preamble: Preamble         = Preamble.empty,
        label:    Optional[Text]   = Unset,
        hold:     Optional[Double] = Unset,
        at:       Optional[Double] = Unset )

  case class Slide(parts: List[Part], line: Int, transcript: Optional[Text] = Unset)

  // The lines `headed` adds before the file's own.
  private def headed(text: Text): Text = pyrocosm.Tels.headed(text)(0)

  private def added(text: Text): Int = pyrocosm.Tels.headed(text)(1)

  // The faults the schema finds, as they are accrued: each TEL error, with the path of what it
  // concerns and where that is.
  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the presentation has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // A path's keywords, without the indices of repeated fields.
  private def keywords(pointer: Text): List[Text] =
    pointer.cut(t"/").filter { segment => segment != t"" && safely(segment.as[Int]).absent }

  // What a fault the schema finds means in a presentation.
  private def explain(error: Tel.Error, pointer: Text): Text =
    import Tel.Error.Reason
    val path: List[Text] = keywords(pointer)
    val last: Text = path.reverse.head.or(t"")
    val parent: Text = path.reverse.skip(1).head.or(t"")

    error.reason match
      case Reason.UnknownKeyword => path.size match
        case 1 =>
          t"A presentation has a `title` and `slide`s, but not `$last`."

        case 2 =>
          if last == t"code"
          then t"A slide's code is `syntax <language>`, not `code`: `syntax` names the highlighting."
          else t"A slide has `markdown`, `syntax <language>` and a `transcript`, but not `$last`."

        case _ =>
          t"`$last` is not something a slide's `$parent` says."

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        t"This has more values than it takes: a value with spaces in it follows two spaces."

      case Reason.RequiredMemberAbsent =>
        if last == t"language" then t"A slide's `syntax` names its language, as `syntax scala`."
        else t"This needs a `$last`."

      case Reason.NonRepeatableTooMany =>
        t"This says `$last` more than once."

      case _ =>
        error.message.text

  // The diagnostics the schema raises against a presentation's TEL, each at its place in the
  // file: a fault without a place of its own (a missing member) at its parent's.
  private def checked(path: Text, text: Text, tel: Tel): List[Block] =
    import strategies.throwUnsafely

    def at(pointer: List[Text]): Optional[Int] =
      if pointer.nil then Unset
      else tel.locateKey(Telp(pointer)).lay(at(pointer.reverse.skip(1).reverse)):
        (position: Tel.Error.Position) => position.line - added(text)

    val faults: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error =>
          val pointer: Text = prior.let(_.pointer.encode).or(t"/")
          accrual + (error, pointer, prior.lay(Span.empty)(_.span))

      . protect(Tel.Type.assign(tel, schema))

    faults.items.map: (error, pointer, span) =>
      val message: Text = explain(error, pointer)

      val line: Optional[Int] =
        if span.exists then span.startLine.lay(1)(_.n1) - added(text)
        else at(pointer.cut(t"/").filter(_ != t"").reverse.skip(1).reverse)

      val location: Block.Location = Block.Location(path, line.or(1))
      Block.Diagnostic(Tone.Warning, location, List(Block.paragraph(message)))

  // A presentation read from its text, with a diagnostic for each fault the schema finds and
  // each part not understood; or the diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (Presentation, List[Block]) | Block.Diagnostic =
    import strategies.throwUnsafely
    import parsing.trackPositions

    try
      val tel: Tel = headed(text).read[Tel]
      val (presentation, problems) = Reading(path, text, tel).presentation
      (presentation, checked(path, text, tel) + problems)
    catch case error: Tel.Error =>
      val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))

      def shifted(location: Block.Location): Block.Location =
        location.copy(line = location.line.let(_ - added(text)))

      val location: Optional[Block.Location] = raw.location.let(shifted)

      raw.copy(location = location)

  // One reading of a presentation's TEL, gathering the problems it finds.
  private class Reading(path: Text, text: Text, tel: Tel):
    @scala.caps.unsafe.untrackedCaptures
    private var problems: List[Block] = Nil

    private def report(line: Int, message: Text): Unit =
      val location: Block.Location = Block.Location(path, line)
      val diagnostic = Block.Diagnostic(Tone.Warning, location, List(Block.paragraph(message)))
      problems = diagnostic :: problems

    // Each slide's line, from one: the nth line beginning `slide` is the nth slide.
    private def slideLine(line: Text): Boolean = line == t"slide" || line.starts(t"slide ")

    private val lines: List[Int] =
      text.cut(t"\n").indexed.filter { (line: Text, _) => slideLine(line) }.map(_(1).n1)

    private def words(compound: Tel.Compound): List[Text] =
      List.from(compound.atoms.stdlib.collect { case Tel.Atom.Inline(text, _) => text }.toList)

    private def source(compound: Tel.Compound): Optional[Text] =
      compound.atoms.stdlib.collectFirst { case Tel.Atom.Source(text) => text }.getOrElse(Unset)

    // The text of a node's child: its atoms, inline then the source, joined.
    private def child(node: Tel, keyword: Text): Optional[Text] =
      node.field(keyword).let(_.atomTexts.to[List].join(t" ")).let: (text: Text) =>
        if text.trim == t"" then Unset else text

    private def number(node: Tel, keyword: Text): Optional[Double] =
      child(node, keyword).let: text => safely(text.as[Double])

    // A slide's code: `syntax <language>` with the code alone beneath, or a preamble beneath
    // it with the code in `code`.
    private def code(child: Tel.Compound, node: Optional[Tel], line: Int): List[Part] =
      words(child).head match
        case language: Text =>
          node.let: (node: Tel) =>
            if child.children.stdlib.forall(_.compounds.stdlib.isEmpty) then
              List(Part.Code(language, source(child).or(words(child).skip(1).join(t" "))))
            else
              val preamble: Preamble = Preamble.of(node)
              val body: Optional[Text] = this.child(node, t"code")

              if body.absent && preamble.frames.nil
              then report(line, t"This slide's code has nothing in its `code`.")

              val label: Optional[Text] = this.child(node, t"label")
              val (hold, at) = (number(node, t"hold"), number(node, t"at"))
              List(Part.Code(language, body.or(t""), preamble, label, hold, at))

          . or(Nil)

        case _ =>
          Nil   // the schema reports the missing language

    // A slide's part; a keyword which is not one is the schema's to report.
    private def part(child: Tel.Compound, node: Optional[Tel], line: Int): List[Part] =
      child.keyword match
        case t"markdown" => List(Part.Prose(source(child).or(words(child).join(t" "))))
        case t"syntax"   => code(child, node, line)
        case _           => Nil

    // A slide, its compound giving the order of its parts and `node` the TEL of each `syntax`,
    // the nth `syntax` compound being the nth `syntax` field; and its transcript.
    private def slide(compound: Tel.Compound, node: Optional[Tel], index: Ordinal): Slide =
      val line: Int = lines.at(index).or(1)

      val children: List[Tel.Compound] =
        List.from(compound.children.stdlib.toList.flatMap(_.compounds.stdlib.toList))

      val codes: List[Tel] = node.lay(Nil)(_.fields(t"syntax").to[List])

      val parts: List[Part] = children.indexed.bind: (child: Tel.Compound, position: Ordinal) =>
        val ordinal: Int = children.keep(position.n0).filter(_.keyword == t"syntax").size
        part(child, codes.at(Ordinal.zerary(ordinal)), line)

      if children.nil then report(line, t"This slide is empty.")

      val transcript: Optional[Text] =
        children.filter(_.keyword == t"transcript").head.let: (child: Tel.Compound) =>
          source(child).or(words(child).join(t" ")).trim

      Slide(parts, line, transcript.let { text => if text == t"" then Unset else text })

    def presentation: (Presentation, List[Block]) =
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List]

      val title: Optional[Text] = tel.field(t"title").let(_.atomTexts.to[List].join(t" ").trim)
      val nodes: List[Tel] = tel.fields(t"slide").to[List]

      val slides: List[Slide] =
        compounds.filter(_.keyword == t"slide").indexed.map: (compound, index) =>
          slide(compound, nodes.at(index), index)

      val named: Optional[Text] = title.let: (title: Text) => if title == t"" then Unset else title

      (Presentation(named, slides), problems.reverse)

  // A slide's parts as blocks, numbered nowhere, since a slide is not a file.
  private def unnumbered(blocks: List[Block]): List[Block] = blocks.map:
    case code: Block.Code => code.copy(start = Unset)
    case other            => other

  // The deck: each slide a leaf, but each run of consecutive slides holding one block of code
  // in one language a single leaf of one animated block, by the embedding a fence with frames
  // goes through. Every fence is compiled by `scala`, and every table read by `tables`, as a
  // document's are; what they report is the problems, apart from the slides.
  def deck
    ( presentation: Presentation,
      scala:        Embedding.ScalaCode,
      linker:       Linker,
      tables: Tables = Tables(t"") )
  :   Block =

    val embeddings: List[Embedding] = List(scala, Embedding.JavaCode, tables)

    // A slide's one block of code, which may join a run of its neighbours' as a frame.
    def lone(slide: Slide): Optional[Part.Code] = slide.parts match
      case (code: Part.Code) :: Nil if code.preamble.frames.nil   => code
      case _                                                      => Unset

    def info(language: Text): List[Text] = List(language, t"fuel")

    def blocks(part: Part, line: Int): List[Block] = part match
      case Part.Prose(markdown) =>
        Blocks.of(Parser.parse(markdown), embeddings, linker)

      case code: Part.Code =>
        scala.embedWith(info(code.language), code.preamble, code.code, line)

    def leaf(slide: Slide): Block.Leaf =
      Block.Leaf(unnumbered(slide.parts.bind(blocks(_, slide.line))))

    // A slide's code as a frame of a run: its code, its marks, its label, hold and time.
    def frame(code: Part.Code): Preamble.Frame =
      val label: List[Inline] = code.label.lay(Nil)(Blocks.inline(_, linker))
      Preamble.Frame(code.code, label, code.hold, code.at, Unset, code.preamble.marks)

    // A run of slides as one fragment: the first's preamble, its marks given to its frame, and
    // a frame for each.
    def animated(run: List[Slide], language: Text): Block.Leaf =
      val codes: List[Part.Code] = run.bind(lone(_).let(List(_)).or(Nil))
      val line: Int = run.head.let(_.line).or(1)
      val first: Preamble = codes.head.let(_.preamble).or(Preamble.empty)
      val preamble: Preamble = first.copy(marks = Nil, frames = codes.map(frame))
      val fenced: List[Block] = scala.embedWith(info(language), preamble, t"", line)

      val code: List[Block] = fenced.filter:
        case Block.Code(_, _, _, _, _, _, _, _) => true
        case _                            => false

      Block.Leaf(unnumbered(code))

    def same(slides: List[Slide], language: Text): List[Slide] = slides match
      case next :: rest if lone(next).let(_.language == language).or(false) =>
        next :: same(rest, language)

      case _ =>
        Nil

    def recur(slides: List[Slide]): List[Block.Leaf] = slides match
      case head :: tail =>
        lone(head).lay(leaf(head) :: recur(tail)): (code: Part.Code) =>
          val following: List[Slide] = same(tail, code.language)

          if following.nil then leaf(head) :: recur(tail)
          else animated(head :: following, code.language) :: recur(tail.skip(following.size))

      case _ =>
        Nil

    Block.Deck(recur(presentation.slides))

  // The steps a deck is stepped through: on each slide, the frames after the first of its
  // last animated block, which the deck steps before it moves on; and the move to each slide
  // after the first. A schedule has a time for each.
  def steps(deck: Block.Deck): Int =
    val frames: Int = deck.leaves.map: (leaf: Block.Leaf) =>
      val animated: List[Block.Animation] = leaf.content.bind:
        case Block.Code(_, _, _, _, animation: Block.Animation, _, _, _) => List(animation)
        case _                                                            => Nil

      animated.reverse.head.let(_.slides.size - 1).or(0)

    . stdlib.sum

    frames + (deck.leaves.size - 1).max(0)

  // A schedule which lands a step every `interval` seconds, the first after one interval: a
  // stand-in until a presentation's own timings are read from a file.
  def regular(steps: Int, interval: Double): List[Double] =
    List.range(1, steps + 1).map(_ * interval)

  // The deck, playing itself to the schedule.
  def scheduled(deck: Block.Deck, schedule: List[Double]): Block.Deck =
    deck.copy(schedule = schedule)

  // The presentation's transcript: each slide's, in order, as paragraphs — a transcript's
  // blank lines divide it into paragraphs — which a renderer sets to a width.
  def transcript(presentation: Presentation): List[Block] =
    presentation.slides.bind: (slide: Slide) =>
      slide.transcript.lay(Nil): (text: Text) =>
        text.cut(t"\n\n").map(_.trim).filter(_ != t"").map: (paragraph: Text) =>
          Block.paragraph(paragraph.cut(t"\n").map(_.trim).join(t" "))

case class Presentation(title: Optional[Text], slides: List[Presentation.Slide])
