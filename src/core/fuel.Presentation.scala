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

import soundness.{Language as _, Token as _, *}

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Inline, Tone}

// A presentation: a TEL document of `slide`s, each holding Markdown or a block of code, shown in
// a browser one slide at a time.
//
//   title  Effects, gently        # what the page is called; the file's name otherwise
//   slide
//     markdown
//         # Effects, gently
//         A *short* talk.
//   slide
//     code scala
//         val x = 1
//   slide
//     code scala                  # a fragment's preamble beneath, its code in `code`
//       highlight y  a new name
//       label  the second step
//       code
//           val x = 1
//           val y = x + 1
//
// A slide's code says what a fence's preamble says — marks, `context`, `errors`, how far it is
// compiled, `frame`s and the rest — beneath it, with the code itself in a `code` child, as a
// frame's is; or it is just the code. A slide holds its parts in order. Consecutive slides which
// each hold one block of code without frames of its own, in the same language, are one animated
// block of code, its frames those slides — each with the slide's marks, and its `label`, `hold`
// and `at` — so that stepping from one to the next moves what stays and fades what arrives and
// departs, as a fence's frames do; the deck steps through the frames before moving on. What
// else the block says is the first slide's.
object Presentation:
  enum Part:
    case Prose(markdown: Text)

    case Code
      ( language: Text,
        code:     Text,
        preamble: Preamble         = Preamble.empty,
        label:    Optional[Text]   = Unset,
        hold:     Optional[Double] = Unset,
        at:       Optional[Double] = Unset )

  case class Slide(parts: List[Part], line: Int)

  // The lines `headed` adds before the file's own.
  private def headed(text: Text): Text =
    if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"

  private def added(text: Text): Int = if text.trim.starts(t"tel ") then 0 else 2

  // A presentation read from its text, with a diagnostic for each part not understood; or the
  // diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (Presentation, List[Block]) | Block.Diagnostic =
    import strategies.throwUnsafely

    try Reading(path, text, headed(text).read[Tel]).presentation catch case error: Tel.Error =>
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

    // A slide's code: the code alone, or a preamble beneath it with the code in `code`.
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
          report(line, t"A slide's `code` names its language, as `code scala`.")
          Nil

    private def part(child: Tel.Compound, node: Optional[Tel], line: Int): List[Part] =
      child.keyword match
        case t"markdown" =>
          List(Part.Prose(source(child).or(words(child).join(t" "))))

        case t"code" =>
          code(child, node, line)

        case other =>
          report(line, t"`$other` is not part of a slide: use `markdown` or `code <language>`.")
          Nil

    // A slide, its compound giving the order of its parts and `node` the TEL of each `code`,
    // the nth `code` compound being the nth `code` field.
    private def slide(compound: Tel.Compound, node: Optional[Tel], index: Ordinal): Slide =
      val line: Int = lines.at(index).or(1)

      val children: List[Tel.Compound] =
        List.from(compound.children.stdlib.toList.flatMap(_.compounds.stdlib.toList))

      val codes: List[Tel] = node.lay(Nil)(_.fields(t"code").to[List])

      val parts: List[Part] = children.indexed.bind: (child: Tel.Compound, position: Ordinal) =>
        val ordinal: Int = children.keep(position.n0).filter(_.keyword == t"code").size
        part(child, codes.at(Ordinal.zerary(ordinal)), line)

      if parts.nil then report(line, t"This slide is empty.")
      Slide(parts, line)

    def presentation: (Presentation, List[Block]) =
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List]

      compounds.each: (compound: Tel.Compound) =>
        if compound.keyword != t"slide" && compound.keyword != t"title"
        then report(1, t"`${compound.keyword}` is not part of a presentation: use `slide`.")

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
  // goes through. Every fence is compiled by `scala`, as a document's are; what the compile
  // reports is the problems, apart from the slides.
  def deck(presentation: Presentation, scala: Embedding.ScalaCode, linker: Linker): Block =
    val embeddings: List[Embedding] = List(scala, Embedding.JavaCode)

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
        case Block.Code(_, _, _, _, _, _) => true
        case _                         => false

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

case class Presentation(title: Optional[Text], slides: List[Presentation.Slide])
