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
import pyrocosm.{Block, Tone}

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
//     code scala
//         val x = 1
//         val y = x + 1
//
// A slide holds its parts in order. Consecutive slides which each hold one block of code, in
// the same language, are one animated block of code, its frames those slides, so that stepping
// from one to the next moves what stays and fades what arrives and departs, as a fence's frames
// do; the deck steps through the frames before moving on.
object Presentation:
  enum Part:
    case Prose(markdown: Text)
    case Code(language: Text, code: Text)

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

    private def part(child: Tel.Compound, line: Int): List[Part] = child.keyword match
      case t"markdown" =>
        List(Part.Prose(source(child).or(words(child).join(t" "))))

      case t"code" => words(child).prim match
        case language: Text =>
          List(Part.Code(language, source(child).or(words(child).skip(1).join(t" "))))

        case _ =>
          report(line, t"A slide's `code` names its language, as `code scala`.")
          Nil

      case other =>
        report(line, t"`$other` is not part of a slide: use `markdown` or `code <language>`.")
        Nil

    private def slide(compound: Tel.Compound, index: Ordinal): Slide =
      val line: Int = lines.at(index).or(1)

      val children: List[Tel.Compound] =
        List.from(compound.children.stdlib.toList.flatMap(_.compounds.stdlib.toList))

      val parts: List[Part] = children.bind(part(_, line))
      if parts.nil then report(line, t"This slide is empty.")
      Slide(parts, line)

    def presentation: (Presentation, List[Block]) =
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List]

      compounds.each: (compound: Tel.Compound) =>
        if compound.keyword != t"slide" && compound.keyword != t"title"
        then report(1, t"`${compound.keyword}` is not part of a presentation: use `slide`.")

      val title: Optional[Text] = tel.field(t"title").let(_.atomTexts.to[List].join(t" ").trim)
      val slides: List[Slide] = compounds.filter(_.keyword == t"slide").indexed.map(slide)
      val named: Optional[Text] = title.let: (title: Text) => if title == t"" then Unset else title

      (Presentation(named, slides), problems.reverse)

  // The code of a run of slides as a fence's preamble: a frame for each, its code beneath it.
  private def frames(codes: List[Text]): Text =
    def indent(line: Text): Text = if line.trim == t"" then t"" else t"      $line"
    def indented(code: Text): Text = code.cut(t"\n").map(indent).join(t"\n")

    t"${codes.map { (code: Text) => t"frame\n  code\n${indented(code)}" }.join(t"\n")}\n##\n"

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

    def lone(slide: Slide): Optional[Part.Code] = slide.parts match
      case (code: Part.Code) :: Nil => code
      case _                        => Unset

    def blocks(part: Part, line: Int): List[Block] = part match
      case Part.Prose(markdown)      => Blocks.of(Parser.parse(markdown), embeddings, linker)
      case Part.Code(language, code) => Blocks.fence(List(language), code, line, embeddings)

    def leaf(slide: Slide): Block.Leaf =
      Block.Leaf(unnumbered(slide.parts.bind(blocks(_, slide.line))))

    def animated(run: List[Slide], language: Text): Block.Leaf =
      val codes: List[Text] = run.map(lone(_).let(_.code).or(t""))
      val line: Int = run.prim.let(_.line).or(1)
      val info: List[Text] = List(language, t"fuel")
      val fenced: List[Block] = Blocks.fence(info, frames(codes), line, embeddings)

      val code: List[Block] = fenced.filter:
        case Block.Code(_, _, _, _, _) => true
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
