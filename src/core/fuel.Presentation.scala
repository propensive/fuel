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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Inline, Tone}

// A presentation: a TEL document of `slide`s, each holding Markdown or a block of code, shown in
// a browser one slide at a time, to the schema `presentation` (`fuel/presentation.schema.tel`).
//
//   title  Effects, gently        # what the page is called; the file's name otherwise
//   style  talk.css               # a stylesheet of its own, a file beside this one
//   script  talk.js               # a script of its own, likewise
//   slide
//     markdown
//         # Effects, gently
//         A *short* talk.
//   slide one-value               # the slide's id, which the transcript cues it by
//     syntax scala
//         val x = 1
//   slide a-second-name
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
// holds its parts in order. Consecutive slides which each hold one block
// of code without frames of its own, in the same language, are one animated block of code, its
// frames those slides — each with the slide's marks, and its `label`, `hold` and `at` — so that
// stepping from one to the next moves what stays and fades what arrives and departs, as a
// fence's frames do; the deck steps through the frames before moving on. What else the block
// says is the first slide's.
//
// The presentation is in one state at a time: a slide as it is first shown, or a later frame of
// the animation the deck steps through on it. A state may have an `id` — a slide's on its line
// or beneath it, a frame's likewise. What is said is a transcript, a Markdown file apart from
// the presentation, which is given with it when it is shown, so that one presentation may be
// read in each of several languages: in it the words which bring a state are a link to its id,
// `[words](@id)` (`Transcript`). A transcript must cue every state which has an id, once, in
// the order the states are shown, and nothing else: `cohere` says where it does not, and a
// presentation is not shown with a transcript which does not.
object Presentation:
  // The schema a presentation is checked against, a TEL schema document among the resources:
  // what a slide may hold, and what a slide's code may say, which is what a fence's preamble
  // may say. The schema is the formal account of the format; `Reading` reads what it admits.
  val schemaResource: Text = t"fuel/presentation.schema.tel"

  // The schema's document, as it was written: read through the thread-context classloader,
  // since under Burdock the system one sees only the launcher's own jar.
  def schemaSource: Text =
    Classloader.threadContext.resource(schemaResource).let(_.utf8).or:
      panic(m"the resource $schemaResource is missing")

  // The schema, read from its document and held to the rules every schema is.
  lazy val schema: stratiform.Tels =
    import strategies.throwUnsafely
    stratiform.Tels.Validation.validate(schemaSource.read[Tel].as[stratiform.Tels])

  enum Part:
    case Prose(markdown: Text)

    case Code
      ( language: Text,
        code:     Text,
        preamble: Preamble         = Preamble.empty,
        label:    Optional[Text]   = Unset,
        hold:     Optional[Double] = Unset,
        at:       Optional[Double] = Unset )

  case class Slide(parts: List[Part], line: Int, id: Optional[Text] = Unset)

  // The stylesheet a presentation names: its file, as it is written — relative to the
  // presentation's — and the line which names it. Its rules come after Pyrocosm's own, so a
  // presentation may be set in a site's colours and faces.
  case class Style(file: Text, line: Int)

  // The script a presentation names, likewise: run by its page after Pyrocosm's own, for what
  // a stylesheet cannot do — a backdrop which moves, say.
  case class Script(file: Text, line: Int)

  // A state of the presentation: slide `slide` (from one) as it is first shown, or its
  // animation's frame `frame` (from one), with the id it has and the line of its slide.
  case class State(id: Optional[Text], line: Int, slide: Int, frame: Optional[Int] = Unset):
    def described: Text =
      val within: Text = frame.lay(t"") { (frame: Int) => t", frame ${frame.show}" }
      t"slide ${slide.show} (line ${line.show})$within"

  // The faults the schema finds, as they are accrued: each TEL error, with the path of what it
  // concerns and where that is.
  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the presentation has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // A path's keywords, without the indices of repeated fields.
  private def keywords(pointer: Text): List[Text] =
    pointer.cut(t"/").filter { segment => segment != t"" && safely(segment.as[Int]).absent }

  // The code of a presentation's problem, `↯/f.25.6.<variant>`: a fault the schema finds, which
  // carries the TEL error's own code (1); a slide's code with nothing in it (2); an empty
  // slide (3); an id which two states have, or a state which has two (4); a cue which names no
  // state (5); a state with an id which the transcript does not cue (6); a state cued twice,
  // or out of its order (7); a cue which is not in what is said (8); a transcript which
  // cannot be read (9); a stylesheet which cannot be read (10); and a script which cannot be
  // read (11).
  private def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.core, FuelBolts.presentation, variant)

  // What a fault the schema finds means in a presentation.
  private def explain(error: Tel.Error, pointer: Text): Text =
    import Tel.Error.Reason
    val path: List[Text] = keywords(pointer)
    val last: Text = path.reverse.head.or(t"")
    val parent: Text = path.reverse.skip(1).head.or(t"")

    error.reason match
      case Reason.UnknownKeyword => path.size match
        case 1 =>
          if last == t"transcript"
          then t"A presentation does not name its transcript, which is given beside it: `fuel present <file> --transcript <transcript>`."
          else t"A presentation has a `title`, a `style`, a `script` and `slide`s, but not `$last`."

        case 2 =>
          if last == t"code"
          then t"A slide's code is `syntax <language>`, not `code`: `syntax` names the highlighting."
          else if last == t"transcript"
          then t"What is said is not written on a slide: it is a Markdown file given beside the presentation, `fuel present <file> --transcript <transcript>`, which links to the slide's `id` as `[words](@id)`."
          else t"A slide has an `id`, `markdown` and `syntax <language>`, but not `$last`."

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
  private def checked(path: Text, tel: Tel): List[Block] =
    def at(pointer: List[Text]): Optional[Int] =
      if pointer.nil then Unset
      else tel.locateKey(Telp(pointer)).lay(at(pointer.reverse.skip(1).reverse))(_.line)

    val faults: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error =>
          val pointer: Text = prior.let(_.pointer.encode).or(t"/")
          accrual + (error, pointer, prior.lay(Span.empty)(_.span))

      . protect(Tel.Type.assign(tel, schema))

    faults.items.map: (error, pointer, span) =>
      val message: Text = explain(error, pointer)

      val line: Optional[Int] =
        if span.exists then span.startLine.lay(1)(_.n1)
        else at(pointer.cut(t"/").filter(_ != t"").reverse.skip(1).reverse)

      val location: Block.Location = Block.Location(path, line.or(1))
      val code: fulminate.Bolt = bolt(1).carrying(error.bolt)
      Block.Diagnostic(Tone.Warning, location, List(Block.paragraph(message)), code = code)

  // A presentation read from its text, with a diagnostic for each fault the schema finds and
  // each part not understood; or the diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (Presentation, List[Block]) | Block.Diagnostic =
    import parsing.trackPositions

    recover:
      case error: Tel.Error => Block.Diagnostic.of(error, path, text)

    . protect[(Presentation, List[Block]) | Block.Diagnostic]:
        val tel: Tel = text.read[Tel]
        val (presentation, problems) = Reading(path, tel).presentation
        (presentation, checked(path, tel) + problems)

  // One reading of a presentation's TEL, gathering the problems it finds.
  private class Reading(path: Text, tel: Tel):
    @scala.caps.unsafe.untrackedCaptures
    private var problems: List[Block] = Nil

    private def report(line: Int, message: Text, variant: Int): Unit =
      val location: Block.Location = Block.Location(path, line)
      val paragraphs: List[Block] = List(Block.paragraph(message))
      val diagnostic = Block.Diagnostic(Tone.Warning, location, paragraphs, code = bolt(variant))
      problems = diagnostic :: problems

    private def words(compound: Tel.Compound): List[Text] = Compounds.inline(compound)
    private def source(compound: Tel.Compound): Optional[Text] = Compounds.source(compound)

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
            if Compounds.children(child).nil then
              List(Part.Code(language, source(child).or(words(child).skip(1).join(t" "))))
            else
              val preamble: Preamble = Preamble.of(node, Blocks.phrasing)
              val body: Optional[Text] = this.child(node, t"code")

              if body.absent && preamble.frames.nil
              then report(line, t"This slide's code has nothing in its `code`.", 2)

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
    // the nth `syntax` compound being the nth `syntax` field; and its id, on its line or
    // beneath it.
    private def slide(compound: Tel.Compound, node: Optional[Tel], index: Ordinal): Slide =
      val line: Int = Compounds.line(tel, List(t"slide", index.n0.show))
      val children: List[Tel.Compound] = Compounds.children(compound)

      val codes: List[Tel] = node.lay(Nil)(_.fields(t"syntax").to[List])

      val parts: List[Part] = children.indexed.bind: (child: Tel.Compound, position: Ordinal) =>
        val ordinal: Int = children.keep(position.n0).filter(_.keyword == t"syntax").size
        part(child, codes.at(Ordinal.zerary(ordinal)), line)

      if children.filter(_.keyword != t"id").nil then report(line, t"This slide is empty.", 3)

      val id: Optional[Text] =
        children.filter(_.keyword == t"id").head.let(words(_).head).or(words(compound).head)

      Slide(parts, line, id.let(_.trim).let { id => if id == t"" then Unset else id })

    def presentation: (Presentation, List[Block]) =
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List]

      val title: Optional[Text] = tel.field(t"title").let(_.atomTexts.to[List].join(t" ").trim)
      val nodes: List[Tel] = tel.fields(t"slide").to[List]

      val slides: List[Slide] =
        compounds.filter(_.keyword == t"slide").indexed.map: (compound, index) =>
          slide(compound, nodes.at(index), index)

      val named: Optional[Text] = title.let: (title: Text) => if title == t"" then Unset else title

      val style: Optional[Style] =
        tel.field(t"style").let(_.atomTexts.to[List].join(t" ").trim).let: (file: Text) =>
          if file == t"" then Unset else Style(file, Compounds.line(tel, List(t"style")))

      val script: Optional[Script] =
        tel.field(t"script").let(_.atomTexts.to[List].join(t" ").trim).let: (file: Text) =>
          if file == t"" then Unset else Script(file, Compounds.line(tel, List(t"script")))

      (Presentation(named, slides, style, script), problems.reverse)

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
      Preamble.Frame
        ( code.code, label, code.hold, code.at, Unset, code.preamble.marks,
          typed = code.preamble.typed )

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

    . fold(0)(_ + _)

    frames + (deck.leaves.size - 1).max(0)

  // A schedule which lands a step every `interval` seconds, the first after one interval: a
  // stand-in until a presentation's own timings are read from a file.
  def regular(steps: Int, interval: Double): List[Double] =
    List.range(1, steps + 1).map(_ * interval)

  // The deck, playing itself to the schedule.
  def scheduled(deck: Block.Deck, schedule: List[Double]): Block.Deck =
    deck.copy(schedule = schedule)

  // The frames an animated block steps through, where it has any: its preamble's which have
  // code, then its own code as the last, if it has any.
  private def framed(preamble: Preamble, code: Text): List[List[Preamble.Frame]] =
    if preamble.frames.nil then Nil else
      val described: List[Preamble.Frame] = preamble.frames.filter(_.code.present)
      val frames: List[Preamble.Frame] =
        if code.trim == t"" then described else described + List(Preamble.Frame(code))

      if frames.nil then Nil else List(frames)

  // The animated blocks of a slide's part, each as its frames: a block of code with frames, or
  // the fences with frames of its Markdown.
  private def animations(part: Part): List[List[Preamble.Frame]] = part match
    case code: Part.Code =>
      framed(code.preamble, code.code)

    case Part.Prose(markdown) =>
      Blocks.fences(Parser.parse(markdown)).bind: (fence: Blocks.Fence) =>
        if !fence.info.has(t"fuel") then Nil else
          val (header, code) = Preamble.split(fence.content)

          header.let(Preamble.parse(_)).lay(Nil):
            case preamble: Preamble => framed(preamble, code)
            case _                  => Nil

  // The states of a presentation, in the order it is stepped through them: each slide as it is
  // first shown, and then the later frames of the last of its animated blocks, which is the one
  // the deck steps. The state a slide with such a block is first shown in is that block's
  // first frame, so its id is the slide's or that frame's. There is one more state than the
  // deck has steps.
  def states(presentation: Presentation): List[State] =
    presentation.slides.indexed.bind: (slide: Slide, index: Ordinal) =>
      val first: State = State(slide.id, slide.line, index.n1)

      slide.parts.bind(animations).reverse.head.lay(List(first)): (frames: List[Preamble.Frame]) =>
        frames.indexed.map: (frame: Preamble.Frame, position: Ordinal) =>
          if position.n0 == 0 then first.copy(id = slide.id.or(frame.id))
          else State(frame.id, slide.line, index.n1, position.n1)

  private def failure(path: Text, line: Int, message: Text, variant: Int): Block =
    val location: Block.Location = Block.Location(path, line)
    Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(message)), code = bolt(variant))

  // That the stylesheet a presentation, at `path`, names could not be read.
  def unstyled(path: Text, style: Style): Block =
    failure(path, style.line, t"The stylesheet `${style.file}` could not be read.", 10)

  // That the script a presentation, at `path`, names could not be read.
  def unscripted(path: Text, script: Script): Block =
    failure(path, script.line, t"The script `${script.file}` could not be read.", 11)

  // That the transcript a presentation is given could not be read.
  def unread(file: Text): Block =
    val message: List[Block] = List(Block.paragraph(t"The transcript could not be read."))
    Block.Diagnostic(Tone.Failure, Block.Location(file), message, code = bolt(9))

  // Where a presentation, at `path`, and its transcript, at `file`, do not cohere: the
  // transcript must cue each state which has an id, once, in the order the states are shown,
  // and cue nothing else. Nothing, where they do.
  def cohere(path: Text, presentation: Presentation, file: Text, transcript: Transcript)
  :   List[Block] =

    val all: List[State] = states(presentation)

    val named: List[(Text, State)] = all.bind: (state: State) =>
      state.id.lay(Nil) { (id: Text) => List((id, state)) }

    val ids: List[Text] = named.map(_(0))
    val cued: List[Text] = transcript.cues.map(_.id)

    // A slide whose first state is given one id by the slide and another by its first frame.
    val doubled: List[Block] = presentation.slides.bind: (slide: Slide) =>
      val first: Optional[Text] =
        slide.parts.bind(animations).reverse.head.let(_.head).let(_.id)

      slide.id.lay(Nil): (id: Text) =>
        first.lay(Nil): (other: Text) =>
          if id == other then Nil else
            val message: Text =
              t"The slide is first shown in its animation's first frame, which is one state: it has the id `$id`, and the frame's `$other` is another."

            List(failure(path, slide.line, message, 4))

    val repeated: List[Block] = named.indexed.bind: (pair: (Text, State), index: Ordinal) =>
      val (id, state) = pair

      if !ids.keep(index.n0).has(id) then Nil else
        val message: Text =
          t"The id `$id` of ${state.described} is an earlier state's too: each state's id is its own."

        List(failure(path, state.line, message, 4))

    val stray: List[Block] = transcript.stray.map: (cue: Transcript.Cue) =>
      val message: Text =
        t"The cue `${Transcript.sigil}${cue.id}` is not in what is said: a cue is a link in the transcript's prose, and not in a heading."

      failure(file, cue.line, message, 8)

    val unknown: List[Block] = transcript.cues.bind: (cue: Transcript.Cue) =>
      if ids.has(cue.id) then Nil else
        val message: Text =
          t"No state of the presentation has the id `${cue.id}`, which this cues."

        List(failure(file, cue.line, message, 5))

    val recued: List[Block] = transcript.cues.indexed.bind: (cue: Transcript.Cue, index: Ordinal) =>
      if !ids.has(cue.id) || !cued.keep(index.n0).has(cue.id) then Nil else
        val message: Text =
          t"The state `${cue.id}` is cued here a second time: a state arrives once."

        List(failure(file, cue.line, message, 7))

    val uncued: List[Block] = named.indexed.bind: (pair: (Text, State), index: Ordinal) =>
      val (id, state) = pair

      if cued.has(id) || ids.keep(index.n0).has(id) then Nil else
        val message: Text =
          t"The state `$id`, ${state.described}, is not cued in the transcript `$file`: a link to it there, `[words](${Transcript.sigil}$id)`, says when it arrives."

        List(failure(path, state.line, message, 6))

    // The first cue which is not the one the order of the states has next, once every state
    // is cued once and every cue is a state's.
    def disordered(cues: List[Transcript.Cue], ids: List[Text]): List[Block] = (cues, ids) match
      case (cue :: rest, id :: later) =>
        if cue.id == id then disordered(rest, later) else
          val message: Text =
            t"The state `${cue.id}` is cued here, where `$id` is the next to be shown: the transcript cues the states in their order."

          List(failure(file, cue.line, message, 7))

      case _ =>
        Nil

    val faults: List[Block] = doubled + repeated + stray + unknown + recued + uncued
    if faults.nil then disordered(transcript.cues, ids) else faults

case class Presentation
  ( title:  Optional[Text],
    slides: List[Presentation.Slide],
    style:  Optional[Presentation.Style]  = Unset,
    script: Optional[Presentation.Script] = Unset )
