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
import palettes.solarizedDarkGaugePalette
import pyrocosm.{Block, Hints, Inline, Interface, Live, Panel, TerminalRenderer}
import tableStyles.thickTableStyle
import textMetrics.uniformMetric

// A presentation in a browser: its deck, compiled as `fuel show` compiles a document — fragments
// checked against the configured classpath, where there is one, or highlighted alone — in an
// interface of one panel, served until stopped.
object Presenter:
  val port: Int = 8061

  // How often a step lands when a presentation plays itself, in seconds: a stand-in for the
  // timings a presentation will read from a file.
  val interval: Double = 4.0

  // Why a presentation cannot be shown: its file, or its transcript's, cannot be read or is
  // not TEL; its transcript does not cue its states, each once and in order; the alignment it
  // is to be timed by cannot be read or does not correspond to its transcript; or the aligner
  // could not be installed or run on the recording.
  enum Problem:
    case Unreadable, Incoherent, Misaligned, Unaligned

  // The transcript a presentation at `path` names, read from its file, which is beside the
  // presentation's unless it is named from the root: with the file as it is named, and where
  // the two do not cohere. `Unset` where the presentation names none, and the diagnostic where
  // its file cannot be read.
  def transcript(path: Text, presentation: Presentation)
  :   Optional[(Transcript, List[Block])] | Block =

    presentation.transcript.let: (reference: Presentation.Reference) =>
      val segments: List[Text] = path.cut(t"/")
      val directory: Text = segments.keep(segments.size - 1).join(t"/")

      val file: Text =
        if reference.file.starts(t"/") then reference.file else t"$directory/${reference.file}"

      readText(file).lay[Optional[(Transcript, List[Block])] | Block]
        ( Presentation.unread(path, reference, file) ): (markdown: Text) =>
            val transcript: Transcript = Transcript.read(markdown)
            (transcript, Presentation.cohere(path, presentation, file, transcript))

  // The presentation at `path`, read and compiled, as its title and its deck, with what the
  // compile reported printed at the terminal's width; or the problem, which is printed too.
  // A presentation whose transcript does not cohere with it is a problem, and is not shown.
  // An `autoplay` deck is given a schedule — the times the cues of its states are spoken, from
  // `align`, a recording of the transcript read aloud (aligned by Montreal Forced Aligner,
  // which fuel installs on first use) or a forced alignment already made; or else a step every
  // `interval` seconds — and says how many steps it has.
  def prepare
    ( config:   Workspace.Config,
      path:     Text,
      width:    Int,
      autoplay: Boolean        = false,
      align:    Optional[Text] = Unset )
    (using Stdio, Environment)
  :   (Text, Block) | Problem =

    val renderer = TerminalRenderer()

    def print(blocks: List[Block]): Unit =
      renderer.blocks(blocks, width).each(Out.println(_))

    readText(path).lay[(Text, Block) | Problem](Problem.Unreadable): (text: Text) =>
      Presentation.read(path, text) match
        case diagnostic: Block.Diagnostic =>
          print(List(diagnostic))
          Problem.Unreadable

        case (presentation: Presentation, problems: List[Block] @unchecked) =>
          (transcript(path, presentation): @unchecked) match
            case unread: Block =>
              print(problems + List(unread))
              Problem.Unreadable

            case (_, faults: List[Block] @unchecked) if !faults.nil =>
              print(problems + faults)
              Problem.Incoherent

            case said =>
              val transcript: Optional[Transcript] = said match
                case (transcript: Transcript, _) => transcript
                case _                           => Unset

              val (deck, raised) = compile(config, path, presentation)
              print(problems + raised)
              val name: Text = path.cut(t"/").reverse.head.or(path)
              val title: Text = presentation.title.or(name)

              if !autoplay then (title, deck)
              else conducted(config, deck, presentation, transcript, align) match
                case problem: Problem => problem
                case deck: Block      => (title, deck)

  // The project's cache directory, where alignments are kept: beside its `.fuel` directory
  // where there is no cache directory, as the compiled sources are.
  private def cache(config: Workspace.Config)(using Environment): Text =
    Fuel.projectCache(config.root).let { (cache: Path on Linux) => cache.encode }
    . or(t"${config.root}/.fuel")

  // A file given to `--align`: a recording, aligned to the transcript by MFA (from the cache
  // when it has been before), or an alignment already made; either way, the alignment's text.
  private def alignment(config: Workspace.Config, transcript: Transcript, file: Text)
    (using Stdio, Environment)
  :   Text | Problem =

    if !onDisk(file) then
      Out.println(t"fuel: ${ForcedAlignment.bolt(1).label} $file could not be read")
      Problem.Unreadable
    else if !Aligner.isAudio(file) then readText(file).or(Problem.Unreadable)
    else Aligner.home match
      case home: Aligner.Home =>
        Aligner.ensure(home) match
          case problem: Text =>
            val label: Text = ForcedAlignment.bolt(2).label
            Out.println(t"fuel: $label Montreal Forced Aligner could not be installed: $problem")
            Aligner.manual(home).each { (line: Text) => Out.println(t"fuel: $line") }
            Problem.Unaligned

          case _ =>
            Aligner.align(home, file, transcript.text, cache(config)) match
              case Aligner.Outcome.Aligned(json, cached) =>
                if cached then Out.println(t"fuel: using the alignment of $file made before")
                json

              case Aligner.Outcome.Failed(message) =>
                Out.println(t"fuel: ${ForcedAlignment.bolt(3).label} $message")
                Problem.Unaligned

      case _ =>
        val label: Text = ForcedAlignment.bolt(4).label
        val message: Text = t"there is no cache directory to install Montreal Forced Aligner in"
        Out.println(t"fuel: $label $message")
        Problem.Unaligned

  // The deck with its schedule, counted and said: its states' times in the alignment, by the
  // transcript's cues; or a step every `interval` seconds.
  private def conducted
    ( config:       Workspace.Config,
      deck:         Block,
      presentation: Presentation,
      transcript:   Optional[Transcript],
      align:        Optional[Text] )
    (using Stdio, Environment)
  :   Block | Problem =

    deck match
      case deck: Block.Deck =>
        val steps: Int = Presentation.steps(deck)
        val count: Text = steps.show

        // Each `Text | …` below is matched `@unchecked`, since the exhaustivity check does not see
        // that a `Text` case covers `Text`.
        def unsaid: Problem =
          val label: Text = ForcedAlignment.bolt(6).label
          val message: Text = t"the presentation names no transcript, which timing it by an alignment needs"
          Out.println(t"fuel: $label $message")
          Problem.Misaligned

        def timed(file: Text, transcript: Transcript): Block | Problem =
          (alignment(config, transcript, file): @unchecked) match
            case problem: Problem => problem
            case content: Text => (ForcedAlignment.read(content): @unchecked) match
              case problem: Text =>
                val label: Text = ForcedAlignment.bolt(5).label
                Out.println(t"fuel: $label $file is not an alignment fuel can read: $problem")
                Problem.Misaligned

              case words: List[ForcedAlignment.Word] @unchecked =>
                ForcedAlignment.schedule(Presentation.states(presentation), transcript, words) match
                  // Each failure is labelled with its code; a line of a difference shown
                  // beneath one, which is indented, is not.
                  case ForcedAlignment.Outcome.Failed(code, messages) =>
                    messages.each: (message: Text) =>
                      if message.starts(t" ") then Out.println(t"fuel: $message")
                      else Out.println(t"fuel: ${code.label} $message")
                    Problem.Misaligned

                  case ForcedAlignment.Outcome.Timed(schedule, comparison) =>
                    val spoken: Text = words.size.show
                    Out.println(t"fuel: autoplaying $count steps, timed by $spoken words of $file")

                    if comparison.differing > 0 then
                      val differ: Text = comparison.differing.show
                      Out.println(t"fuel: the transcript and the alignment differ in $differ words:")
                      comparison.shown.each { (hunk: Text) => Out.println(t"fuel:   $hunk") }

                    val times: Text = schedule.size.show
                    if schedule.size != steps
                    then Out.println(t"fuel: the deck has $count steps but its states give $times times")

                    Presentation.scheduled(deck, schedule)

        align match
          case file: Text =>
            transcript.lay(unsaid) { (transcript: Transcript) => timed(file, transcript) }

          case _ =>
            val decimalizer = Decimalizer(decimalPlaces = 1, exponentThreshold = Unset)
            val seconds: Text = decimalizer.decimalize(interval)
            Out.println(t"fuel: autoplaying $count steps, one every $seconds s")
            Presentation.scheduled(deck, Presentation.regular(steps, interval))

      case other =>
        other

  private def compile(config: Workspace.Config, path: Text, presentation: Presentation)
  :   (Block, List[Block]) =

    def build(checker: Checker): (Block, List[Block]) =
      val scala = Blocks.scala(checker, path, config.session)
      val tables: Tables = Tables(path)
      val deck: Block = Presentation.deck(presentation, scala, Linker.none, tables)
      (deck, scala.problems + tables.problems)

    val checked: Boolean = !config.classpath.nil && config.classpath.all(onDisk(_))

    if !checked then build(Checker.tokenized)
    else sheltered(Checking.session(config.classpath, config.options)(build)): _ =>
      build(Checker.tokenized)

  // The deck's interface: its title, and the deck as the one panel.
  def interface(title: Text, deck: Block): Interface =
    val content: Live[List[Block]] = Live(List(deck))
    val priority: Panel.Priority = Panel.Priority.Essential
    val panel: Panel = Panel(Panel.Id(t"slides"), Panel.Role.Primary, Unset, content, priority)

    // On a stage: the deck alone, filling the window and scaled to it.
    Interface(Inline.text(title), List(panel), hints = Hints(pyrocosm.hints.web.Stage))

  // A frontend serving the interface on the port, which `run` serves until `stop`. Vouched
  // pure, as the dashboard's is: it outlives the invocation which starts it.
  def frontend(port: Int)(using Monitor, Probate): pyrocosm.WebFrontend =
    import webserverErrorPages.minimalErrorPage
    scala.caps.unsafe.unsafeAssumePure(pyrocosm.WebFrontend(port))
