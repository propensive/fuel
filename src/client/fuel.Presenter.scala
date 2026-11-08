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

import palettes.solarizedDarkGaugePalette
import pyrocosm.{Block, Hints, Inline, Interface, Live, Panel, TerminalRenderer}
import tableStyles.thickTableStyle
import textMetrics.uniformMetric

// A presentation in a browser: its deck, compiled as `fuel show` compiles a document — fragments
// checked against the configured classpath, where there is one, or highlighted alone — in an
// interface of one panel, served until stopped.
object Presenter:
  val port: Int = 8061

  private def exists(entry: Text): Boolean =
    java.nio.file.Files.exists(java.nio.file.Path.of(entry.s))

  private def read(path: Text): Optional[Text] =
    try Text(java.nio.file.Files.readString(java.nio.file.Path.of(path.s)).nn)
    catch case _: Exception => Unset

  // How often a step lands when a presentation plays itself, in seconds: a stand-in for the
  // timings a presentation will read from a file.
  val interval: Double = 4.0

  // The presentation at `path`, read and compiled, as its title and its deck, with what the
  // compile reported printed at the terminal's width; `Unset` if it cannot be read or is not
  // TEL, which is printed too. An `autoplay` deck is given a schedule, a step every `interval`
  // seconds, and says how many steps it has.
  def prepare(config: Workspace.Config, path: Text, width: Int, autoplay: Boolean = false)
    (using Stdio)
  :   Optional[(Text, Block)] =

    val renderer = TerminalRenderer()

    def print(blocks: List[Block]): Unit =
      renderer.blocks(blocks, width).each(Out.println(_))

    read(path).lay(Unset): (text: Text) =>
      Presentation.read(path, text) match
        case diagnostic: Block.Diagnostic =>
          print(List(diagnostic))
          Unset

        case (presentation: Presentation, problems: List[Block] @unchecked) =>
          val (deck, raised) = compile(config, path, presentation)
          print(problems + raised)
          val name: Text = path.cut(t"/").reverse.head.or(path)
          (presentation.title.or(name), if autoplay then conducted(deck) else deck)

  // The deck with its schedule: a step every `interval` seconds, counted and said.
  private def conducted(deck: Block)(using Stdio): Block = deck match
    case deck: Block.Deck =>
      val steps: Int = Presentation.steps(deck)
      val seconds: Text = interval.toString.tt
      Out.println(t"fuel: autoplaying ${steps.toString.tt} steps, one every $seconds s")
      Presentation.scheduled(deck, Presentation.regular(steps, interval))

    case other =>
      other

  private def compile(config: Workspace.Config, path: Text, presentation: Presentation)
  :   (Block, List[Block]) =

    def build(checker: Checker): (Block, List[Block]) =
      val scala = Embedding.ScalaCode(checker, path, config.session)
      val tables: Tables = Tables(path)
      val deck: Block = Presentation.deck(presentation, scala, Linker.none, tables)
      (deck, scala.problems + tables.problems)

    val checked: Boolean = !config.classpath.nil && config.classpath.all(exists(_))

    if !checked then build(Checker.tokenized)
    else
      try Checking.session(config.classpath, config.options)(build)
      catch case error: Throwable => build(Checker.tokenized)

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
