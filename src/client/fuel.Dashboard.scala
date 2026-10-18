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

import java.util.concurrent as juc

import soundness.*

import pyrocosm.{Action, Block, Event, Hints, Inline, Interface, Live, Panel, Tone, Tool, hints}

// The documents as the web front-end `Tool` serves from the daemon: launched once, for as long
// as the daemon lives, when a config says `serve` (on its `port`, or 8060), and stopped by
// `fuel quit`. `fuel serve` remains the interactive way to serve it, from a terminal.
object Dashboard:
  val web: Tool.Web = new Tool.Web:
    def port: Int = 8060

    // The frontend holds the monitor and the error page, which outlive it; vouched pure so it
    // can be stopped from another invocation.
    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var frontend: Optional[pyrocosm.WebFrontend] = Unset

    // Serves until `stop`. A ticker drives the dashboard a few times a second for as long as
    // the front-end runs: it is what notices a new compile.
    def serve(port: Int)(using Monitor, Probate): Unit =
      import webserverErrorPages.minimalErrorPage

      val dashboard = Dashboard()
      val serving: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(true)

      val running: pyrocosm.WebFrontend =
        scala.caps.unsafe.unsafeAssumePure(pyrocosm.WebFrontend(port))

      frontend = running

      try
        async:
          while serving.get do
            dashboard.tick()
            snooze(0.25*Second)

        running.run(dashboard.interface)(dashboard.handle)
      finally serving.set(false)

    def stop(): Unit = frontend.let(_.stop())

// The dashboard: every section's documents to choose from, and the chosen document's blocks.
final class Dashboard():
  // A document in a section, as the navigation offers it.
  private case class Target(section: Text, path: Text)

  private val docActions: juc.ConcurrentHashMap[Target, Action] = juc.ConcurrentHashMap()

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var selected: Optional[Target] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var shown: Long = -1L

  @scala.caps.unsafe.untrackedCaptures
  private var lastNavigation: List[Block] = Nil

  val navigation: Live[List[Block]] = Live(Nil)

  val content: Live[List[Block]] =
    Live(List(Block.paragraph(t"Run `fuel compile` in a project, and its documents appear here.")))

  val interface: Interface =
    Interface
      ( Inline.text(t"fuel"),
        List
          ( Panel
              ( Panel.Id(t"documents"),
                Panel.Role.Navigation,
                Inline.text(t"Documents"),
                navigation,
                Panel.Priority.Important ),
            Panel
              ( Panel.Id(t"content"),
                Panel.Role.Primary,
                Unset,
                content,
                Panel.Priority.Essential ) ) )

  private def docAction(target: Target): Action =
    docActions.computeIfAbsent(target, _ => Action(t"${target.section}/${target.path}")).nn

  private def keyOf(action: Action): Optional[Target] =
    val entries = docActions.entrySet.nn.iterator.nn

    def recur(): Optional[Target] =
      if !entries.hasNext then Unset else
        val entry = entries.next.nn
        if entry.getValue == action then entry.getKey.nn else recur()

    recur()

  def handle(event: Event): Unit = event match
    case Event.Pressed(action) =>
      keyOf(action).let: (target: Target) =>
        selected = target
        show()
        refresh()

    case _ =>
      ()

  // The serving loop's beat: a new compile replaces the navigation and re-shows the selected
  // document (or the first, where none is selected or the selected one is gone).
  def tick(): Unit =
    val edition: Long = Server.edition

    if edition != shown then
      shown = edition

      Server.corpus.let: (corpus: Corpus) =>
        def exists(target: Target): Boolean = corpus.doc(target.section, target.path).present
        val current: Boolean = selected.lay(false)(exists)

        if !current then
          selected =
            corpus.sections.seek(!_.docs.nil).let: (section: Corpus.Section) =>
              section.docs.prim.let: (doc: Doc) => Target(section.name, doc.path)

      show()
      refresh()

  private def refresh(): Unit = synchronized:
    val navigation0 = navigationBlocks()

    if navigation0 != lastNavigation then
      lastNavigation = navigation0
      navigation() = navigation0

  private def show(): Unit =
    selected.let: (target: Target) =>
      Server.corpus.let(_.doc(target.section, target.path)).let: (doc: Doc) =>
        content() = doc.blocks

  private def marked(chosen: Boolean, label: Text): List[Inline] =
    if chosen then List(Inline.Toned(Tone.Accent, List(Inline.Emphasis(Inline.text(label)))))
    else Inline.text(label)

  // Every section's documents, by title, the selected one marked.
  private def navigationBlocks(): List[Block] =
    val empty: List[Block] =
      List(Block.paragraph(t"No documents yet: run fuel compile in a project."))

    Server.corpus.lay(empty): (corpus: Corpus) =>
      corpus.sections.bind[List[Block], Block, List[Block]]: (section: Corpus.Section) =>
        val items: List[Block.Item] =
          section.docs.map: (doc: Doc) =>
            val target = Target(section.name, doc.path)
            val label: List[Inline] = marked(selected == target, doc.title)
            Block.Item(List(Block.Paragraph(label)), docAction(target))

        List(Block.Heading(3, Inline.text(section.name)), Block.Listing(false, items))
