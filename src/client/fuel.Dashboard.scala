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

import soundness.{Query as _, *}

import dysasymptotics.{linearAccess, linearSize}

import pyrocosm.{Action, Block, Control, Event, Hints, Inline, Input, Interface, Live, Panel, Tone, Tool, hints}

// The documents and the API as the web front-end `Tool` serves from the daemon: launched once,
// for as long as the daemon lives, when a config says `serve` (on its `port`, or 8060), and
// stopped by `fuel quit`. `fuel serve` remains the interactive way to serve it, from a
// terminal. Every tab has a session of its own, since every page has a URL of its own:
// `/doc/<section>/<path>` for a document, `/api/<entity>` for an entity.
object Dashboard:
  // The search field, one for every tab, which `handle` knows its events by.
  val searchInput: Input = Input(t"search")

  // The results of a search, as a page; or what is wrong with the query.
  def searchPage(text: Text): List[Block] =
    val api: Api = Server.api
    val scope: Scope = Scope.document(Server.imports)

    Query.read(text) match
      case error: Query.Error =>
        List(Block.Notice(Tone.Warning, Inline.text(t"Query"), List(Block.paragraph(error.message.text))))

      case query: Query =>
        ApiPages.search(api, text, api.search(query), Resolver(api, scope), scope)

  val web: Tool.Web = new Tool.Web:
    def port: Int = 8060

    // The frontend holds the monitor and the error page, which outlive it; vouched pure so it
    // can be stopped from another invocation.
    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var frontend: Optional[pyrocosm.WebFrontend] = Unset

    // Serves until `stop`. A ticker drives every live dashboard a few times a second for as
    // long as the front-end runs: it is what notices a new compile.
    def serve(port: Int)(using Monitor, Probate): Unit =
      import webserverErrorPages.minimalErrorPage

      val serving: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(true)
      val live: juc.ConcurrentHashMap[Dashboard, Boolean] = juc.ConcurrentHashMap()

      val running: pyrocosm.WebFrontend =
        scala.caps.unsafe.unsafeAssumePure:
          pyrocosm.WebFrontend(port)

      frontend = running

      try
        async:
          while serving.get do
            live.keySet.nn.forEach { dashboard => dashboard.nn.tick() }
            snooze(0.25*Second)

        running.serve: () =>
          val dashboard = Dashboard()
          live.put(dashboard, true)

          // The handler lives as long as the session; vouched pure, as the frontend's are.
          val handle: Event -> Unit = scala.caps.unsafe.unsafeAssumePure: (event: Event) =>
            event match
              case Event.Closed => live.remove(dashboard)
              case _            => ()
            dashboard.handle(event)

          (dashboard.interface, handle)
      finally serving.set(false)

    def stop(): Unit = frontend.let(_.stop())

  // The URL of a document: its section and its path, each segment encoded.
  def docUrl(section: Text, path: Text): Text = Doc.url(section, path)

// One tab's dashboard: the navigation of every section's documents and of the API, and the
// page its URL names.
final class Dashboard():
  // What a tab is looking at: a document, by its section and path, or an entity, by its key.
  private enum Target:
    case Document(section: Text, path: Text)
    case Entity(key: Text)
    case Glossary()
    case Missing(path: Text)
    case Search(query: Text)

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var selected: Optional[Target] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var shown: Long = -1L

  @scala.caps.unsafe.untrackedCaptures
  private var lastNavigation: List[Block] = Nil

  val navigation: Live[List[Block]] = Live(Nil)

  // The API search, typed into the navigation panel: a code field, so that the front-end offers
  // its completions as it does Flame's; its text is coloured by the query's grammar.
  val search: Control.Field =
    Control.Field
      ( Dashboard.searchInput,
        Control.Field.Kind.Code(pyrocosm.Language.Scala),
        notification = Control.Field.Notify.Keystrokes,
        placeholder = t"Search the API: name, def, #Type, /pkg, +Result, -Param" )

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
                Panel.Priority.Important,
                controls = List(search) ),
            Panel
              ( Panel.Id(t"content"),
                Panel.Role.Primary,
                Unset,
                content,
                Panel.Priority.Essential ) ),
        // Every page is read under the reader's purview, which begins as the project's imports.
        purview = Scope.purview(Server.imports) )

  // The target a path names: the first document for `/`, a document, an entity, or nothing.
  private def target(path: Text): Optional[Target] =
    def decode(text: Text): Text = Text(java.net.URLDecoder.decode(text.s, "UTF-8").nn)

    if path == t"/" || path == t"" then first
    else if path.starts(t"/doc/") then
      val rest: List[Text] = path.skip(5).cut(t"/")
      rest.head.let { (section: Text) => Target.Document(decode(section), rest.skip(1).map(decode).join(t"/")) }
    else if path.starts(t"/api/") then
      Server.api.locate(path.skip(5)).lay(Target.Missing(path)) { (entity: Api.Entity) => Target.Entity(entity.key) }
    else if path == t"/glossary" then Target.Glossary()
    else Target.Missing(path)

  private def first: Optional[Target] =
    Server.corpus.let: (corpus: Corpus) =>
      corpus.sections.seek(!_.docs.nil).let: (section: Corpus.Section) =>
        section.docs.head.let { (doc: Doc) => Target.Document(section.name, doc.path) }

  def handle(event: Event): Unit = event match
    case Event.Located(path) =>
      selected = target(path)
      show()
      refresh()

    // Every keystroke searches; the decoration carries the version of the text it describes.
    case Event.Edited(Dashboard.searchInput, text, caret, version) =>
      search.decoration() = Control.Field.Decoration(Query.tokens(text), Server.api.search.completions(text, caret), false, version = version)
      selected = if text.trim == t"" then first else Target.Search(text)
      show()
      refresh()

    case Event.Submitted(Dashboard.searchInput, text) =>
      search.decoration() = Control.Field.Decoration()
      if text.trim != t"" then
        selected = Target.Search(text)
        show()
        refresh()

    case _ =>
      ()

  // The serving loop's beat: a new compile replaces the navigation and re-shows the selected
  // page (or the first document, where none is selected or the selected one is gone).
  def tick(): Unit =
    val edition: Long = Server.edition

    if edition != shown then
      shown = edition

      // A page which is gone gives way to the first document; one which was never there stays
      // as it is, saying so.
      val current: Boolean = selected match
        case Target.Document(section, path) => Server.corpus.let(_.doc(section, path)).present
        case Target.Entity(key)             => Server.api(key).present
        case Target.Glossary()              => true
        case Target.Missing(_)              => true
        case Target.Search(_)               => true
        case _                              => false

      if !current then selected = first
      show()
      refresh()

  private def refresh(): Unit = synchronized:
    val navigation0 = navigationBlocks()

    if navigation0 != lastNavigation then
      lastNavigation = navigation0
      navigation() = navigation0

  private def show(): Unit =
    selected.let:
      case Target.Document(section, path) =>
        Server.corpus.let(_.doc(section, path)).let { (doc: Doc) => content() = doc.blocks + doc.problems }

      case Target.Entity(key) =>
        content() = Server.page(key).or(Server.api(key).let(Dashboard.render).or(Nil))

      case Target.Glossary() =>
        content() = Server.glossary

      case Target.Search(text) =>
        content() = Dashboard.searchPage(text)

      case Target.Missing(path) =>
        content() = List(Block.Notice(Tone.Warning, Inline.text(t"Not found"), List(Block.paragraph(t"There is no page at $path."))))

  // The navigation: each section's documents, by title, then the API, the current page
  // marked.
  private def navigationBlocks(): List[Block] =
    val empty: List[Block] =
      List(Block.paragraph(t"No documents yet: run fuel compile in a project."))

    Server.corpus.lay(empty): (corpus: Corpus) =>
      val documents: List[Block.TreeNode] =
        corpus.sections.map: (section: Corpus.Section) =>
          val children: List[Block.TreeNode] =
            section.docs.map: (doc: Doc) =>
              val current: Boolean = selected == Target.Document(section.name, doc.path)
              val tone: Optional[Tone] = if current then Tone.Accent else Unset
              Block.TreeNode(Inline.text(doc.title), Nil, tone, Unset, Unset, Inline.Destination.Local(Dashboard.docUrl(section.name, doc.path)))

          Block.TreeNode(Inline.text(section.name), children, Unset, Unset, true)

      // The glossary, after the sections, when there is one.
      val glossary: List[Block.TreeNode] =
        if Server.glossary.nil then Nil else
          val tone: Optional[Tone] = if selected == Target.Glossary() then Tone.Accent else Unset
          List(Block.TreeNode(Inline.text(t"Glossary"), Nil, tone, Unset, Unset, Inline.Destination.Local(t"/glossary")))

      val api: Api = Server.api
      val current: Optional[Text] = selected match
        case Target.Entity(key) => key
        case _                  => Unset

      val apiTree: List[Block] =
        if api.entities.isEmpty then Nil
        else List(Block.Heading(3, Inline.text(t"API")), ApiPages.tree(api, current))

      List(Block.Tree(documents + glossary)) + apiTree



// A page rendered when asked for — a dependency's, or one the compile did not render — with
// its examples highlighted alone, on the compiler's thread.
extension (dashboard: Dashboard.type)
  private def render(entity: Api.Entity): List[Block] =
    Checking.exclusive:
      val api: Api = Server.api
      val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil))
      val resolver: Resolver = Resolver(api, scope)
      val scala: Embedding.ScalaCode = Embedding.ScalaCode(Checker.tokenized, entity.key, false, Unset, resolver)
      ApiPages.page(api, entity, resolver, scope, List(scala, Embedding.JavaCode))
