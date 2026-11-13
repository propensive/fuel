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

import soundness.{Query as _, *}

import dysasymptotics.linearSize
import pyrocosm.WebService
import pyrocosm.{Block, Control, Event, Hints, Inline, Input, Interface, Live, Panel, Tone, Tool}

// The documents and the API as the web front-end `Tool` serves from the daemon: launched once,
// for as long as the daemon lives, when a config says `serve`, and stopped by `fuel quit`. It
// is served behind the machine's Pyrocosm hub, at `/fuel/` on the hub's origin (8090), from a
// port of its own the hub reaches it by; a `port` in a config serves it on that port too.
// `fuel serve` remains the interactive way to serve it, from a terminal. Every tab has a session
// of its own, since every page has a URL of its own: `/doc/<section>/<path>` for a document,
// `/api/<entity>` for an entity.
object Dashboard:
  // The search field, one for every tab, which `handle` knows its events by.
  val searchInput: Input = Input(t"search")

  // The results of a search, as a page; or what is wrong with the query.
  def searchPage(text: Text): List[Block] =
    val apis: Apis = Server.apis
    val api: Api = apis.primary
    val scope: Scope = Scope.document(Server.imports, Server.standard)

    Query.read(text, apis.vocabulary) match
      case error: Query.Error =>
        val message: List[Block] = List(Block.paragraph(error.message.text))
        List(Block.Notice(Tone.Warning, Inline.text(t"Query"), message))

      case query: Query =>
        val outcome: Search.Outcome = apis.search(query, Server.terms)
        ApiPages.search(apis, text, outcome, Resolver(api, scope, Unset, apis), scope)

  // Where the documents on `port` are found.
  def address(port: Int): Text = Fuel.address(port)

  // Served until `stop`, as every tool's web front-end is (`WebService`): every tab has a
  // session of its own, since every page has a URL of its own, and the ticker drives each a
  // few times a second for as long as the front-end runs, which is what notices a new compile.
  val web: Tool.Web = WebService(): () =>
    val dashboard = Dashboard()
    WebService.Session(dashboard.interface, dashboard.handle, () => dashboard.tick())

  // The URL of a document: its section and its path, each segment encoded.
  def docUrl(section: Text, path: Text): Text = Doc.url(section, path)

  // A page rendered when asked for — a dependency's, or one the compile did not render — with
  // its examples highlighted alone, on the compiler's thread.
  private def render(entity: Api.Entity): List[Block] =
    Checking.exclusive:
      val api: Api = Server.apis.of(entity)
      val standard: List[Locator] = api.format.standard(Server.standard)
      val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil), standard)
      val resolver: Resolver = Resolver(api, scope, Unset, Server.apis)

      val scala: Embedding.ScalaCode =
        Embedding.ScalaCode(Checker.tokenized, entity.key, false, Unset, resolver)

      val tables: Tables = Tables(entity.key, resolver)
      ApiPages.page(api, entity, resolver, scope, List(scala, Embedding.JavaCode, tables))

// One tab's dashboard: the navigation of every section's documents and of the API, and the
// page its URL names.
final class Dashboard():
  // What a tab is looking at: a document, by its section and path, or an entity, by its
  // address.
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

  // The tab's searches, done apart from the websocket's messages, so that a burst of
  // keystrokes is searched for once, as its last text, rather than at every text along the way.
  // A search which fails says so in place of its results.
  private val searches: Latest[Text] = Latest: (text: Text) =>
    val page: List[Block] =
      try Dashboard.searchPage(text) catch case error: Exception =>
        val message: List[Block] = List(Block.paragraph(error.toString.tt))
        List(Block.Notice(Tone.Failure, Inline.text(t"Search"), message))

    display(Target.Search(text), page)

  // A page computed apart, shown only if the tab is still showing what it was computed for:
  // a search overtaken by another keystroke, or by a move to another page, is dropped. It is
  // checked and shown under the lock `show` holds, so that a move made meanwhile is never
  // overwritten by the search it left.
  private def display(target: Target, blocks: List[Block]): Unit = synchronized:
    if selected == target then content() = blocks

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
            // The search heads the main column, the full width of it, with its results beneath.
            Panel
              ( Panel.Id(t"content"),
                Panel.Role.Primary,
                Unset,
                content,
                Panel.Priority.Essential,
                controls = List(search),
                hints = Hints(pyrocosm.hints.web.Leading) ) ),
        // Every page is read under the reader's purview, which begins as the project's imports.
        purview = Scope.purview(Server.imports) )

  // The target a path names: the first document for `/`, a document, an entity, or nothing.
  private def target(path: Text): Optional[Target] =
    def decode(text: Text): Text = Text(java.net.URLDecoder.decode(text.s, "UTF-8").nn)

    if path == t"/" || path == t"" then first
    else if path.starts(t"/doc/") then
      val rest: List[Text] = path.skip(5).cut(t"/")
      val within: Text = rest.skip(1).map(decode).join(t"/")
      rest.head.let: (section: Text) => Target.Document(decode(section), within)
    else if path.starts(t"/api/") then
      Server.apis.locate(path.skip(5)).lay(Target.Missing(path)): (entity: Api.Entity) =>
        Target.Entity(entity.address)
    else if path == t"/glossary" then
      Target.Glossary()
    else
      Target.Missing(path)

  private def first: Optional[Target] =
    Server.corpus.let: (corpus: Corpus) =>
      corpus.sections.seek(!_.docs.nil).let: (section: Corpus.Section) =>
        section.docs.head.let: (doc: Doc) => Target.Document(section.name, doc.path)

  def handle(event: Event): Unit = event match
    case Event.Located(path) =>
      selected = target(path)
      show()
      refresh()

    // Every keystroke decorates the field at once, the decoration carrying the version of the
    // text it describes, and asks for a search, which a later keystroke may overtake.
    case Event.Edited(Dashboard.searchInput, text, caret, version) =>
      val completions = Server.api.search.completions(text, caret)

      search.decoration() =
        val tokens = Query.tokens(text, Server.apis.vocabulary)
        Control.Field.Decoration(tokens, completions, false, version = version)

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
        case Target.Entity(key)             => Server.apis.entity(key).present
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

  // The selected page, shown now, unless it is a search, which is shown when it is done.
  private def show(): Unit = synchronized:
    selected.let:
      case Target.Document(section, path) =>
        Server.corpus.let(_.doc(section, path)).let: (doc: Doc) =>
          content() = doc.blocks + doc.problems

      case Target.Entity(key) =>
        val page: List[Block] =
          Server.page(key).or(Server.apis.entity(key).let(Dashboard.render).or(Nil))

        content() = ApiPages.withIssues(page, Server.issues(key))

      case Target.Glossary() =>
        content() = Server.glossary

      case Target.Search(text) =>
        searches(text)

      case Target.Missing(path) =>
        val message: List[Block] = List(Block.paragraph(t"There is no page at $path."))
        content() = List(Block.Notice(Tone.Warning, Inline.text(t"Not found"), message))

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
              val url: Text = Dashboard.docUrl(section.name, doc.path)
              val destination: Inline.Destination = Inline.Destination.Local(url)
              Block.TreeNode(Inline.text(doc.title), Nil, tone, Unset, Unset, destination)

          Block.TreeNode(Inline.text(section.name), children, Unset, Unset, true)

      // The glossary, after the sections, when there is one.
      val glossary: List[Block.TreeNode] =
        if Server.glossary.nil then Nil else
          val tone: Optional[Tone] = if selected == Target.Glossary() then Tone.Accent else Unset
          val destination: Inline.Destination = Inline.Destination.Local(t"/glossary")
          List(Block.TreeNode(Inline.text(t"Glossary"), Nil, tone, Unset, Unset, destination))

      val current: Optional[Api.Entity] = selected match
        case Target.Entity(address) => Server.apis.entity(address)
        case _                      => Unset

      // Each format's entities under its own heading, the current page marked in its own.
      val apiTrees: List[Block] = Server.apis.all.bind: (api: Api) =>
        val here: Optional[Text] = current.let: (entity: Api.Entity) =>
          if entity.format == api.format.id then entity.key else Unset

        if api.entities.isEmpty then Nil
        else List(Block.Heading(3, Inline.text(api.format.title)), ApiPages.tree(api, here))

      List(Block.Tree(documents + glossary)) + apiTrees
