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

import environments.javaBaseEnvironment
import pyrocosm.{Digests, Filing, Shares, Tool}

// What Fuel knows of the API it last compiled, for another tool on this machine to show beside
// a name — Flame's completions, say: each documented entity's address (`gossamer.Text#length`)
// and its `info` line, as plain text. It is a share (`pyrocosm.Shares`): a RaTEL image in the
// project's cache, `$XDG_CACHE_HOME/fuel/<digest>/share/help.ratel`, which a `fuel compile`
// replaces whole and any other tool maps into memory, finding one entry by its address without
// reading the rest. Nothing is served and nothing announced by it: a tool reads the file
// whether or not Fuel is running, and sees a newer one the next time it looks.
//
// The image carries its schema, and a reader asks for `entry`, `address` and `info` by those
// keywords, so a member added here is one a reader may ignore.
//
// Every entity's page — its path beneath the site's root, by the same address — is a share of
// its own beside it, `pages.ratel`, and not a second member of the index: the keys of one
// record's children are distinct whichever member they are children of (TEL's rule of key
// uniqueness), and an entry and a page have one address. A reader asks it for `page`, `address`
// and `url`.
//
// Whether a page can be opened is another matter, which neither says: the pages are there only
// while Fuel serves them. That is said by a third share, `site.ratel`, which is there for
// exactly as long as this process serves the project's pages, and says where: a reader which
// finds it, and finds the process it names alive, may link an address to the site's address
// followed by its page's `url`.
object Help:
  case class Index(entry: List[Entry])
  case class Entry(@key address: Text, info: Text)

  case class Pages(page: List[Page])
  case class Page(@key address: Text, url: Text)

  // Where the pages are served: the address of the site's root as a browser on this machine
  // opens it, without the `/` it ends in, so that a page's `url` follows it; and the process
  // serving them, by which a reader knows an announcement a crash left behind from a live one.
  case class Site(address: Text, pid: Text)

  // The shares' names, under the project's cache.
  val share: Text = t"help"
  val pagesShare: Text = t"pages"
  val siteShare: Text = t"site"

  // The index of the model `Server` holds: every entity with an `info`, rendered to plain text
  // through its own scope, so that a sigilled reference reads as a name. An address is a key,
  // so the first entity to have one keeps it.
  def index(): Index =
    val apis: Apis = Server.apis

    // Every format's entities, each by its address.
    val entries: List[Entry] = apis.all.bind: (api: Api) =>
      api.entities.values.bind: (entity: Api.Entity) =>
        entity.doc.let(_.info).lay(Nil: List[Entry]): (info: Text) =>
          val standard: List[Locator] = api.format.standard(Server.standard)
          val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil), standard)
          val resolver: Resolver = Resolver(api, scope, Unset, apis)
          List(Entry(entity.address, Blocks.plain(Blocks.inline(info, resolver))))

    val distinct: Map[Text, Entry] =
      Map(entries.reverse.map { (entry: Entry) => entry.address -> entry }*)

    Index(distinct.values.to[List])

  // Whether an entity's page is found at its URL: the dashboard answers a path beneath `/api/`
  // with the entity `locate` finds for it, whatever its kind — a parameter and a hidden entity
  // have pages too, though nothing leads to them — so an entity is without one only if its URL
  // leads to another entity, or to none.
  def paged(apis: Apis, entity: Api.Entity): Boolean =
    apis.locate(entity.url.skip(5)).lay(false)(_.address == entity.address)

  // The pages of the model `Server` holds: every entity's, whether or not anything is said of
  // it, each by its entity's address, which the first entity to have it keeps here too.
  def pages(): Pages =
    val apis: Apis = Server.apis

    val pages: List[Page] = apis.all.bind: (api: Api) =>
      api.entities.values.bind: (entity: Api.Entity) =>
        if paged(apis, entity) then List(Page(entity.address, entity.url)) else Nil

    val distinct: Map[Text, Page] =
      Map(pages.reverse.map { (page: Page) => page.address -> page }*)

    Pages(distinct.values.to[List])

  // Where the index of the project at `root` is published, and its pages, and where its site
  // is announced.
  def file(root: Text): Optional[Path on Linux] = Shares.path(Fuel.name, root, share)
  def pagesFile(root: Text): Optional[Path on Linux] = Shares.path(Fuel.name, root, pagesShare)
  def siteFile(root: Text): Optional[Path on Linux] = Shares.path(Fuel.name, root, siteShare)

  // Publishes the index and the pages for the project at `root`; `false` if either could not
  // be written, which leaves the last one in place. The project is the one whose pages are
  // served from now on, so its site is announced too, if the pages are being served.
  def publish(root: Text): Boolean =
    val indexed: Boolean = file(root).lay(false)(Shares.publish(_, index()))
    val listed: Boolean = pagesFile(root).lay(false)(list(root, _))
    compiled(root)
    indexed && listed

  // The project and the pages this process published last, as a digest of both. The image of
  // every entity's page takes seconds to make where the entities are a JDK's and its
  // dependencies', hundreds of thousands of them, and making it is how `Shares.publish` knows
  // an image is the one already there; so a compile which leaves every page as it was — most
  // do, changing documents and not the API — is known here first, by the digest, and makes none.
  @scala.caps.unsafe.untrackedCaptures
  private var listed: Optional[Text] = Unset

  private def list(root: Text, file: Path on Linux): Boolean = synchronized:
    val pages: Pages = this.pages()
    val lines: List[Text] = pages.page.map { (page: Page) => t"${page.address} ${page.url}" }
    val digest: Text = Digests.of(root, lines.join(t"\n"))

    if listed.lay(false)(_ == digest) && Filing.exists(file) then true else
      val published: Boolean = Shares.publish(file, pages)
      listed = if published then digest else Unset
      published

  // Announces at `file` that this process serves a site at `address`; `false` if it could not
  // be written.
  def announce(file: Path on Linux, address: Text): Boolean =
    Shares.publish(file, Site(address, ProcessHandle.current().nn.pid().toString.tt))

  // Withdraws the announcement at `file`: a share which is not there is one nobody serves.
  def withdraw(file: Path on Linux): Unit = Filing.wipe(file)

  // The address the site is served at, while it is, and which time of serving this is; the
  // project compiled last, whose pages are the ones served; and the project announced, which
  // is that one for as long as the site is served.
  @scala.caps.unsafe.untrackedCaptures
  private var serving: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private var session: Long = 0L

  @scala.caps.unsafe.untrackedCaptures
  private var project: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private var announced: Optional[Text] = Unset

  // Makes what is announced what is so: the project compiled last, at the address served, and
  // no other. The daemon holds one project's model, so a project compiled before it is no
  // longer served, and its announcement is withdrawn.
  private def settle(): Unit =
    val current: Optional[Text] = serving.let { _ => project }

    if announced != current then
      announced.let(siteFile(_)).let(withdraw(_))
      announced = Unset

    serving.let: (address: Text) =>
      current.let: (root: Text) =>
        if siteFile(root).lay(false)(announce(_, address)) then announced = root

  // The site is served at `address` from now on. The number it answers with names this time of
  // serving, for `stopped`.
  def started(address: Text): Long = synchronized:
    serving = address
    session += 1L
    settle()
    session

  // The serving `started` numbered has stopped, unless it already had: the front-end is
  // stopped from one thread and its serving ends on another, perhaps after it has been started
  // again, and the announcement of a later serving is not the earlier one's to withdraw.
  def stopped(number: Long): Unit = synchronized:
    if serving.present && session == number then
      serving = Unset
      settle()

  // Whatever serving is under way has stopped.
  def stopped(): Unit = synchronized(stopped(session))

  // The project at `root` has been compiled, and its pages are the ones served.
  def compiled(root: Text): Unit = synchronized:
    project = root
    settle()

  // The site's address on `port`, as Fuel says it when it starts serving, without the `/` it
  // ends in: the machine's hub's, `http://localhost:8090/fuel`, unless a configuration gives
  // the front-end a port of its own, and then that port's.
  def address(port: Int): Text =
    val said: Text = Fuel.address(port)
    if said.ends(t"/") then said.keep(said.length - 1) else said

  // A web front-end which announces the site for as long as it is served, however it comes to
  // be — a configuration's `serve` or the command — and however it ends: `stop` withdraws the
  // announcement itself, since `fuel quit` ends the process without waiting for the serving
  // to return.
  def announcing(web: Tool.Web): Tool.Web = new Tool.Web:
    override def keyword: Text = web.keyword
    override def portKeyword: Text = web.portKeyword
    def port: Int = web.port
    def serve(port: Int)(using Monitor, Probate): Unit = serve(port, _ => Unset)

    override def serve(port: Int, settings: Text => Optional[Text])(using Monitor, Probate)
    :   Unit =

      val number: Long = started(address(port))
      try web.serve(port, settings) finally stopped(number)

    def stop(): Unit =
      stopped()
      web.stop()
