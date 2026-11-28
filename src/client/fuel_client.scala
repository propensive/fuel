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

import scala.caps

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Format as _, Compilation as _, Query as _, *}

import dysasymptotics.{linearAccess, linearSize}
import interpreters.posixInterpreter
import probates.cancelProbate
import pyrocosm.Boards.{show as showBoard}
import pyrocosm.Tool
import pyrocosm.standardRendering.given
import pyrocosm.{ambient, Attend, Block, Board, Digests, Filing, Notices, TerminalRenderer, Token,
  Tone}
import soundness.collationComparable
import sortingAlgorithms.timsort
import collations.codepointCollation
import threads.platformThreads

// The exit statuses fuel can terminate with, as objects (soundness#1811), so that the union of
// an `execute` block's result type documents them in the manpage's EXIT STATUS section.
object NoDocuments extends Status(3, t"no document directory is configured")
object NoDocument extends Status(5, t"the document could not be read")
object CompileFailed extends Status(4, t"a code fragment did not do as its preamble said")
object DaemonFailed extends Status(6, t"the daemon failed while carrying out the command")
object Misaligned extends Status(8, t"the transcript and its alignment do not correspond")
object Unaligned extends Status(9, t"the aligner could not be installed or run on the recording")
object Incoherent extends Status(10, t"the transcript does not cue the presentation's states, each once and in order")
object SyncFailed extends Status(7, t"the issues of an attached tracker could not be fetched")

// What ends an invocation which a failure escaped: an `Error`, such as the daemon running out of
// memory, which a silent backstop would end with status 2 and no word, as though nothing were
// wrong. It is reported on the standard error, with where it was thrown, after its code,
// `↯/f.27.4`, carrying the code of the error which escaped where it has one; and the invocation
// ends with a status of its own.
given fuelBackstop: Backstop = new Backstop:
  def handle(error: Throwable)(using Stdio): Exit =
    safely:
      val cause: fulminate.Bolt = error match
        case error: fulminate.Error => error.bolt
        case _                      => fulminate.Bolt.Uncoded

      val label: Text = FuelBolts(FuelBolts.client, FuelBolts.daemon).carrying(cause).label
      val message: Text = t"the daemon failed while carrying out the command"
      Err.println(t"fuel: $label $message: ${described(error)}")
      frames(error).keep(8).each { (frame: Text) => Err.println(t"    at $frame") }

    DaemonFailed.exit

// Fuel's user interface, in one namespace: its subcommands, flags and settings.
object ui:
  val Compile = Subcommand("compile", "compile the configured directories of Markdown documents")
  val Show    = Subcommand("show", "render one document in the terminal")
  val Present = Subcommand("present", "show a presentation's slides in a browser until Ctrl+C")

  val Search  =
    Subcommand("search", "search the compiled API by name, kind, package, result or parameter type")

  val Sync   = Subcommand("sync", "attach a tracker's issues to the API documentation, and fetch them")

  val Transcript =
    Subcommand("transcript", "print a presentation's transcript, and check that it cues the presentation's states")
  val Github = Subcommand("github", "a GitHub repository, as owner/repo")

  // Each is read from the cascade every Pyrocosm tool shares: the flag, the `fuel.*` property,
  // the `FUEL_*` variable, the repository's `.pyrocosm/fuel/config.tel` and the user's. A
  // repeated keyword's atoms arrive joined by `:`.
  val Classpath =
    Setting[Text](t"classpath", t"the jars and directories embedded code is compiled against")

  val Document =
    Setting[Text](t"document", t"a directory of Markdown documents, relative to the project root")

  val ScalacOption =
    Setting[Text](t"scalacOption", t"a compiler option for the fragments, as `-deprecation`")

  val Session =
    Setting[Boolean]
      ( t"session",
        t"compile each document's fragments as one session, each following the ones before" )

  val Fixtures =
    Setting[Text]
      ( t"fixtures",
        t"a directory of `<section>/<document>.scala` (or `<document>.scala`) fixtures, " +
          t"compiled before each document's fragments" )

  val Source =
    Setting[Text]
      ( t"source", t"a directory of the project's own Scala sources, whose API is documented" )

  val Overlay =
    Setting[Text](t"overlay", t"a directory of TEL overlays, the API's documentation")

  val Schema =
    Setting[Text](t"schema", t"a TEL schema, documented as an API is, at `/api/tels/<name>`")

  val Imports =
    Setting[Text]
      ( t"imports",
        t"a package or object whose members a document's references resolve through, as " +
          t"`soundness`" )

  val Output =
    Setting[Text]
      ( t"output",
        t"a directory `fuel compile` writes its output into, as BinTEL, compiling only what " +
          t"changed" )

  val Force =
    Flag[Unit]
      ( "force",
        false,
        List('f'),
        "compile everything again, ignoring what the output directory keeps" )

  val Section =
    Setting[Text]
      ( t"section",
        t"a section of documents, its name and its directory, as `guide doc`; beneath it, its " +
          t"`metadata`, each `overlay` and each `glossary`, within that directory" )

  val Store =
    Setting[Text]
      ( t"store",
        t"the directory an Orthant store is in, whose documents are read in each of its " +
          t"languages; beneath it, an `at` for each other axis's value, as `at release=1.2.0`" )

  val Autoplay =
    Flag[Unit]
      ( "autoplay",
        false,
        Nil,
        "play the presentation by itself, to a schedule of one step every four seconds" )

  val Align =
    Flag[Text]
      ( "align",
        false,
        List(t"alignment"),
        "time the autoplay by a recording of the transcript being read, or by a forced alignment of one (MFA's TextGrid, JSON or CSV)" )

  val PresentPort =
    Setting[Text](t"presentPort", t"the port `fuel present` serves on (8061 by default)")

  val Glossary =
    Setting[Text](t"glossary", t"a TEL file of `define`d terms, which a document names in brackets")

  val CheckLinks =
    Setting[Boolean]
      ( t"checkLinks", t"fetch every link to the web, and fail on one which does not answer" )

// Fuel as a Pyrocosm tool: `about`, `install`, `quit` and `--version` come from `Tool`, as does
// the search for `.pyrocosm/fuel/config.tel`, and the dashboard a config's `serve` launches.
// Capitalised, since a `val fuel` would clash with the launcher's `object fuel`.
val Fuel: Tool =
  Tool
    ( t"fuel",
      prose = t"Fuel compiles a project's Markdown documentation, with its embedded code " +
        t"highlighted, and serves it on the web; in time it will check that code against " +
        t"the project's classpath, animate it, and document the project's API.",
      web      = fuel.Dashboard.web )

// The client's command dispatch. The `@main` entry point and burdock's `externalize` wrapper
// live alone in the `launcher` module, which depends on this module as a PUBLISHED artifact.
// An object rather than a top-level method, so that it can declare its use of the global
// `threads` the completions executive supervises under and the `Watcher` the daemon watches
// its state files with.
object Client uses parasite.threads, surveillance.Watcher:
  def run(): Unit =
    given executive: exoskeleton.executives.CompletionsExecutive =
      caps.unsafe.unsafeAssumePure(executives.completionsExecutive)

    cli:
      Fuel.standard:
        dispatch()

// The dispatch is a method rather than the body of the `cli` block: that block runs on a
// connection's task and may retain only what is durable.
private def dispatch()(using Cli, Resident, Monitor, Environment, Interpreter, Configurator)
:   Execution =

  val directory: Text = summon[Cli].workingDirectory.directory()

  // Whether the client is a terminal, which decides how Ctrl+C reaches `serve`.
  val tty: Boolean = summon[Resident].cliInput == ethereal.Terminus.Terminal

  arguments match
    // `fuel present <file>`: a presentation's slides in a browser, until Ctrl+C.
    case ui.Present() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)

      // Registered here, so that it is offered and documented; its value is read in the
      // execution, where a flag's value is a value.
      ui.Align()

      // The operand is the first word which is not a flag, nor a flag's value.
      val words: List[Text] = rest.map(_())

      val operand: Optional[Text] = words.indexed.filter: (word: Text, index: Ordinal) =>
        val flags: List[Text] = List(t"--align", t"--alignment")
        val valued: Boolean = before(words, index).lay(false)(flags.has(_))
        !word.starts(t"-") && !valued

      . head.let(_(0))

      val autoplay0: Boolean = ui.Autoplay().present

      val port: Int = ui.PresentPort() match
        case text: Text => safely(text.as[Int]).or(Presenter.port)
        case _          => Presenter.port

      execute:
        given Stdio = summon[Invocation].stdio
        given Environment = caps.unsafe.unsafeAssumePure(summon[Invocation].environment)
        val stdio: Stdio = summon[Stdio]

        val align: Optional[Text] = ui.Align.value.let(absolute(directory, _))

        // `--align` plays the presentation to the recording's times, so it implies `--autoplay`.
        val autoplay: Boolean = autoplay0 || align.present

        operand match
          case operand: Text =>
            val prepared =
              Presenter.prepare(config, absolute(directory, operand), width, autoplay, align)

            type Outcome =
              Exit | Tool.UsageError.type | NoDocument.type | Misaligned.type | Unaligned.type |
                Incoherent.type

            prepared match
              case Presenter.Problem.Unreadable => NoDocument
              case Presenter.Problem.Incoherent => Incoherent
              case Presenter.Problem.Misaligned => Misaligned
              case Presenter.Problem.Unaligned  => Unaligned

              case (title: Text, deck: Block) =>
                val frontend: pyrocosm.WebFrontend = Presenter.frontend(port)
                async(frontend.run(Presenter.interface(title, deck))((_: pyrocosm.Event) => ()))
                val url: Text = t"http://localhost:${port.show}/"
                Out.println(t"fuel: presenting $title at $url (Ctrl+C to stop)")

                Attend.interrupt()
                frontend.stop()
                Out.println(t"fuel: the presentation has stopped")
                Exit.Ok

          case _ =>
            val label: Text = usage(1).label
            val form: Text = t"fuel present <file> [--autoplay] [--align <recording or alignment>]"
            Out.println(t"fuel: $label usage: $form")
            Tool.UsageError

    // `fuel transcript <file>`: a presentation's transcript, set to the terminal's width, and
    // checked against the presentation's states.
    case ui.Transcript() :: rest =>
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).head
      ambient(transcript(directory, operand))

    // `fuel compile`: read the settings here, where completion mode registers them, and
    // compile within the execution.
    // `fuel show <file>`: one document, compiled as `compile` would, set in the terminal.
    case ui.Show() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).head
      ambient(show(config, directory, operand))

    // `fuel search <query>`: the daemon's API, searched; `-Type` is a term of the query, so only
    // a `--flag` is not.
    case ui.Search() :: rest =>
      ambient(searchApi(rest.map(_()).filter(!_.starts(t"--")).join(t" ")))

    case ui.Compile() :: _ =>
      val config: Workspace.Config = Workspace.load(directory)
      val force: Boolean = ui.Force().present
      ambient(compile(config, force, tty))

    // `fuel sync github <owner/repo>`: attach the repository's issues, fetch them now, and keep
    // them fresh while serving; `fuel sync` alone fetches every attached target's again.
    case ui.Sync() :: ui.Github() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).head
      val target: Optional[Sync.Target] = operand.let { name => Sync.Target.parse(List(t"github", name)) }
      ambient(sync(config, directory, target, named = true))

    case ui.Sync() :: _ =>
      val config: Workspace.Config = Workspace.load(directory)
      ambient(sync(config, directory, Unset, named = false))

    case _ =>
      ambient
        ( Fuel.usage
            ( List
                ( ui.Compile, ui.Show, ui.Search, ui.Present, ui.Transcript, ui.Sync ) ) )

// Compiles every configured directory into the daemon's corpus, which the dashboard shows, and
// reports a line per section and a line per file that could not be read.
private def width(using Environment): Int = Fuel.columns

// Attaches a tracker, or fetches every attached one again: each target's state is loaded from
// the project's cache, polled, merged and kept, and the issues are indexed against the API the
// daemon serves and published to its pages; then the daemon's refresher keeps them fresh. A
// poll which fails leaves the target's last state, and the command fails.
private def sync
  ( config: Workspace.Config, directory: Text, target: Optional[Sync.Target], named: Boolean )
  ( using Stdio, Environment, WorkingDirectory, Monitor )
:   Exit | Tool.UsageError.type | SyncFailed.type =

  val cache: Text = Sync.directory(config.root)
  val targets: List[Sync.Target] = target.let(List(_)).or(Sync.targets(config.sync, cache))

  if named && target.absent then
    Out.println(t"fuel: ${usage(4).label} usage: fuel sync github <owner/repo>")
    Tool.UsageError
  else if targets.nil then
    val message: Text = t"nothing is attached: fuel sync github <owner/repo> attaches a repository"
    Out.println(t"fuel: ${usage(5).label} $message")
    Tool.UsageError
  else
    val token: Optional[Text] = Fuel.credential(t"github", directory)
    Server.notices().each { (notice: Text) => Out.println(t"fuel: $notice") }

    @scala.caps.unsafe.untrackedCaptures
    var failed: Boolean = false

    val states: List[Sync.State] = targets.map: (target: Sync.Target) =>
      val fetched: Sync.Fetched = Sync.fetch(target, cache, token)

      fetched.failure.let: (message: Text) =>
        val label: Text = tracker(1).label
        Out.println(t"fuel: $label the issues of ${target.text} could not be fetched: $message")
        failed = true

      if fetched.failure.absent then
        fetched.changed.lay(Out.println(t"fuel: ${target.text}: nothing has changed since the last sync")):
          (count: Int) => Out.println(t"fuel: ${target.text}: ${count.show} issues fetched")

      fetched.state

    val unresolved: List[(Int, Text)] = Sync.publish(states)

    states.each: (state: Sync.State) =>
      val affecting: List[Issue] = state.issues.filter(!_.affects.nil)
      val count: Text = t"${state.issues.size.show} open issues, ${affecting.size.show} affect the API"
      Out.println(t"fuel: ${state.target}: $count")

    if Server.api.entities.nil then
      Out.println(t"fuel: no API has been compiled; the issues are attached by the next fuel compile")

    unresolved.each: (number: Int, name: Text) =>
      Out.println(t"fuel: #${number.show} names `$name`, which nothing resolves")

    Sync.watch(cache, token)
    if failed then SyncFailed else Exit.Ok

private def compile(config: Workspace.Config, force: Boolean, tty: Boolean)
  ( using Stdio, Console, Environment, WorkingDirectory, Monitor )
:   Exit | NoDocuments.type | CompileFailed.type =

  if config.documents.nil then
    // What is wrong with the sections the configuration has, if that is why there is none.
    config.badSections.each: (reason: Text, code: fulminate.Bolt) =>
      Out.println(t"fuel: ${code.label} $reason")

    Out.println(t"fuel: ${Workspace.bolt(11).label} no section of documents is configured")

    Out.println
      ( t"fuel: add `section <name> <dir>` to .pyrocosm/fuel/config.tel, and a `store <dir>` " +
          t"where an Orthant store holds the documents" )

    NoDocuments
  else
    // On a terminal the compile runs behind a board: a gauge of the work, and each problem as
    // it is found; the report follows once the board has closed. Elsewhere the problems are
    // printed as they are found, and the gauge is nothing.
    val board: Optional[Board] = if tty then Board(t"fuel: compile") else Unset
    val progress: Progress = Progress(board, width)

    def work(): Exit | CompileFailed.type = compiling(config, force, progress)

    val result: Exit | CompileFailed.type =
      board.lay(work()): (board: Board) =>
        unsafely:
          supervise:
            recover:
              case Terminal.Error() => work()

            . protect(board.showBoard()(work()))

    if board.let(_.aborted).or(false) then Out.println(t"fuel: the compile was abandoned")

    // Every diagnostic, rendered as the terminal renders them: at its place, with its excerpt —
    // unless each was printed as it was found.
    if !progress.streamed then
      val renderer = TerminalRenderer()
      renderer.blocks(progress.problems, width).each: (line: Teletype) => Out.println(line)

    attached(config)
    result

// The attached trackers' issues, indexed against the API just compiled and published to its
// pages: from the state kept for each, or fetched now for a target attached by the
// configuration which has none yet; then kept fresh by the daemon's refresher.
private def attached(config: Workspace.Config)
  ( using Stdio, Environment, WorkingDirectory, Monitor )
:   Unit =

  config.badSync.each: (line: Text) =>
    val label: Text = tracker(2).label
    Out.println(t"fuel: $label `sync $line` names no tracker: a target is `github <owner/repo>`")

  val cache: Text = Sync.directory(config.root)
  val targets: List[Sync.Target] = Sync.targets(config.sync, cache)

  if !targets.nil then
    val token: Optional[Text] = Fuel.credential(t"github", summon[WorkingDirectory].directory())

    val states: List[Sync.State] = targets.map: (target: Sync.Target) =>
      Sync.load(Sync.file(cache, target)).or:
        val fetched: Sync.Fetched = Sync.fetch(target, cache, token)

        fetched.failure.let: (message: Text) =>
          val label: Text = tracker(1).label
          Out.println(t"fuel: $label the issues of ${target.text} could not be fetched: $message")

        fetched.state

    Sync.publish(states).each: (number: Int, name: Text) =>
      Out.println(t"fuel: issue #${number.show} names `$name`, which nothing resolves")

    Sync.watch(cache, token)

// The compile itself, its problems and its progress reported as it goes.
private def compiling(config: Workspace.Config, force: Boolean, progress: Progress)
  ( using Stdio, Environment )
:   Exit | CompileFailed.type =

  import Progress.Phase

  locally:
    // Fragments are compiled only against a configured classpath: without one there is
    // nothing to compile them against, and they are highlighted alone.
    // A classpath entry that does not exist is reported once, and nothing is compiled against
    // the rest: the compiler would fail on every fragment for want of the standard library.
    val missing: List[Text] = config.classpath.filter(!onDisk(_))

    missing.each: (entry: Text) =>
      val text: Text = t"This classpath entry does not exist, so no fragment was compiled."
      val location: Block.Location = Block.Location(entry)
      progress.report(absent(location, text, 1))

    // What is wrong with the configuration's sections, or with its store.
    config.badSections.each: (reason: Text, code: fulminate.Bolt) =>
      val text: List[Block] = List(Block.paragraph(t"$reason."))
      val location: Block.Location = Block.Location(t"${config.root}/.pyrocosm/fuel/config.tel")
      progress.report(Block.Diagnostic(Tone.Failure, location, text, code = code))

    // The work there is, counted before it starts: the documents of every root, the dependency
    // jars, the sources; the pages are guessed from the sources until the API says.
    val documents: Int = config.documents.fold(0): (count, root) =>
      count + Compilation.files(root, config.library).size

    val jars: Int = config.classpath.filter(_.ends(t".jar")).size
    progress.plan(Phase.Documents, documents)
    progress.plan(Phase.Links, 1)

    if !config.classpath.nil && missing.nil then
      progress.plan(Phase.Jars, jars)

      if !config.sources.nil then
        progress.plan(Phase.Build, 1)
        progress.plan(Phase.Inspect, 1)
        progress.plan(Phase.Pages, config.sources.size*8)

      if !config.overlays.nil then progress.plan(Phase.Overlays, 1)

    // A session that fails to open or to compile is a diagnostic of its own, and the documents
    // are then compiled without it (tokenized, unchecked): the documents are still produced.
    val checked: Boolean = !config.classpath.nil && missing.nil

    // With an output directory, what the last compile kept is used wherever its inputs are
    // unchanged, unless `--force` says to compile everything again.
    val store: Optional[Store] = config.output.let(Store(_, force))

    // The hash of the classpath: of its bytes, until the hash of its API is available.
    val classpath: Text = Digests.of(config.classpath.map(Server.hash)*)

    // What an Orthant store cannot give, of the documents as each language reads them.
    config.depot.let: (depot: Depot) =>
      progress.reports(depot.failures())

      depot.tongues.each: (tongue: Text) =>
        progress.reports(depot.failures(tongue).map(depot.attributed(_, tongue)))

    // The glossary first, since every document's prose may name its terms.
    val (glossary, glossaryProblems, glossed) = glossaries(config)
    progress.reports(glossaryProblems)

    // What each section's file says, and what each language reads of it.
    val (titles, sectionProblems, titled) = sectionFiles(config)
    progress.reports(sectionProblems)

    // A corpus with each section called what its file calls it, in a language.
    def entitled(corpus: Corpus, titles: List[(Text, Text)]): Corpus =
      Corpus:
        corpus.sections.map: (section: Corpus.Section) =>
          section.copy(title = titles.filter(_(0) == section.root).head.let(_(1)))

    def retitled(corpus: Corpus, tongue: Text): Corpus =
      entitled(corpus, titled.filter(_(0) == tongue).head.let(_(1)).or(titles))

    // The glossary a language reads: its own, or the one the terms are defined in.
    def terms(tongue: Text): Glossary =
      glossed.filter(_(0) == tongue).head.let(_(1)).or(glossary)

    // The key of everything a document or a page depends on beyond its own text: Fuel's
    // version, the classpath, the project's sources, the compiler's options, whether documents
    // are sessions and fragments checked, the imports references resolve through, and which
    // entities the API has, which decides what links.
    def environment(sources: Text, apis: Apis): Text =
      val addresses: List[Text] =
        apis.all.bind(_.entities.values.map(_.address)).order(identity(_))

      val entities: Text = Digests.of(addresses.join(t"\n"))
      val mode: Text = if config.session then t"session" else t"single"
      val checking: Text = if checked then t"checked" else t"tokenized"

      Digests.of
        ( t"fuel ${Fuel.version}", classpath, sources, config.options.join(t"\n"), mode, checking,
          config.imports.join(t"\n"), entities, glossary.definitions )

    // The languages the documents are translated into; and what a compile keeps of what it
    // compiles: the output's, or for one which writes none and has translations, its own, so
    // that a fence every language shares is compiled once.
    val tongues: List[Text] = config.tongues
    val transient: Optional[Cache] = if tongues.nil then Unset else Cache.Transient()
    val kept: Optional[Cache] = store.lay(transient) { (store: Store) => store }

    def regime(environment: Text, linker: (Text, Text) -> Linker): Compilation.Regime =
      // Each document is reported as it is done, with its problems, since a corpus can take a
      // long while.
      Compilation.Regime
        ( config.session,
          config.fixtures,
          (path: Text, doc: Doc) =>
            progress.reports(doc.problems)
            progress.step(Phase.Documents),
          linker,
          kept,
          (path: Text) => Digests.of(environment, path),
          glossary,
          sections = config.sections,
          library = config.library )

    // The documents as a language reads them, each compiled from the language's own text of
    // it, with what is wrong with that text and not with the text it was written in.
    def readIn(written: Corpus, tongue: Text, checker: Checker, regime: Compilation.Regime)
    :   Corpus =

      val read: Corpus =
        Compilation.translated
          ( written, checker,
            regime.copy(glossary = terms(tongue), library = config.library(tongue)) )

      val theirs: List[Doc] = read.sections.bind(_.docs)
      val ours: List[Doc] = written.sections.bind(_.docs)

      theirs.filter { (doc: Doc) => !ours.exists(_ eq doc) }.each: (doc: Doc) =>
        progress.reports(doc.problems.map(config.attributed(_, tongue)))

      read

    // The API first, since the documents link to it: the sources compiled and saved, their
    // output and the dependencies walked, the overlays merged, and the pages of the project's
    // own entities rendered with their examples compiled.
    type Readings = Map[Text, Server.Reading]

    type Outcome = (Corpus, Apis, Map[Text, List[Block]], Text, Readings)

    def checking(): Outcome =
      Checking.session(config.classpath, config.options): (checker: Checker) =>
        val (apis, sources, spoken) = model(config, checker, progress)
        val api: Api = apis.primary
        val key: Text = environment(sources, apis)
        val standard: List[Locator] = Scope.standardOf(config.options)

        val pages: Map[Text, List[Block]] =
          rendered(apis, checker, store, key, progress, standard)

        val scope: Scope = Scope.document(config.imports, standard)

        val linker: (Text, Text) -> Linker =
          (section: Text, path: Text) => Resolver(api, scope, Doc.url(section, path), apis)

        val written: Corpus =
          Compilation.corpus(config.documents, checker, regime(key, linker))

        // The corpus and the API as each language reads them.
        val readings: Readings =
          spoken.map: (tongue: Text, apis: Apis) =>
            val read: Corpus = readIn(written, tongue, checker, regime(key, linker))
            tongue -> Server.Reading(retitled(read, tongue), apis)

          . to[Map]

        (entitled(written, titles), apis, pages, key, readings)

    val outcome: Optional[Outcome] =
      if !checked then Unset else sheltered(checking()): (error: Throwable) =>
        progress.report(failed(t"The fragments could not be compiled", error, 1))
        Unset

    val unchecked: Text = environment(t"", Apis.empty)

    val unlinked: (Text, Text) -> Linker = (_: Text, _: Text) => Linker.none

    val corpus: Corpus =
      outcome.let(_(0)).or:
        def highlighted: Corpus =
          Compilation.corpus(config.documents, Checker.tokenized, regime(unchecked, unlinked))

        sheltered(entitled(highlighted, titles)): (error: Throwable) =>
          progress.report(failed(t"The documents could not be highlighted", error, 2))
          Corpus(Nil)

    val apis: Apis = outcome.let(_(1)).or(Apis.of(Api.empty))

    // Without an API, a language still reads the documents, highlighted alone.
    val readings: Readings = outcome.let(_(4)).or:
      tongues.map: (tongue: Text) =>
        def translated: Corpus =
          readIn(corpus, tongue, Checker.tokenized, regime(unchecked, unlinked))

        val read: Corpus = sheltered(translated)(_ => corpus)

        tongue -> Server.Reading(retitled(read, tongue), apis)

      . to[Map]

    // The languages there are to read in: the one the documents are written in, then each
    // they are translated into.
    val languages: List[Text] =
      if tongues.nil then Nil else config.depot.lay(Nil)(_.languages.map(_(0)))

    val api: Api = apis.primary

    // Every link, now that every target is known.
    val links: List[Block] = Links.check(corpus, apis, glossary, config.checkLinks)
    progress.reports(links)
    progress.finish(Phase.Links)

    // The glossary page, in the order and the words of the language it is read in.
    def glossaryPage(glossary: Glossary, tongue: Optional[Text], apis: Apis): List[Block] =
      if glossary.terms.nil then Nil
      else
        val scope: Scope = Scope.document(config.imports, Scope.standardOf(config.options))

        glossary.page
          ( Resolver(apis.primary, scope, t"/glossary", apis), Phrases.locale(tongue) )

    val glossaried: Readings =
      readings.to[List].map: (tongue: Text, reading: Server.Reading) =>
        val read: Glossary = terms(tongue)
        tongue -> reading.copy(glossary = glossaryPage(read, tongue, reading.apis), terms = read)

      . to[Map]

    Server.publish
      ( corpus, api, outcome.let(_(2)).or(Map()), config.imports,
        glossaryPage(glossary, languages.head, apis), Scope.standardOf(config.options), glossary,
        apis.others, languages, glossaried )

    fuel.Help.publish(config.root)

    corpus.sections.each: (section: Corpus.Section) =>
      Out.println(t"${section.name}: ${section.docs.size.show} documents from ${section.root}")

    // The output: every document, by its section's directory relative to the project root,
    // and the manifest.
    store.let: (store: Store) =>
      corpus.sections.each: (section: Corpus.Section) =>
        val directory: Text =
          if section.root.starts(t"${config.root}/") then section.root.skip(config.root.length + 1)
          else section.root

        section.docs.each(store.document(directory, _))

      store.finish(Fuel.version, classpath, outcome.let(_(3)).or(unchecked))
      val kept: Int = store.hits()
      val compiled: Int = store.misses()
      val output: Text = config.output.or(t"")
      Out.println(t"fuel: wrote $output: ${kept.show} kept, ${compiled.show} compiled")

    if !api.entities.nil then
      val own: Int = api.entities.values.count(_.own)
      val documented: Int = api.entities.values.count: e => e.own && e.doc.present
      val counts: Text = t"${own.show} of the project's own, ${documented.show} documented"
      Out.println(t"api: ${api.entities.size.show} entities, $counts")
      val unrendered: Int = Inspection.unrendered.swap(0)
      val rendered: Int = Inspection.rendered.swap(0)
      val shown: Text = t"${unrendered.show} of ${rendered.show} types"
      if unrendered > 0 then Out.println(t"api: $shown could not be rendered, and are shown as `?`")

    // How much of each language's translation is done, as Orthant counts it: said, and no
    // problem, since a language reads what nobody has translated as it was written.
    config.depot.let: (depot: Depot) =>
      Out.println(t"fuel: the documents of the Orthant store in ${depot.home}, at ${depot.place}")

      // What is written in the live tree and is not in the store, which is not what was read.
      val unstored: List[Text] = depot.unstored

      if !unstored.nil then
        val count: Text =
          if unstored.size == 1 then t"1 document is" else t"${unstored.size.show} documents are"

        val shown: Text = unstored.keep(3).map { (document: Text) => t"`$document`" }.join(t", ")
        val more: Text = if unstored.size > 3 then t", …" else t""

        Out.println
          ( t"fuel: $count not in the store as ${depot.root} has them ($shown$more), and " +
              t"were read as the store has them: `orthant tag` stores them" )

      depot.tongues.each: (tongue: Text) =>
        depot.work(tongue).let: (work: orthant.Survey.Sum) =>
          val parts: List[Text] =
            List
              ( (work.pending, t"lines to translate"),
                (work.stale, t"passages written for text which has changed"),
                (work.faults, t"changes which conflict") )
            . filter(_(0) > 0)
            . map { (count: Int, what: Text) => t"${count.show} $what" }

          val left: Text = if parts.nil then t"" else t": ${parts.join(t", ")} (`orthant todo`)"
          Out.println(t"$tongue: ${work.percent.show}% translated$left")

    if config.classpath.nil
    then Out.println(t"fuel: no classpath is configured, so no fragment was compiled")

    val result: Exit | CompileFailed.type = if progress.problems.nil then Exit.Ok else CompileFailed
    result

// What each section's file says, as the title its directory's section has; the problems — a
// file which cannot be read or is not what the schema allows, in the language it was written
// in or in another; and the titles each other language reads: a section's file as that
// language's text of it says, and as it was written where that says no title.
private def sectionFiles(config: Workspace.Config)
:   (List[(Text, Text)], List[Block], List[(Text, List[(Text, Text)])]) =

  @scala.caps.unsafe.untrackedCaptures
  var problems: List[Block] = Nil

  val library: Library = config.library

  val read: List[(Text, Text, Text, SectionFile, List[Block])] =
    config.sectionFiles.bind: (root: Text, file: Text) =>
      def unreadable: List[(Text, Text, Text, SectionFile, List[Block])] =
        problems = Compilation.unreadable(file) :: problems
        Nil

      library.text(file).lay(unreadable): (text: Text) =>
        SectionFile.read(file, text) match
          case diagnostic: Block.Diagnostic =>
            problems = diagnostic :: problems
            Nil

          case (said, faults) =>
            faults.each { (fault: Block) => problems = fault :: problems }
            List((root, file, text, said, faults))

  def titles(all: List[(Text, SectionFile)]): List[(Text, Text)] =
    all.bind { (root, said) => said.title.let { title => List((root, title)) }.or(Nil) }

  val spoken: List[(Text, List[(Text, Text)])] =
    config.tongues.map: (tongue: Text) =>
      val theirs: Library = config.library(tongue)

      val each: List[(Text, SectionFile)] = read.map:
        (root: Text, file: Text, text: Text, defined: SectionFile, faults: List[Block]) =>
          val said: SectionFile = theirs.text(file).lay(defined): (foreign: Text) =>
            if foreign == text then defined else SectionFile.read(file, foreign) match
              case diagnostic: Block.Diagnostic =>
                problems = config.attributed(diagnostic, tongue) :: problems
                defined

              case (said, found) =>
                Compilation.novel(found, faults).each: (fault: Block) =>
                  problems = config.attributed(fault, tongue) :: problems

                SectionFile(said.title.or(defined.title))

          (root, said)

      (tongue, titles(each))

  val defined: List[(Text, SectionFile)] = read.map { (root, _, _, said, _) => (root, said) }
  (titles(defined), problems.reverse, spoken)

// The glossaries the configuration names, read and combined, with the problems: a file which
// cannot be read or is not TEL, and what its schema or `Glossary.merge` finds, in the
// language it was written in or in another. With them, the glossary each other language
// reads: every glossary as that language's text of it says, combined as the glossaries were,
// each translated term saying the name it translates, in the language its glossary was
// written in.
private def glossaries(config: Workspace.Config): (Glossary, List[Block], List[(Text, Glossary)]) =
  @scala.caps.unsafe.untrackedCaptures
  var problems: List[Block] = Nil

  val library: Library = config.library

  val read: List[(Text, Text, Glossary)] = config.glossaries.bind: (file: Text) =>
    def unreadable: List[(Text, Text, Glossary)] =
      problems = Compilation.unreadable(file) :: problems
      Nil

    library.text(file).lay(unreadable): (text: Text) =>
      Glossary.read(file, text) match
        case glossary: Glossary =>
          problems = Glossary.faults(file, text).reverse + problems
          List((file, text, glossary))

        case diagnostic: Block.Diagnostic =>
          problems = diagnostic :: problems
          Nil

  val (glossary, merging) = Glossary.merge(read.map(_(2)))
  val names: List[Text] = glossary.terms.map(_.name)

  // The language a glossary is written in: that of the section whose directory it is in.
  def language(file: Text): Optional[Text] =
    config.languages.filter { (directory, _) => file.starts(t"$directory/") }.head.let(_(1))

  val spoken: List[(Text, Glossary)] =
    config.tongues.map: (tongue: Text) =>
      given Locale[Phrases.Spoken] = Phrases.locale(tongue)
      val theirs: Library = config.library(tongue)

      val each: List[Glossary] = read.map: (file: Text, text: Text, defined: Glossary) =>
        val original: Text => Text = (name: Text) => Phrases.original(language(file), name)()

        theirs.text(file).lay(defined): (foreign: Text) =>
          if foreign == text then defined else
            val found: List[Block] = GlossaryTranslations.check(file, foreign, text, names)

            Compilation.novel(found, Glossary.faults(file, text)).each: (fault: Block) =>
              problems = config.attributed(fault, tongue) :: problems

            GlossaryTranslations.foreign(file, foreign, defined, original)

      (tongue, Glossary.merge(each)(0))

  (glossary, problems.reverse + merging, spoken)

// The API model of a project, in the session: its sources compiled and saved beneath the
// cache (`$XDG_CACHE_HOME/fuel/<project>/classes`, or `~/.cache/fuel/…`), the output and each
// dependency jar walked (a jar once per version, kept by the daemon), the overlays read and
// merged, and the pages of the project's own documented entities rendered — their examples
// compiled as a document's are — with every problem found on the way.
private def model(config: Workspace.Config, checker: Checker, progress: Progress)
  ( using Stdio, Environment )
:   (Apis, Text, List[(Text, Apis)]) =

  import Progress.Phase

  def report(block: Block): Unit = progress.report(block)

  val cache: Text =
    def classes(cache: Path on Linux): Text = t"${cache.encode}/classes"
    Fuel.projectCache(config.root).let(classes(_)).or(t"${config.root}/.fuel/classes")

  // The sources, read; one that cannot be read is a problem and left out.
  val sources: List[(Text, Text)] = config.sources.bind: (directory: Text) =>
    val root: Path on Linux = unsafely(directory.as[Path on Linux])

    if !Filing.directory(root) then
      val text: Text = t"This source directory does not exist."
      report(absent(Block.Location(directory), text, 2))
      Nil
    else
      filesBeneath(root, t".scala").bind: (file: Text) =>
        def unreadable: List[(Text, Text)] =
          report(Compilation.unreadable(file))
          Nil

        readText(file).lay(unreadable): (text: Text) => List((file, text))

  // The packages the sources declare, whose copies in a jar are theirs to shadow.
  val shadowed: List[Text] =
    val names: List[Text] = sources.bind: (_, text) =>
      text.cut(t"\n").map(_.trim).filter(_.starts(t"package ")).map: (line: Text) =>
        line.skip(8).trim.keep { char => char.isLetterOrDigit || char == '.' || char == '_' }

      . filter(_ != t"")

    names.distinct

  // The dependencies, each walked through a session of its own (once per version of the jar,
  // which the daemon keeps): a walk through the compile's session leaves it, whichever comes
  // first, reading next to nothing or writing nothing.
  val jars: List[Text] = config.classpath.filter(_.ends(t".jar"))

  val dependencies: Api = jars.fold(Api.empty): (acc, jar) =>
    val walked: Api = Server.jar(jar, shadowed):
      Checking.session(config.classpath, config.options): (walker: Checker) =>
        walker.context.lay(Api.empty): (context: dotty.tools.dotc.core.Contexts.Context) =>
          Inspection.classpath(List(jar), context, shadowed) match
            case api: Api => api

            case error: Throwable =>
              report(failed(t"The dependency $jar could not be inspected", error, 3))
              Api.empty

    progress.step(Phase.Jars)
    acc.merge(walked)

  // The JDK's public classes, which every Scala source sees, and which a document may name.
  val jdk: Api = Server.jdk:
    Checking.session(config.classpath, config.options): (walker: Checker) =>
      walker.context.lay(Api.empty): (context: dotty.tools.dotc.core.Contexts.Context) =>
        Inspection.jdk(context) match
          case api: Api => api

          case error: Throwable =>
            report(failed(t"The JDK could not be inspected", error, 4))
            Api.empty

  val built: Boolean =
    if sources.nil then false else
      // A fresh output: what the sources no longer declare must not linger.
      Filing.wipe(unsafely(cache.as[Path on Linux]))

      val notices: List[Compiler.Notice] = checker.build(sources.to[Map], cache)

      // The notice names its file as the compiler does, by its name alone or its path; the
      // file is named relative to the project root, as a reader knows it.
      def source(file: Text): Optional[Text] =
        sources.seek(_(0) == file).or(sources.seek(_(0).ends(t"/$file"))).let(_(1))

      Notices.diagnostics(notices, source(_), config.root).each(report(_))

      val errors: Int = notices.filter(_.importance == Importance.Error).size
      Out.println(t"fuel: compiled ${sources.size.show} sources (${errors.show} errors)")
      progress.finish(Phase.Build)
      errors == 0

  // The project's own output, from the inspector over what the build saved.
  val own: Api =
    if !built then Api.empty else Inspection.walk(List(cache), Nil, config.classpath) match
      case api: Api => api

      case error: Throwable =>
        report(failed(t"The project's own code could not be inspected", error, 5))
        Api.empty

  progress.finish(Phase.Inspect)
  val inspected: Api = jdk.merge(dependencies).merge(own)

  Inspection.failures() match
    case (count, error: Throwable) if count > 0 =>
      val trace: Text = frames(error).keep(6).join(t"; ")

      val text: Text =
        t"${count.show} members could not be read into the API model; the first failed with: " +
          t"${described(error)} ($trace)"

      // Each member is left out for a `ReadError`, whose code this carries.
      val code: fulminate.Bolt = compiler(6).carrying(FuelBolts(FuelBolts.api, FuelBolts.unread))
      report(Block.Diagnostic(Tone.Warning, Unset, List(Block.paragraph(text)), code = code))

    case _ =>
      ()

  // The overlays, read and merged.
  val library: Library = config.library

  val overlays: List[Overlay] = config.overlays.bind: (directory: Text) =>
    if !library.directory(directory) then
      val text: Text = t"This overlay directory does not exist."
      report(absent(Block.Location(directory), text, 3))
      Nil
    else
      // A glossary or a section's file in the directory is not an overlay.
      val files: List[Text] =
        library.files(directory, t".tel").filter(!config.glossaries.has(_))
        . filter { (file: Text) => !config.sectionFiles.exists(_(1) == file) }

      files.bind: (file: Text) =>
        def unreadable: List[Overlay] =
          report(Compilation.unreadable(file))
          Nil

        library.text(file).lay(unreadable): (text: Text) =>
          Overlay.read(file, text) match
            case overlay: Overlay =>
              List(overlay)

            case diagnostic: Block.Diagnostic =>
              report(diagnostic)
              Nil

  // Each overlay documents the API of its format: Scala's, unless it says.
  def overlaysOf(format: Format): List[Overlay] = overlays.filter(_.format == format.id)
  val formats: List[Format] = List(ScalaFormat, TelsFormat)

  overlays.filter { (overlay: Overlay) => !formats.exists(_.id == overlay.format) }.each:
    (overlay: Overlay) =>
      val known: Text = formats.map { (format: Format) => t"`${format.id}`" }.join(t", ", t" or ")
      val text: Text = t"`${overlay.format}` is not a format an overlay documents: $known."
      val location: Block.Location = Block.Location(overlay.file, 1)
      val code: fulminate.Bolt = Overlay.bolt(14)
      report(Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text)), code = code))

  // An API with its overlays merged into it, and what merging them found.
  def documented(api: Api): (Api, List[Block]) =
    val these: List[Overlay] = overlaysOf(api.format)

    if these.nil then (api, List[Block]()) else
      val (merged, merging) = Overlay.merge(api, these)
      merging.each(report)
      (merged, merging)

  // What applies to each type beyond its members, over the whole model.
  val (inspectedDocs, inspectedFaults) = documented(inspected)
  val api: Api = ScalaFormat.decorate(inspectedDocs)

  // The schemas: each read from its file, those which are sound together one API.
  val schemaTexts: List[(Text, Text)] = config.schemas.bind: (file: Text) =>
    def unreadable: List[(Text, Text)] =
      report(Compilation.unreadable(file))
      Nil

    readText(file).lay(unreadable): (text: Text) => List((file, text))

  val read: List[Api] = schemaTexts.bind: (file: Text, text: Text) =>
    val (schema, problems) = TelsFormat.read(file, text)
    problems.each(report)
    schema.let(List(_)).or(Nil)

  val (combined, clashes) = TelsFormat.combine(read)

  clashes.each: (name: Text) =>
    val text: Text = t"The schema `$name` is declared by more than one of the configured files."
    val paragraphs: List[Block] = List(Block.paragraph(text))
    report(Block.Diagnostic(Tone.Failure, Unset, paragraphs, code = TelsFormat.clash))

  val (combinedDocs, combinedFaults) = documented(combined)
  val schemas: Api = TelsFormat.decorate(combinedDocs)
  progress.finish(Phase.Overlays)

  if !overlays.nil then Out.println(t"fuel: ${overlays.size.show} overlays")
  if !read.nil then Out.println(t"fuel: ${read.size.show} schemas")

  // The sources and the schemas, as one hash: their paths and their texts.
  val hashed: Text =
    Digests.of((sources + schemaTexts).bind { (path: Text, text: Text) => List(path, text) }*)

  val others: List[Api] = if schemas.entities.nil then Nil else List(schemas)

  // The API as each other language reads it: every overlay read from that language's text of
  // it, and merged as the overlays were. An overlay is held, in each language, to what an
  // overlay must be; what is wrong with one as it was written was said above, and is not
  // said again of a language which has the same text.
  val spoken: List[(Text, Apis)] =
    config.tongues.map: (tongue: Text) =>
      val theirs: Library = config.library(tongue)

      val read: List[Overlay] = overlays.map: (overlay: Overlay) =>
        theirs.text(overlay.file).lay(overlay): (text: Text) =>
          if library.text(overlay.file) == text then overlay
          else Overlay.read(overlay.file, text) match
            case translated: Overlay =>
              translated

            case diagnostic: Block.Diagnostic =>
              report(config.attributed(diagnostic, tongue))
              overlay

      def documentedIn(api: Api, known: List[Block]): Api =
        val these: List[Overlay] = read.filter(_.format == api.format.id)

        if these.nil then api else
          val (merged, merging) = Overlay.merge(api, these)

          Compilation.novel(merging, known).each: (fault: Block) =>
            report(config.attributed(fault, tongue))

          merged

      val scala: Api = ScalaFormat.decorate(documentedIn(inspected, inspectedFaults))
      val tels: Api = TelsFormat.decorate(documentedIn(combined, combinedFaults))
      val schemas: List[Api] = if tels.entities.nil then Nil else List(tels)
      (tongue, Apis(scala :: schemas))

  (Apis(api :: others), hashed, spoken)

// The pages of the project's own entities, each rendered — its examples compiled as a
// document's are, its references resolved in its own scope — unless the output keeps it under
// the key of its inputs: everything the compile depends on (`environment`), the entity, and the
// members its page lists. Their problems are kept with them.
private def rendered
  ( apis:        Apis,
    checker:     Checker,
    store:       Optional[Store],
    environment: Text,
    progress:    Progress,
    standard:    List[Locator] )
:   Map[Text, List[Block]] =

  def render(entity: Api.Entity): (List[Block], List[Block]) =
    val api: Api = apis.of(entity)
    val imports: List[Text] = entity.doc.let(_.imports).or(Nil)
    val scope: Scope = Scope.of(entity, imports, api.format.standard(standard))
    val resolver: Resolver = Resolver(api, scope, Unset, apis)
    val place: Text = entity.doc.let(_.file).or(entity.key)
    val scala: Embedding.ScalaCode = Blocks.scala(checker, place, false, Unset, resolver)
    val tables: Tables = Tables(place, resolver)
    val embeddings = List(scala, Embedding.JavaCode, tables)
    val blocks: List[Block] = ApiPages.page(api, entity, resolver, scope, embeddings)

    // A table has reported, at its own place, the ambiguous references in its cells.
    val unplaced: List[Linker.Ambiguity] = resolver.ambiguities.filter:
      (ambiguity: Linker.Ambiguity) => !tables.ambiguous.has(ambiguity.reference)

    val ambiguous: List[Block] = unplaced.map: (ambiguity: Linker.Ambiguity) =>
      Doc.ambiguity(ambiguity, Block.Location(place, entity.doc.let(_.line)))

    (blocks, scala.problems + tables.problems + ambiguous)

  // Every format's own entities, Scala's first, each kept by its address.
  val own: List[Api.Entity] = apis.all.bind: (api: Api) =>
    api.entities.values.filter(_.own).order(_.key)

  progress.plan(Progress.Phase.Pages, own.size)

  val pages: List[(Text, List[Block])] = own.map: (entity: Api.Entity) =>
    val (blocks, raised) = store.lay(render(entity)): (store: Store) =>
      val members: Text = apis.of(entity).children(entity).map(_.toString.tt).join(t"\n")
      val key: Text = Digests.of(environment, t"page", entity.toString.tt, members)
      val path: Text = t"${Api.prefix(entity.format)}${entity.path}"
      val page: Output.Page = store.page(key, entity.address, path)(render(entity))
      (page.blocks, page.problems)

    progress.reports(raised)
    progress.step(Progress.Phase.Pages)
    (entity.address, blocks)

  pages.to[Map]

// One document set in the terminal: its blocks at the terminal's width, paragraphs justified
// and hyphenated, code highlighted, an animated block as its steps in sequence; its problems
// after it. Compiled as `compile` compiles, with the session and the classpath the
// configuration gives, so what the page shows is what the dashboard would.
// A presentation's transcript, printed as it is set, wrapped to the terminal's width, words
// whole, a cue's words as any others; what the presentation's reading reports follows it, and
// where the transcript does not cohere with the presentation, which is a failure.
private def transcript(directory: Text, operand: Optional[Text])(using Stdio, Environment)
:   Exit | Tool.UsageError.type | NoDocument.type | Incoherent.type =

  operand match
    case operand: Text =>
      val path: Text = absolute(directory, operand)
      val renderer = TerminalRenderer()

      def print(blocks: List[Block]): Unit =
        renderer.blocks(blocks, width).each: (line: Teletype) => Out.println(line)

      readText(path) match
        case content: Text => Presentation.read(path, content) match
          case diagnostic: Block.Diagnostic =>
            print(List(diagnostic))
            NoDocument

          case (presentation: Presentation, problems: List[Block] @unchecked) =>
            def reported(problems: List[Block]): Unit =
              if !problems.nil then
                Out.println(blank)
                print(problems)

            (Presenter.transcript(path, presentation): @unchecked) match
              case unread: Block =>
                print(problems + List(unread))
                NoDocument

              case (transcript: Transcript, faults: List[Block] @unchecked) =>
                print(transcript.blocks)
                reported(problems + faults)
                if faults.nil then Exit.Ok else Incoherent

              case _ =>
                Out.println(t"fuel: $path names no transcript")
                reported(problems)
                Exit.Ok

        case _ =>
          Out.println(t"fuel: ${unreadable.label} $path could not be read")
          NoDocument

    case _ =>
      Out.println(t"fuel: ${usage(6).label} usage: fuel transcript <file>")
      Tool.UsageError

private def show(config: Workspace.Config, directory: Text, operand: Optional[Text])
  ( using Stdio, Environment )
:   Exit | Tool.UsageError.type | NoDocument.type | CompileFailed.type =

  operand match
    case operand: Text =>
      val path: Text = absolute(directory, operand)

      compiled(config, path) match
        case doc: Doc =>
          import hyphenations.englishHyphenation
          val renderer = TerminalRenderer(justify = true, purview = Scope.purview(config.imports))
          renderer.blocks(doc.blocks, width).each: (line: Teletype) => Out.println(line)

          if !doc.problems.nil then
            Out.println(blank)
            renderer.blocks(doc.problems, width).each: (line: Teletype) => Out.println(line)

          if doc.problems.nil then Exit.Ok else CompileFailed

        case _ =>
          Out.println(t"fuel: ${unreadable.label} $path could not be read")
          NoDocument

    case _ =>
      Out.println(t"fuel: ${usage(7).label} usage: fuel show <file>")
      Tool.UsageError

// Searches the API the daemon last compiled, printing each declaration found over its key and
// its page.
private def searchApi(text: Text)(using Stdio): Exit | Tool.UsageError.type =
  if text.trim == t"" then
    Out.println(t"fuel: ${usage(8).label} usage: fuel search <query>")
    Tool.UsageError
  else
    val apis: Apis = Server.apis

    Query.read(text, apis.vocabulary) match
      case error: Query.Error =>
        Out.println(t"fuel: ${error.bolt.label} ${error.message.text} (at ${error.offset.show})")
        Tool.UsageError

      case query: Query =>
        if apis.isEmpty then
          Out.println(t"fuel: no API has been compiled; run fuel compile in a project")
          Exit.Ok
        else
          val outcome: Search.Outcome = apis.search(query, Server.terms)
          val scope: Scope = Scope.document(Server.imports, Server.standard)
          val renderer: TerminalRenderer = TerminalRenderer()
          outcome.notices.each: (notice: Text) => Out.println(t"fuel: $notice")

          outcome.terms.each: (term: Glossary.Term) =>
            Out.println(t"${term.name}  /glossary#${term.slug}")

          outcome.hits.each: (hit: Search.Hit) =>
            val api: Api = apis.of(hit.entity)
            Out.println(t"${hit.entity.address}  ${hit.entity.url}")

            // Scala's declaration on one line; another format's as its signature's first line:
            // either in the colours code has wherever else it is shown.
            val written: List[Token] =
              if api.format == ScalaFormat then
                Signatures.declaration(hit.entity.name, hit.declaration, api, scope)
              else api.format.signature(hit.entity.name, hit.declaration, api, scope) match
                case Block.Code(_, line :: _, _, _, _, _, _, _) => line.tokens
                case _                                         => List(Token.plain(hit.entity.name))

            Out.println(e"    ${renderer.tokens(written)}")

          Out.println(t"fuel: ${outcome.hits.size.show} results")
          Exit.Ok

private def blank: Teletype = e""

// A path as the command line gives it, from the directory `fuel` was run in.
private def absolute(directory: Text, operand: Text): Text =
  if operand.starts(t"/") then operand else t"$directory/$operand"

// One document compiled as `compile` compiles it, with the session and the classpath the
// configuration gives, or highlighted alone where there is no usable classpath or the
// session fails; `Unset` if it cannot be read.
private def compiled(config: Workspace.Config, path: Text): Optional[Doc] =
  val checked: Boolean = !config.classpath.nil && config.classpath.all(onDisk(_))
  val glossary: Glossary = glossaries(config)(0)

  val sections: List[(Text, Text)] = config.sections

  val regime: Compilation.Regime =
    Compilation.Regime
      ( config.session, config.fixtures, glossary = glossary, sections = sections )

  def compiled: Optional[Doc] =
    Checking.session(config.classpath, config.options): (checker: Checker) =>
      Compilation.document(path, checker, regime)

  if checked then sheltered(compiled)(_ => Compilation.document(path, Checker.tokenized, regime))
  else
    Compilation.document(path, Checker.tokenized, regime)

// Whether a path exists.
def onDisk(entry: Text): Boolean =
  safely(entry.as[Path on Linux]).let(Filing.exists(_)).or(false)

// The text of a file, if it can be read.
def readText(file: Text): Optional[Text] =
  safely(file.as[Path on Linux]).let(Filing.text(_))

// The files beneath a directory whose names end with a suffix, in order.
def filesBeneath(root: Path on Linux, suffix: Text): List[Text] =
  Filing.beneath(root).map(_.encode).filter(_.ends(suffix)).order(identity(_))

// The word before the one at `index`, if there is one.
private def before(words: List[Text], index: Ordinal): Optional[Text] =
  if index.n0 == 0 then Unset else words.at(Ordinal.zerary(index.n0 - 1))

// A computation of the compiler's, or, if it fails, what `fallback` makes of its failure. The
// compiler fails not only with exceptions but with errors — an `AssertionError` of its own, a
// `StackOverflowError` deep in a type, a `LinkageError` from the classpath it is given — and
// contingency's `safely` and `recover` see exceptions alone, so this is the one place which
// catches every `Throwable` (a gap in Soundness, which has no handler for errors).
def sheltered[result](block: => result)(fallback: Throwable => result): result =
  try block catch case error: Throwable => fallback(error)

// A failure, as the JVM describes it: its class and its message. A `Throwable` has no
// `Showable`, so its own description is the one there is.
def described(error: Throwable): Text = error.toString.tt

// Where a failure was thrown: each frame of its stack, innermost first, as its class, its
// method, and its file and line.
def frames(error: Throwable): List[Text] =
  StackTrace(error).frames.map: (frame: StackTrace.Frame) =>
    val line: Text = frame.line.let(_.show).or(t"?")
    t"${frame.method.className}.${frame.method.method}(${frame.file}:$line)"

// A failure of the compiler itself, which has no place in a document, as a diagnostic, with
// the code of the compiler's failures, `↯/f.27.8.<variant>`: the fragments which could not be
// compiled (1); the documents which could not be highlighted (2); a dependency (3), the JDK (4)
// or the project's own code (5) which could not be inspected; and, a caution, the members
// which could not be read into the API's model (6).
private def failed(what: Text, error: Throwable, variant: Int): Block =
  val detail: List[Block] = List(Block.paragraph(described(error)))
  val message: List[Block] = List(Block.paragraph(what))
  Block.Diagnostic(Tone.Failure, Unset, message, Nil, detail, compiler(variant))

private def compiler(variant: Int): fulminate.Bolt =
  FuelBolts(FuelBolts.client, FuelBolts.compiler, variant)

// A configured path which does not exist, as a diagnostic at it, with its code,
// `↯/f.27.7.<variant>`: a classpath entry (1), a source directory (2) or an overlay directory
// (3).
private def absent(location: Block.Location, text: Text, variant: Int): Block =
  val code: fulminate.Bolt = FuelBolts(FuelBolts.client, FuelBolts.missing, variant)
  Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text)), code = code)

// The code of a command line which was not understood, `↯/f.27.5.<variant>`: of `fuel present`
// (1); of `fuel sync`, without a repository (4) or with nothing attached (5); and of
// `fuel transcript` (6), `fuel show` (7) and `fuel search` (8) without what they take. The
// second and the third were of `fuel translate`, and are no longer raised.
private def usage(variant: Int): fulminate.Bolt =
  FuelBolts(FuelBolts.client, FuelBolts.usage, variant)

// The code of a file which could not be read, as the core's diagnostic of one has it.
private def unreadable: fulminate.Bolt = FuelBolts(FuelBolts.core, FuelBolts.unreadable)

// The code of a tracker's failure, `↯/f.27.6.<variant>`: its issues could not be fetched (1),
// or a `sync` line names none (2).
private def tracker(variant: Int): fulminate.Bolt =
  FuelBolts(FuelBolts.client, FuelBolts.tracker, variant)
