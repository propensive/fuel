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

import scala.caps
import scala.collection.immutable as sci
import scala.jdk.CollectionConverters.*

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Compilation as _, *}

import backstops.silentBackstop
import dysasymptotics.{linearAccess, linearSize}
import interpreters.posixInterpreter
import palettes.solarizedDarkGaugePalette
import probates.cancelProbate
import pyrocosm.{Block, Inline, TerminalRenderer, Tone, Tool}
import tableStyles.thickTableStyle
import textMetrics.uniformMetric
import threads.platformThreads

// The exit statuses fuel can terminate with, as objects (soundness#1811), so that the union of
// an `execute` block's result type documents them in the manpage's EXIT STATUS section.
object UsageError extends Status(2, t"the command line was not understood")
object NoDocuments extends Status(3, t"no document directory is configured")
object NoDocument extends Status(5, t"the document could not be read")
object CompileFailed extends Status(4, t"a code fragment did not do as its preamble said")

// Fuel's user interface, in one namespace: its subcommands, flags and settings.
object ui:
  val Compile = Subcommand("compile", "compile the configured directories of Markdown documents")
  val Serve   = Subcommand("serve", "serve the compiled documents on the web until Ctrl+C")
  val Show    = Subcommand("show", "render one document in the terminal")
  val Present = Subcommand("present", "show a presentation's slides in a browser until Ctrl+C")

  // Each is read from the cascade every Pyrocosm tool shares: the flag, the `fuel.*` property,
  // the `FUEL_*` variable, the repository's `.pyrocosm/fuel/config.tel` and the user's. A
  // repeated keyword's atoms arrive joined by `:`.
  val Classpath =
    Setting[Text](t"classpath", t"the jars and directories embedded code is compiled against")

  val Document =
    Setting[Text](t"document", t"a directory of Markdown documents, relative to the project root")

  val Port = Setting[Text](t"port", t"the port on which `fuel serve` serves the documents")

  val ScalacOption =
    Setting[Text](t"scalacOption", t"a compiler option for the fragments, as `-deprecation`")

  val Session =
    Setting[Boolean](t"session", t"compile each document's fragments as one session, each following the ones before")

  val Fixtures =
    Setting[Text](t"fixtures", t"a directory of `<document>.scala` fixtures, compiled before each document's fragments")

  val Source =
    Setting[Text](t"source", t"a directory of the project's own Scala sources, whose API is documented")

  val Overlay =
    Setting[Text](t"overlay", t"a directory of TEL overlays, the API's documentation")

  val Imports =
    Setting[Text](t"imports", t"a package or object whose members a document's references resolve through, as `soundness`")

  val Output =
    Setting[Text](t"output", t"a directory `fuel compile` writes its output into, as BinTEL, compiling only what changed")

  val Force = Flag[Unit]("force", false, List('f'), "compile everything again, ignoring what the output directory keeps")

  val PresentPort =
    Setting[Text](t"presentPort", t"the port `fuel present` serves on (8061 by default)")

  val Glossary =
    Setting[Text](t"glossary", t"a TEL file of `define`d terms, which a document names in brackets")

  val CheckLinks =
    Setting[Boolean](t"checkLinks", t"fetch every link to the web, and fail on one which does not answer")

// There is no `Boolean is Decodable in Text` given in distillate, so `Setting[Boolean]` reads
// decode here: `true`, `yes` and `on` — case-insensitively — are true, and anything else false.
given (Boolean is Decodable in Text) = (value: Text) =>
  value.lower == t"true" || value.lower == t"yes" || value.lower == t"on"

// Fuel as a Pyrocosm tool: `about`, `install`, `quit` and `--version` come from `Tool`, as does
// the search for `.pyrocosm/fuel/config.tel`, and the dashboard a config's `serve` launches.
// Capitalised, since a `val fuel` would clash with the launcher's `object fuel`.
val Fuel: Tool =
  Tool
    ( t"fuel",
      prose = t"Fuel compiles a project's Markdown documentation, with its embedded code " +
        t"highlighted, and serves it on the web; in time it will check that code against " +
        t"the project's classpath, animate it, and document the project's API.",
      web   = fuel.Dashboard.web )

// Every command body takes `Stdio`, `Environment` and `WorkingDirectory` as pure `using`
// parameters; `execute` provides them through an `Invocation` whose derived capabilities are
// tracked, so each arm seals them once with `unsafeAssumePure` — they outlive the command they
// drive — before calling the body.
private inline def ambient[result <: Termination: scala.Precise]
  ( inline body: (Stdio, Environment, WorkingDirectory) ?=> result )
  ( using Cli )
  ( using result is Status.Admissible )
:   Execution to result =

  execute:
    (erased effectful: Effectful) ?=> (invocation: Invocation) ?=>
      given Stdio = caps.unsafe.unsafeAssumePure(invocation.stdio)
      given Environment = caps.unsafe.unsafeAssumePure(invocation.environment)
      given WorkingDirectory = caps.unsafe.unsafeAssumePure(invocation.workingDirectory)
      body

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
    // `fuel serve [--port]`: the compiled documents, served until Ctrl+C.
    case ui.Serve() :: _ =>
      val port: Int = ui.Port() match
        case text: Text => safely(text.as[Int]).or(fuel.Dashboard.web.port)
        case _          => fuel.Dashboard.web.port

      execute:
        given Stdio = summon[Invocation].stdio
        val stdio: Stdio = summon[Stdio]
        val aborted: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)

        trap:
          case Signal(Interrupt.Int, _, _, _) =>
            aborted.set(true)
            SignalResponse.Accept

        async(fuel.Dashboard.web.serve(port))
        val url: Text = t"http://localhost:${port.toString.tt}/"
        Out.println(t"fuel: serving the documents at $url (Ctrl+C to stop)")

        // On a terminal the launcher forwards Ctrl+C as a byte rather than a signal, so the
        // input is read for it here.
        def loop(): Unit =
          if tty then
            while stdio.in.available() > 0 do if stdio.in.read() == 3 then aborted.set(true)

          if !aborted.get then
            snooze(0.25*Second)
            loop()

        loop()
        fuel.Dashboard.web.stop()
        Out.println(t"fuel: the server has stopped")
        Exit.Ok

    // `fuel present <file>`: a presentation's slides in a browser, until Ctrl+C.
    case ui.Present() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).prim

      val port: Int = ui.PresentPort() match
        case text: Text => safely(text.as[Int]).or(Presenter.port)
        case _          => Presenter.port

      execute:
        given Stdio = summon[Invocation].stdio
        given Environment = caps.unsafe.unsafeAssumePure(summon[Invocation].environment)
        val stdio: Stdio = summon[Stdio]

        operand match
          case Unset =>
            Out.println(t"fuel: usage: fuel present <file>")
            UsageError

          case operand: Text =>
            val prepared = Presenter.prepare(config, absolute(directory, operand), width)

            prepared.lay[Exit | UsageError.type | NoDocument.type](NoDocument): (title, deck) =>
              val aborted: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)

              trap:
                case Signal(Interrupt.Int, _, _, _) =>
                  aborted.set(true)
                  SignalResponse.Accept

              val frontend: pyrocosm.WebFrontend = Presenter.frontend(port)
              async(frontend.run(Presenter.interface(title, deck))((_: pyrocosm.Event) => ()))
              val url: Text = t"http://localhost:${port.toString.tt}/"
              Out.println(t"fuel: presenting $title at $url (Ctrl+C to stop)")

              // On a terminal the launcher forwards Ctrl+C as a byte rather than a signal.
              def loop(): Unit =
                if tty then
                  while stdio.in.available() > 0 do if stdio.in.read() == 3 then aborted.set(true)

                if !aborted.get then
                  snooze(0.25*Second)
                  loop()

              loop()
              frontend.stop()
              Out.println(t"fuel: the presentation has stopped")
              Exit.Ok

    // `fuel compile`: read the settings here, where completion mode registers them, and
    // compile within the execution.
    // `fuel show <file>`: one document, compiled as `compile` would, set in the terminal.
    case ui.Show() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).prim
      ambient(show(config, directory, operand))

    case ui.Compile() :: _ =>
      val config: Workspace.Config = Workspace.load(directory)
      val force: Boolean = ui.Force().present
      ambient(compile(config, force))

    case _ =>
      ambient:
        Out.println(t"Usage: fuel <subcommand>")
        Out.println(t"")
        Out.println(t"  compile    compile the configured directories of Markdown documents")
        Out.println(t"  show       render one document in the terminal")
        Out.println(t"  present    show a presentation's slides in a browser until Ctrl+C")
        Out.println(t"  serve      serve the compiled documents on the web until Ctrl+C")
        Out.println(t"  about      show this tool's name, version and daemon")
        Out.println(t"  install    install shell tab-completions and the manpage")
        Out.println(t"  quit       stop the background daemon")
        UsageError

// Compiles every configured directory into the daemon's corpus, which the dashboard shows, and
// reports a line per section and a line per file that could not be read.
// The terminal's width, for the report; 120 where nothing says.
private def width(using Environment): Int =
  safely(Environment.columns[Text].s.toInt).lay(120)(_.min(160))

private def compile(config: Workspace.Config, force: Boolean)(using Stdio, Environment)
:   Exit | NoDocuments.type | CompileFailed.type =

  if config.documents.nil then
    Out.println(t"fuel: no document directory is configured")
    Out.println(t"fuel: add `document <dir>` to .pyrocosm/fuel/config.tel")
    NoDocuments
  else
    // Fragments are compiled only against a configured classpath: without one there is
    // nothing to compile them against, and they are highlighted alone.
    // A classpath entry that does not exist is reported once, and nothing is compiled against
    // the rest: the compiler would fail on every fragment for want of the standard library.
    def exists(entry: Text): Boolean = java.nio.file.Files.exists(java.nio.file.Path.of(entry.s))
    val missing: List[Text] = config.classpath.filter(!exists(_))

    @scala.caps.unsafe.untrackedCaptures
    var failures: List[Block] =
      missing.map: (entry: Text) =>
        val text: Text = t"This classpath entry does not exist, so no fragment was compiled."
        Block.Diagnostic(Tone.Failure, Block.Location(entry), List(Block.paragraph(text)))

    // A session that fails to open or to compile is a diagnostic of its own, and the documents
    // are then compiled without it (tokenized, unchecked): the documents are still produced.
    val checked: Boolean = !config.classpath.nil && missing.nil

    // With an output directory, what the last compile kept is used wherever its inputs are
    // unchanged, unless `--force` says to compile everything again.
    val store: Optional[Store] = config.output.let(Store(_, force))

    // The hash of the classpath: of its bytes, until the hash of its API is available.
    val classpath: Text = Hashes.of(config.classpath.map(Server.hash)*)

    // The glossary first, since every document's prose may name its terms.
    val (glossary, glossaryProblems) = glossaries(config)

    // The key of everything a document or a page depends on beyond its own text: Fuel's
    // version, the classpath, the project's sources, the compiler's options, whether documents
    // are sessions and fragments checked, the imports references resolve through, and which
    // entities the API has, which decides what links.
    def environment(sources: Text, api: Api): Text =
      val entities: Text = Hashes.of(List.from(api.entities.keys.toList.sortBy(_.s)).join(t"\n"))

      Hashes.of
        ( t"fuel ${Fuel.version}", classpath, sources, config.options.join(t"\n"),
          if config.session then t"session" else t"single", if checked then t"checked" else t"tokenized",
          config.imports.join(t"\n"), entities, glossary.definitions )

    def regime(environment: Text, linker: (Text, Text) -> Linker): Compilation.Regime =
      // Each document is reported as it is done, since a corpus can take a long while.
      Compilation.Regime
        ( config.session,
          config.fixtures,
          (path: Text, problems: Int) =>
            Out.println(t"fuel: $path: ${problems.show} ${if problems == 1 then t"problem" else t"problems"}"),
          linker,
          store,
          (path: Text) => Hashes.of(environment, path),
          glossary )

    // The API first, since the documents link to it: the sources compiled and saved, their
    // output and the dependencies walked, the overlays merged, and the pages of the project's
    // own entities rendered with their examples compiled.
    @scala.caps.unsafe.untrackedCaptures
    var apiProblems: List[Block] = Nil

    val outcome: Optional[(Corpus, Api, sci.Map[Text, List[Block]], Text)] =
      if !checked then Unset else
        try Checking.session(config.classpath, config.options): (checker: Checker) =>
          val (api, problems, sources) = model(config, checker)
          val key: Text = environment(sources, api)
          val (pages, pageProblems) = rendered(api, checker, store, key)
          apiProblems = problems + pageProblems
          val linker: (Text, Text) -> Linker =
            (section: Text, path: Text) => Resolver(api, Scope.document(config.imports), Doc.url(section, path))

          (Compilation.corpus(config.documents, checker, regime(key, linker)), api, pages, key)
        catch case error: Throwable =>
          failures = failed(t"The fragments could not be compiled", error) :: failures
          Unset

    val unchecked: Text = environment(t"", Api.empty)

    val corpus: Corpus =
      outcome.let(_(0)).or:
        try Compilation.corpus(config.documents, Checker.tokenized, regime(unchecked, (_: Text, _: Text) => Linker.none))
        catch case error: Throwable =>
          failures = failed(t"The documents could not be highlighted", error) :: failures
          Corpus(Nil)

    val api: Api = outcome.let(_(1)).or(Api.empty)

    // Every link, now that every target is known.
    val links: List[Block] = Links.check(corpus, api, glossary, config.checkLinks)

    val glossaryPage: List[Block] =
      if glossary.terms.nil then Nil else glossary.page(Resolver(api, Scope.document(config.imports), t"/glossary"))

    Server.publish(corpus, api, outcome.let(_(2)).or(sci.Map()), config.imports, glossaryPage)

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
      val kept: Int = store.hits.get
      val compiled: Int = store.misses.get
      Out.println(t"fuel: wrote ${config.output.or(t"")}: ${kept.show} kept, ${compiled.show} compiled")

    if !api.entities.isEmpty then
      val own: Int = api.entities.values.count(_.own)
      val documented: Int = api.entities.values.count { e => e.own && e.doc.present }
      Out.println(t"api: ${api.entities.size.show} entities, ${own.show} of the project's own, ${documented.show} documented")
      val unrendered: Int = Inspection.unrendered.getAndSet(0)
      val rendered: Int = Inspection.rendered.getAndSet(0)
      if unrendered > 0 then Out.println(t"api: ${unrendered.show} of ${rendered.show} types could not be rendered, and are shown as `?`")

    // Every diagnostic, rendered as the terminal renders them: at its place, with its excerpt.
    val problems: List[Block] = failures.reverse + glossaryProblems + apiProblems + corpus.problems + links
    val renderer = TerminalRenderer()
    renderer.blocks(problems, width).each: (line: Teletype) => Out.println(line)

    if config.classpath.nil
    then Out.println(t"fuel: no classpath is configured, so no fragment was compiled")

    if problems.nil then Exit.Ok else CompileFailed

// The glossaries the configuration names, read and combined, with the problems: a file which
// cannot be read or is not TEL, and what `Glossary.merge` finds.
private def glossaries(config: Workspace.Config): (Glossary, List[Block]) =
  @scala.caps.unsafe.untrackedCaptures
  var problems: List[Block] = Nil

  val read: List[Glossary] = config.glossaries.bind: (file: Text) =>
    try Glossary.read(file, Text(java.nio.file.Files.readString(java.nio.file.Path.of(file.s)).nn)) match
      case glossary: Glossary           => List(glossary)
      case diagnostic: Block.Diagnostic =>
        problems = diagnostic :: problems
        Nil
    catch case error: Throwable =>
      problems = Compilation.unreadable(file) :: problems
      Nil

  val (glossary, merging) = Glossary.merge(read)
  (glossary, problems.reverse + merging)

// The API model of a project, in the session: its sources compiled and saved beneath the
// cache (`$XDG_CACHE_HOME/fuel/<project>/classes`, or `~/.cache/fuel/…`), the output and each
// dependency jar walked (a jar once per version, kept by the daemon), the overlays read and
// merged, and the pages of the project's own documented entities rendered — their examples
// compiled as a document's are — with every problem found on the way.
private def model(config: Workspace.Config, checker: Checker)(using Stdio)
:   (Api, List[Block], Text) =

  @scala.caps.unsafe.untrackedCaptures
  var problems: List[Block] = Nil

  def report(block: Block): Unit = problems = block :: problems

  val cache: Text =
    val xdg: String | Null = java.lang.System.getenv("XDG_CACHE_HOME")
    val base: Text = if xdg == null || xdg.isEmpty then t"${java.lang.System.getProperty("user.home").nn}/.cache" else xdg.nn.tt
    val project: Text = Text(java.lang.Integer.toHexString(config.root.s.hashCode).nn)
    t"$base/fuel/$project/classes"

  // The sources, read; one that cannot be read is a problem and left out.
  val sources: List[(Text, Text)] = config.sources.bind: (directory: Text) =>
    val root = java.nio.file.Path.of(directory.s).nn
    if !java.nio.file.Files.isDirectory(root) then
      report(Block.Diagnostic(Tone.Failure, Block.Location(directory), List(Block.paragraph(t"This source directory does not exist."))))
      Nil
    else
      val stream = java.nio.file.Files.walk(root).nn
      val files: List[Text] =
        try List.from(stream.iterator.nn.asScala.map(_.toString.tt).filter(_.ends(t".scala")).toList.sortBy(_.s))
        finally stream.close()

      files.bind: (file: Text) =>
        try List((file, Text(java.nio.file.Files.readString(java.nio.file.Path.of(file.s)).nn)))
        catch case error: Throwable =>
          report(Compilation.unreadable(file))
          Nil

  // The dependencies first, walked through the session before it compiles anything: a walk
  // after a compile under the project's options reads next to nothing.
  val dependencies: Api = config.classpath.filter(_.ends(t".jar")).stdlib.foldLeft(Api.empty): (acc, jar) =>
    val walked: Api = Server.jar(jar):
      checker.context.lay(Api.empty): (context: dotty.tools.dotc.core.Contexts.Context) =>
        Inspection.classpath(List(jar), context) match
          case api: Api => api
          case error: Throwable =>
            report(failed(t"The dependency $jar could not be inspected", error))
            Api.empty

    acc.merge(walked)


  val built: Boolean =
    if sources.nil then false else
      // A fresh output: what the sources no longer declare must not linger.
      val root = java.nio.file.Path.of(cache.s).nn
      if java.nio.file.Files.exists(root) then
        val stream = java.nio.file.Files.walk(root).nn
        try stream.sorted(java.util.Comparator.reverseOrder()).nn.forEach { p => java.nio.file.Files.delete(p) }
        finally stream.close()

      val notices: List[Notice] = checker.build(Map.from(sources.stdlib), cache)
      val texts: sci.Map[Text, Text] = sci.Map.from(sources.stdlib)

      notices.each: (notice: Notice) =>
        val tone: Tone = notice.importance match
          case Importance.Error   => Tone.Failure
          case Importance.Warning => Tone.Warning
          case _                  => Tone.Info

        // The notice names its file as the compiler does, by its name alone or its path; the
        // line is counted from the offset in the file's text.
        val source: Option[(Text, Text)] =
          texts.find { (path, _) => path == notice.file || path.ends(t"/${notice.file}") }

        val line: Optional[Int] = notice.span.let(_.offset).let: (offset: Ordinal) =>
          source.map { (_, text) => text.s.substring(0, offset.n0.min(text.s.length)).nn.count(_ == '\n') + 1 }.getOrElse(Unset)

        // The compiler's own summary ("3 warnings found") is no diagnostic.
        val summary: Boolean = notice.message.s.matches("\\d+ (warnings?|errors?) found")

        if tone != Tone.Info && !summary then
          report(Block.Diagnostic(tone, Block.Location(source.map(_(0)).getOrElse(notice.file), line), List(Block.paragraph(notice.message))))

      Out.println(t"fuel: compiled ${sources.size.show} sources (${notices.filter(_.importance == Importance.Error).size.show} errors)")
      notices.filter(_.importance == Importance.Error).nil

  // The project's own output, from the inspector over what the build saved.
  val own: Api =
    if !built then Api.empty else Inspection.walk(List(cache), Nil, config.classpath) match
      case api: Api => api
      case error: Throwable =>
        report(failed(t"The project's own code could not be inspected", error))
        Api.empty

  val inspected: Api = dependencies.merge(own)

  Inspection.failures() match
    case (count, error: Throwable) if count > 0 =>
      val trace: Text = Text(java.lang.String.join("; ", error.getStackTrace.nn.take(6).map(_.toString)*).nn)
      report(Block.Diagnostic(Tone.Warning, Unset, List(Block.paragraph(t"${count.show} members could not be read into the API model; the first failed with: ${error.toString.tt} ($trace)"))))
    case _ => ()

  // The overlays, read and merged.
  val overlays: List[Overlay] = config.overlays.bind: (directory: Text) =>
    val root = java.nio.file.Path.of(directory.s).nn
    if !java.nio.file.Files.isDirectory(root) then
      report(Block.Diagnostic(Tone.Failure, Block.Location(directory), List(Block.paragraph(t"This overlay directory does not exist."))))
      Nil
    else
      val stream = java.nio.file.Files.walk(root).nn
      val files: List[Text] =
        try List.from(stream.iterator.nn.asScala.map(_.toString.tt).filter(_.ends(t".tel")).toList.sortBy(_.s))
        finally stream.close()

      files.bind: (file: Text) =>
        try Overlay.read(file, Text(java.nio.file.Files.readString(java.nio.file.Path.of(file.s)).nn)) match
          case overlay: Overlay          => List(overlay)
          case diagnostic: Block.Diagnostic =>
            report(diagnostic)
            Nil
        catch case error: Throwable =>
          report(Compilation.unreadable(file))
          Nil

  val (api, merging) = if overlays.nil then (inspected, Nil: List[Block]) else Overlay.merge(inspected, overlays)
  merging.each(report)

  if !overlays.nil then Out.println(t"fuel: ${overlays.size.show} overlays")

  // The sources, as one hash: their paths and their texts.
  val hashed: Text = Hashes.of(sources.bind { (path: Text, text: Text) => List(path, text) }*)

  (api, problems.reverse, hashed)

// The pages of the project's own entities, each rendered — its examples compiled as a
// document's are, its references resolved in its own scope — unless the output keeps it under
// the key of its inputs: everything the compile depends on (`environment`), the entity, and the
// members its page lists. Their problems are kept with them.
private def rendered(api: Api, checker: Checker, store: Optional[Store], environment: Text)
:   (sci.Map[Text, List[Block]], List[Block]) =

  @scala.caps.unsafe.untrackedCaptures
  var problems: List[Block] = Nil

  def render(entity: Api.Entity): (List[Block], List[Block]) =
    val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil))
    val resolver: Resolver = Resolver(api, scope)
    val place: Text = entity.doc.let(_.file).or(entity.key)
    val scala: Embedding.ScalaCode = Embedding.ScalaCode(checker, place, false, Unset, resolver)
    val blocks: List[Block] = ApiPages.page(api, entity, resolver, scope, List(scala, Embedding.JavaCode))

    val ambiguous: List[Block] = resolver.ambiguities.map: (reference: Text) =>
      val text: Text = t"`$reference` names more than one entity, so it is not linked; qualify it."
      Block.Diagnostic(Tone.Warning, Block.Location(place, entity.doc.let(_.line)), List(Block.paragraph(text)))

    (blocks, scala.problems + ambiguous)

  val own: scala.List[Api.Entity] = api.entities.values.filter(_.own).toList.sortBy(_.key.s)

  val pages: scala.List[(Text, List[Block])] = own.map: (entity: Api.Entity) =>
    val (blocks, raised) = store.lay(render(entity)): (store: Store) =>
      val members: Text = api.children(entity).map(_.toString.tt).join(t"\n")
      val key: Text = Hashes.of(environment, t"page", entity.toString.tt, members)
      val page: Output.Page = store.page(key, entity.key, entity.path)(render(entity))
      (page.blocks, page.problems)

    problems = raised.reverse + problems
    (entity.key, blocks)

  (sci.Map.from(pages), problems.reverse)

// One document set in the terminal: its blocks at the terminal's width, paragraphs justified
// and hyphenated, code highlighted, an animated block as its steps in sequence; its problems
// after it. Compiled as `compile` compiles, with the session and the classpath the
// configuration gives, so what the page shows is what the dashboard would.
private def show(config: Workspace.Config, directory: Text, operand: Optional[Text])
  ( using Stdio, Environment )
:   Exit | UsageError.type | NoDocument.type | CompileFailed.type =

  operand match
    case Unset =>
      Out.println(t"fuel: usage: fuel show <file>")
      UsageError

    case operand: Text =>
      val path: Text = absolute(directory, operand)

      compiled(config, path) match
        case Unset =>
          Out.println(t"fuel: $path could not be read")
          NoDocument

        case doc: Doc =>
          import hyphenations.englishHyphenation
          val renderer = TerminalRenderer(justify = true)
          renderer.blocks(doc.blocks, width).each: (line: Teletype) => Out.println(line)

          if !doc.problems.nil then
            Out.println(blank)
            renderer.blocks(doc.problems, width).each: (line: Teletype) => Out.println(line)

          if doc.problems.nil then Exit.Ok else CompileFailed

private def blank: Teletype = e""

// A path as the command line gives it, from the directory `fuel` was run in.
private def absolute(directory: Text, operand: Text): Text =
  if operand.starts(t"/") then operand else t"$directory/$operand"

// One document compiled as `compile` compiles it, with the session and the classpath the
// configuration gives, or highlighted alone where there is no usable classpath or the
// session fails; `Unset` if it cannot be read.
private def compiled(config: Workspace.Config, path: Text): Optional[Doc] =
  def exists(entry: Text): Boolean = java.nio.file.Files.exists(java.nio.file.Path.of(entry.s))
  val checked: Boolean = !config.classpath.nil && config.classpath.all(exists(_))
  val regime: Compilation.Regime = Compilation.Regime(config.session, config.fixtures, glossary = glossaries(config)(0))

  if checked then
    try Checking.session(config.classpath, config.options): (checker: Checker) =>
      Compilation.document(path, checker, regime)
    catch case error: Throwable => Compilation.document(path, Checker.tokenized, regime)
  else
    Compilation.document(path, Checker.tokenized, regime)

// A failure of the compiler itself, which has no place in a document, as a diagnostic.
private def failed(what: Text, error: Throwable): Block =
  val detail: List[Block] = List(Block.paragraph(error.toString.tt))
  Block.Diagnostic(Tone.Failure, Unset, List(Block.paragraph(what)), Nil, detail)
