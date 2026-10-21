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
  val Blocks  = Subcommand("blocks", "write one document's blocks as TEL, for another tool")

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

    // `fuel compile`: read the settings here, where completion mode registers them, and
    // compile within the execution.
    // `fuel show <file>`: one document, compiled as `compile` would, set in the terminal.
    case ui.Show() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).prim
      ambient(show(config, directory, operand))

    // `fuel blocks <file>`: one document, compiled as `compile` would, as TEL.
    case ui.Blocks() :: rest =>
      val config: Workspace.Config = Workspace.load(directory)
      val operand: Optional[Text] = rest.map(_()).filter(!_.starts(t"-")).prim
      ambient(blocks(config, directory, operand))

    case ui.Compile() :: _ =>
      val config: Workspace.Config = Workspace.load(directory)
      ambient(compile(config))

    case _ =>
      ambient:
        Out.println(t"Usage: fuel <subcommand>")
        Out.println(t"")
        Out.println(t"  compile    compile the configured directories of Markdown documents")
        Out.println(t"  show       render one document in the terminal")
        Out.println(t"  blocks     write one document's blocks as TEL, for another tool")
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

private def compile(config: Workspace.Config)(using Stdio, Environment)
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

    // The API first, since the documents link to it: the sources compiled and saved, their
    // output and the dependencies walked, the overlays merged, and the pages of the project's
    // own documented entities rendered with their examples compiled.
    @scala.caps.unsafe.untrackedCaptures
    var apiProblems: List[Block] = Nil

    val outcome: Optional[(Corpus, Api, sci.Map[Text, List[Block]])] =
      if !checked then Unset else
        try Checking.session(config.classpath, config.options): (checker: Checker) =>
          val (api, pages, problems) = model(config, checker)
          apiProblems = problems

          // Each document is reported as it is done, since a corpus can take a long while.
          val regime: Compilation.Regime =
            Compilation.Regime(config.session, config.fixtures, (path: Text, problems: Int) =>
              Out.println(t"fuel: $path: ${problems.show} ${if problems == 1 then t"problem" else t"problems"}"),
              (_: Text) => Resolver(api, Scope.document(config.imports)))

          (Compilation.corpus(config.documents, checker, regime), api, pages)
        catch case error: Throwable =>
          failures = failed(t"The fragments could not be compiled", error) :: failures
          Unset

    val corpus: Corpus =
      outcome.let(_(0)).or:
        try Compilation.corpus(config.documents, Checker.tokenized, Compilation.Regime(config.session, config.fixtures)) catch case error: Throwable =>
          failures = failed(t"The documents could not be highlighted", error) :: failures
          Corpus(Nil)

    val api: Api = outcome.let(_(1)).or(Api.empty)
    Server.publish(corpus, api, outcome.let(_(2)).or(sci.Map()), config.imports)

    corpus.sections.each: (section: Corpus.Section) =>
      Out.println(t"${section.name}: ${section.docs.size.show} documents from ${section.root}")

    if !api.entities.isEmpty then
      val own: Int = api.entities.values.count(_.own)
      val documented: Int = api.entities.values.count { e => e.own && e.doc.present }
      Out.println(t"api: ${api.entities.size.show} entities, ${own.show} of the project's own, ${documented.show} documented")

    // Every diagnostic, rendered as the terminal renders them: at its place, with its excerpt.
    val problems: List[Block] = failures.reverse + apiProblems + corpus.problems
    val renderer = TerminalRenderer()
    renderer.blocks(problems, width).each: (line: Teletype) => Out.println(line)

    if config.classpath.nil
    then Out.println(t"fuel: no classpath is configured, so no fragment was compiled")

    if problems.nil then Exit.Ok else CompileFailed

// The API model of a project, in the session: its sources compiled and saved beneath the
// cache (`$XDG_CACHE_HOME/fuel/<project>/classes`, or `~/.cache/fuel/…`), the output and each
// dependency jar walked (a jar once per version, kept by the daemon), the overlays read and
// merged, and the pages of the project's own documented entities rendered — their examples
// compiled as a document's are — with every problem found on the way.
private def model(config: Workspace.Config, checker: Checker)(using Stdio)
:   (Api, sci.Map[Text, List[Block]], List[Block]) =

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

  // The pages of the project's own documented entities, their examples compiled.
  val pages: sci.Map[Text, List[Block]] =
    sci.Map.from:
      api.entities.values.filter { e => e.own && e.doc.present }.map: entity =>
        val scope: Scope = Scope.of(entity, entity.doc.let(_.imports).or(Nil))
        val resolver: Resolver = Resolver(api, scope)
        val scala: Embedding.ScalaCode = Embedding.ScalaCode(checker, entity.doc.let(_.file).or(entity.key), false, Unset, resolver)
        val blocks: List[Block] = ApiPages.page(api, entity, resolver, scope, List(scala, Embedding.JavaCode))
        scala.problems.each(report)
        resolver.ambiguities.each { (reference: Text) => report(Block.Diagnostic(Tone.Warning, Block.Location(entity.doc.let(_.file).or(entity.key), entity.doc.let(_.line)), List(Block.paragraph(t"`$reference` names more than one entity, so it is not linked; qualify it.")))) }
        (entity.key, blocks)

  if !overlays.nil then Out.println(t"fuel: ${overlays.size.show} overlays, ${pages.size.show} pages")

  (api, pages, problems.reverse)

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

// One document's blocks as TEL, one TEL document per block (see `Fragments`), on standard
// output: what Flame or any other tool reads to show the document as Fuel renders it.
private def blocks(config: Workspace.Config, directory: Text, operand: Optional[Text])
  ( using Stdio )
:   Exit | UsageError.type | NoDocument.type =

  operand match
    case Unset =>
      Out.println(t"fuel: usage: fuel blocks <file>")
      UsageError

    case operand: Text =>
      val path: Text = absolute(directory, operand)

      compiled(config, path) match
        case Unset =>
          Out.println(t"fuel: $path could not be read")
          NoDocument

        case doc: Doc =>
          Out.print(Fragments.tel(doc.blocks))
          Exit.Ok

// A path as the command line gives it, from the directory `fuel` was run in.
private def absolute(directory: Text, operand: Text): Text =
  if operand.starts(t"/") then operand else t"$directory/$operand"

// One document compiled as `compile` compiles it, with the session and the classpath the
// configuration gives, or highlighted alone where there is no usable classpath or the
// session fails; `Unset` if it cannot be read.
private def compiled(config: Workspace.Config, path: Text): Optional[Doc] =
  def exists(entry: Text): Boolean = java.nio.file.Files.exists(java.nio.file.Path.of(entry.s))
  val checked: Boolean = !config.classpath.nil && config.classpath.all(exists(_))
  val regime: Compilation.Regime = Compilation.Regime(config.session, config.fixtures)

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
