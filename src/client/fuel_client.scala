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

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Compilation as _, *}

import backstops.silentBackstop
import dysasymptotics.linearSize
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
object CompileFailed extends Status(4, t"a code fragment did not do as its preamble said")

// Fuel's user interface, in one namespace: its subcommands, flags and settings.
object ui:
  val Compile = Subcommand("compile", "compile the configured directories of Markdown documents")
  val Serve   = Subcommand("serve", "serve the compiled documents on the web until Ctrl+C")

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
    case ui.Compile() :: _ =>
      val config: Workspace.Config = Workspace.load(directory)
      ambient(compile(config))

    case _ =>
      ambient:
        Out.println(t"Usage: fuel <subcommand>")
        Out.println(t"")
        Out.println(t"  compile    compile the configured directories of Markdown documents")
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

    val outcome: Optional[Corpus] =
      if !checked then Unset else
        try Checking.session(config.classpath, config.options): (checker: Checker) =>
          // Each document is reported as it is done, since a corpus can take a long while.
          val regime: Compilation.Regime =
            Compilation.Regime(config.session, config.fixtures, (path: Text, problems: Int) =>
              Out.println(t"fuel: $path: ${problems.show} ${if problems == 1 then t"problem" else t"problems"}"))

          Compilation.corpus(config.documents, checker, regime)
        catch case error: Throwable =>
          failures = failed(t"The fragments could not be compiled", error) :: failures
          Unset

    val corpus: Corpus =
      outcome.or:
        try Compilation.corpus(config.documents, Checker.tokenized, Compilation.Regime(config.session, config.fixtures)) catch case error: Throwable =>
          failures = failed(t"The documents could not be highlighted", error) :: failures
          Corpus(Nil)

    Server.publish(corpus)

    corpus.sections.each: (section: Corpus.Section) =>
      Out.println(t"${section.name}: ${section.docs.size.show} documents from ${section.root}")

    // Every diagnostic, rendered as the terminal renders them: at its place, with its excerpt.
    val problems: List[Block] = failures.reverse + corpus.problems
    val renderer = TerminalRenderer()
    renderer.blocks(problems, width).each: (line: Teletype) => Out.println(line)

    if config.classpath.nil
    then Out.println(t"fuel: no classpath is configured, so no fragment was compiled")

    if problems.nil then Exit.Ok else CompileFailed

// A failure of the compiler itself, which has no place in a document, as a diagnostic.
private def failed(what: Text, error: Throwable): Block =
  val detail: List[Block] = List(Block.paragraph(error.toString.tt))
  Block.Diagnostic(Tone.Failure, Unset, List(Block.paragraph(what)), Nil, detail)
