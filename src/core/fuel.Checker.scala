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

import ambience.systems.javaBaseSystem
import logging.silentLogging
import strategies.throwUnsafely

object Checker:
  object tokenized extends Checker:
    def check(code: Text, mode: Preamble.Mode, context: Optional[harlequin.Scala.Context]): SourceCode =
      harlequin.Scala.highlight(code, context)

    def enter(name: Text, unit: Text): Boolean = false
    def build(sources: Map[Text, Text], output: Text): List[Notice] = Nil
    def context: Optional[dotty.tools.dotc.core.Contexts.Context] = Unset
    def compiles: Boolean = false

// How a Scala fragment is highlighted: at tokenized depth, which needs nothing, or in a warm
// compiler session on a project's classpath, as far as its `mode` asks — parsed, typechecked
// or compiled through every phase — which is what makes a fragment's validity part of its
// rendering (its diagnostics are in the `SourceCode`).
trait Checker:
  def check(code: Text, mode: Preamble.Mode, context: Optional[harlequin.Scala.Context]): SourceCode

  // Compiles a unit into the session under its own source name, so that what it defines stays
  // in scope for every later check, as a REPL's lines do for one another; true if it compiled.
  def enter(name: Text, unit: Text): Boolean

  // Compiles a project's own sources, as one compile in the session, and writes what it emits
  // — classfiles and TASTy — beneath `output`, so that they can be inspected and put on a
  // classpath; the compiler's notices say what did not compile.
  def build(sources: Map[Text, Text], output: Text): List[Notice]

  // The session's own context, whose symbol table loads the classpath lazily — what walks a
  // dependency's API without unpickling a body — good only on the compiler's thread.
  def context: Optional[dotty.tools.dotc.core.Contexts.Context]

  def compiles: Boolean

// One warm session per compile, opened on the configured classpath with the configured
// options; every fragment of every document is typechecked in it.
object Checking:
  // Every use of the compiler — tokenizing, which uses its scanner, as much as a session —
  // runs on this one thread: dotc pins a context base to the thread of its first run, and a
  // run from another thread overflows (`RecursionOverflow`, with no frames to say so). The
  // daemon serves each invocation on a connection thread of its own, so without this the
  // first compile after a highlight would fail. The thread's stack is generous, since the
  // compiler recurses deeply.
  private val executor: juc.ExecutorService =
    juc.Executors.newSingleThreadExecutor: (runnable: Runnable | Null) =>
      val thread = Thread(null, runnable, "fuel-compiler", 0L)
      thread.setDaemon(true)
      thread

    . nn

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var compilerThread: Optional[Thread] = Unset

  // Runs `body` on the compiler thread and waits for it, or directly when already there.
  def exclusive[result](body: => result): result =
    if compilerThread == Thread.currentThread then body else
      val callable: juc.Callable[result] = scala.caps.unsafe.unsafeAssumePure: () =>
        compilerThread = Thread.currentThread.nn
        body

      try executor.submit(callable).nn.get.nn
      catch case error: juc.ExecutionException => throw error.getCause.nn

  def session[result](classpath: List[Text], options: List[Text])(body: Checker => result)
  :   result =

    val entries: List[Classpath.Entry.Directory | Classpath.Entry.Jar] =
      classpath.map: (entry: Text) =>
        if entry.ends(t".jar") then Classpath.Entry.Jar(entry) else Classpath.Entry.Directory(entry)

    session(LocalClasspath(entries*), options)(body)

  // A session on a classpath already resolved — a classloader's, say, as the tests use.
  def session[result](classpath: LocalClasspath, options: List[Text])(body: Checker => result)
  :   result =

    exclusive(open(classpath, options)(body))

  private def open[result](classpath: LocalClasspath, options: List[Text])(body: Checker => result)
  :   result =

    given (CompileEvent is Loggable) = Loggable.silent[CompileEvent]

    val scalac: Scalac[3.9, Universe.Classfile] =
      Scalac[3.9](options.map { (option: Text) => Scalac.Option[3.9](option) })

    scalac.on(classpath).session:
      val frontend: Scalac.Session.Frontend = compilation.frontend
      given Scalac.Session.Frontend = frontend
      import highlighting.sessionScala

      // Compiled highlighting in the same session — every phase before bytecode generation,
      // which is where capture checking reports — from the frontend the session gives of
      // itself, as harlequin's own session highlighting is built.
      val compiled: harlequin.Highlight = new harlequin.Highlight:
        def depth: harlequin.Depth = harlequin.Depth.Compiled

        val compilation: Optional[harlequin.Highlight.Compilation] =
          new harlequin.Highlight.Compilation:
            def arguments: List[Text] = frontend.options
            def classpath: Text = frontend.classpath
            def compiler(): dotty.tools.dotc.Compiler = frontend.compiler
            override def context(): Optional[dotty.tools.dotc.core.Contexts.Context] = frontend.context

      // The checker captures the session, which `body` never outlives: it is vouched pure so
      // that `body` may take a plain `Checker`.
      val checker: Checker = scala.caps.unsafe.unsafeAssumePure:
        new Checker:
          def check(code: Text, mode: Preamble.Mode, context: Optional[harlequin.Scala.Context]): SourceCode =
            mode match
              case Preamble.Mode.Typecheck => harlequin.Scala.highlight(code, context)
              case Preamble.Mode.Compile   => harlequin.Scala.highlight(code, context)(using compiled)
              case Preamble.Mode.Parse     => parsed(frontend, code, context)

          def enter(name: Text, unit: Text): Boolean =
            compilation.compile(Map(name -> unit)).errors == 0

          def build(sources: Map[Text, Text], output: Text): List[Notice] =
            val process = compilation.compile(sources)
            val root = java.nio.file.Path.of(output.s).nn

            process.classfiles.each: (path, data) =>
              val target = root.resolve(path.encode.s).nn
              java.nio.file.Files.createDirectories(target.getParent.nn)
              java.nio.file.Files.write(target, Array.unsafeJvm(data))
              ()

            process.notices

          def context: Optional[dotty.tools.dotc.core.Contexts.Context] = frontend.context
          def compiles: Boolean = true

      body(checker)

  // A fragment parsed and no more, in the session: its syntax errors, from a run stopped after
  // the parser, on its tokens highlighted without types.
  private def parsed
    ( frontend: Scalac.Session.Frontend, code: Text, context: Optional[harlequin.Scala.Context] )
  :   SourceCode =

    import dotty.tools.dotc.core.Contexts
    import dotty.tools.dotc.reporting.{Diagnostic as CompilerDiagnostic, HideNonSensicalMessages, Reporter, UniqueMessagePositions}
    import dotty.tools.dotc.util.SourceFile

    val text: Text = harlequin.Scala.preprocess(code, context)

    @scala.caps.unsafe.untrackedCaptures
    var collected: List[harlequin.Diagnostic] = Nil

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: CompilerDiagnostic)(using Contexts.Context): Unit =
        val pos = diagnostic.pos.span

        if pos.exists then
          val importance = diagnostic.level match
            case dotty.tools.dotc.interfaces.Diagnostic.ERROR   => harlequin.Diagnostic.Importance.Error
            case dotty.tools.dotc.interfaces.Diagnostic.WARNING => harlequin.Diagnostic.Importance.Warning
            case _                                              => harlequin.Diagnostic.Importance.Info

          val span = Span.offset(pos.start.z, pos.end - pos.start)
          collected = harlequin.Diagnostic(span, diagnostic.message.tt, importance) :: collected

    val base: Contexts.FreshContext = frontend.context.fresh
    base.setReporter(reporter)
    base.setSetting(base.settings.YstopAfter, scala.collection.immutable.List("parser"))
    given Contexts.Context = base
    frontend.compiler.newRun.compileSources(scala.collection.immutable.List(SourceFile.virtual("<fuel>", text.s)))

    harlequin.Scala.highlight(code, context).copy(diagnostics = collected.reverse)
