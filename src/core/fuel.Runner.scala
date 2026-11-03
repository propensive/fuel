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

import java.io as ji

import soundness.*

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Diversions}
import yossarian.*

// Runs a fragment's object, as Flame runs a line: on a thread of its own, with what it writes
// to the standard streams — Java's and Scala's, in the order written — captured as one text,
// and what it throws kept; and reads that text as a terminal would, into the lines and styled
// runs of an `Output` block.
object Runner:
  case class Outcome(text: Text, error: Optional[Throwable])

  def run(body: => Unit): Outcome =
    val sink: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
    val stream: ji.PrintStream = ji.PrintStream(sink, true, "UTF-8")

    @scala.caps.unsafe.untrackedCaptures
    @volatile
    var failure: Optional[Throwable] = Unset

    val runnable: Runnable = scala.caps.unsafe.unsafeAssumePure: () =>
      try
        Diversions.threaded(stream, stream):
          scala.Console.withOut(stream):
            scala.Console.withErr(stream)(body)
      catch
        case error: ExceptionInInitializerError => failure = Optional(error.getCause).or(error)
        case error: Throwable                   => failure = error

    // Its own thread, with a generous stack, so that the fragment is neither on the compiler's
    // thread nor bound by it; `Diversions` routes by thread, so this is the thread to route.
    val thread: Thread = Thread(null, runnable, "fuel-run", 64L*1024*1024)
    thread.start()
    thread.join()
    stream.flush()
    Outcome(sink.toString("UTF-8").nn.tt, failure)

  // The captured text as a terminal shows it: its sequences interpreted (a colour set, a
  // carriage return, a cursor moved), each line's cells in runs of one style, and a colour the
  // output left to the terminal left to the reader's own. Text holding no sequence is itself,
  // unstyled, and text the terminal cannot read is stripped of its sequences.
  def styled(raw: Text): (Text, List[Block.Run]) =
    val none: List[Block.Run] = Nil

    if !raw.contains(t"\u001b") && !raw.contains(t"\r") then (raw, none) else
      val lines: List[Text] = raw.cut(t"\n")
      val count: Int = if raw.s.endsWith("\n") then lines.size - 1 else lines.size
      val width: Int = lines.fold(1): (longest: Int, line: Text) => longest.max(line.length)

      safely(Pty(width + 1, count + 1).consume(raw)) match
        case pty: Pty => read(pty.buffer, count)
        case _        => (stripped(raw), none)

  private def stripped(raw: Text): Text =
    raw.s.replaceAll("\u001b\\[[0-9;?]*[a-zA-Z]", "").nn.replaceAll("\u001b.", "").nn.tt

  // The first `count` rows of a screen, each trimmed of the blank cells after its last and
  // ended by a newline, as runs of one style; the runs are left out when none styles anything.
  private def read(screen: Screen[Style], count: Int): (Text, List[Block.Run]) =
    val plain: Style = Style()

    @scala.caps.unsafe.untrackedCaptures
    var segments: List[(Style, Text)] = Nil

    var y: Int = 0

    while y < count do
      var last: Int = screen.width - 1

      while last >= 0 && screen.char(last.z, y.z) == ' ' && screen.style(last.z, y.z) == plain
      do last -= 1

      var x: Int = 0

      while x <= last do
        if !screen.isWideTrailing(x.z, y.z) then
          val style: Style = screen.style(x.z, y.z)
          val grapheme: Text = screen.grapheme(x.z, y.z).text

          segments = segments match
            case (`style`, text) :: rest => (style, t"$text$grapheme") :: rest
            case other                   => (style, grapheme) :: other

        x += 1

      segments = segments match
        case (style, text) :: rest if last >= 0 => (style, t"$text\n") :: rest
        case other                              => (plain, t"\n") :: other

      y += 1

    val ordered: List[(Style, Text)] = segments.reverse
    val text: Text = ordered.map(_(1)).join
    val runs: List[Block.Run] = ordered.map: (pair: (Style, Text)) => run(pair(0), pair(1).length)
    val styled: Boolean = runs.exists: (run: Block.Run) => run != Block.Run(run.length)
    (text, if styled then runs else Nil)

  private def run(style: Style, length: Int): Block.Run =
    def rgb(chroma: Chroma): Block.Rgb = Block.Rgb(chroma.red, chroma.green, chroma.blue)

    Block.Run
      ( length,
        if style.defaultForeground then Unset else rgb(style.foreground),
        if style.defaultBackground then Unset else rgb(style.background),
        style.bold,
        style.faint,
        style.italic,
        style.underline,
        style.strike,
        style.reverse )
