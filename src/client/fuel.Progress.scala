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
import scala.collection.mutable as scm

import soundness.{Compilation as _, Query as _, *}

import pyrocosm.{Block, Board, Status, TerminalRenderer}
import palettes.solarizedDarkGaugePalette
import tableStyles.thickTableStyle
import textMetrics.uniformMetric

// What a compile has done against what it has to do, and where its problems go. The work is
// counted in units of fixed weight — a dependency jar walked, the sources built, their output
// inspected, the overlays merged, a page rendered, a document compiled, the links checked — and
// the gauge is the weight done over the weight planned, the caption the phase in hand; a count
// not known until a phase begins (the pages, until the API is built) is guessed until it is.
// A problem goes to the board as it is found, where there is one, or straight to the terminal,
// so that it is seen at once either way; and every problem is kept, for the report once the
// board has closed.
object Progress:
  enum Phase:
    case Jars, Build, Inspect, Overlays, Pages, Documents, Links

    def weight: Int = this match
      case Jars      => 3
      case Build     => 4
      case Inspect   => 1
      case Overlays  => 1
      case Pages     => 1
      case Documents => 3
      case Links     => 1

    def caption: Text = this match
      case Jars      => t"Reading the dependencies"
      case Build     => t"Compiling the sources"
      case Inspect   => t"Reading the API"
      case Overlays  => t"Merging the overlays"
      case Pages     => t"Rendering the API pages"
      case Documents => t"Compiling the documents"
      case Links     => t"Checking the links"

final class Progress(board: Optional[Board], width: Int)(using Stdio):
  import Progress.Phase

  // The counts, read and written under the lock, from the compiler's thread and the board's.
  private val totals: scm.HashMap[Phase, Int] = scm.HashMap()
  private val done: scm.HashMap[Phase, Int] = scm.HashMap()
  private val found: juc.ConcurrentLinkedQueue[Block] = juc.ConcurrentLinkedQueue()
  private val renderer: TerminalRenderer = TerminalRenderer()

  // How much of a phase there is; said again when a guess gives way to a count.
  def plan(phase: Phase, total: Int): Unit = synchronized { totals(phase) = total.max(0) }

  // One more unit of a phase done, and the gauge brought up to date.
  def step(phase: Phase, count: Int = 1): Unit =
    val (now, total, part) = synchronized:
      val now: Int = done.getOrElse(phase, 0) + count
      done(phase) = now
      (now, totals.getOrElse(phase, now), fraction)

    board.let(_.gauge(Status.Fraction(part), t"${phase.caption}, ${now.toString.tt} of ${total.toString.tt}"))

  // A phase done whole, whatever was planned for it.
  def finish(phase: Phase): Unit =
    synchronized { done(phase) = totals.getOrElse(phase, 1) }
    step(phase, 0)

  // The weight done over the weight planned; under the lock, or by a caller holding it.
  def fraction: Double = synchronized:
    val planned: Double = totals.iterator.map { (phase, total) => (total*phase.weight).toDouble }.sum
    val completed: Double = totals.iterator.map { (phase, total) => (done.getOrElse(phase, 0).min(total)*phase.weight).toDouble }.sum
    if planned == 0.0 then 0.0 else (completed/planned).min(1.0)

  // A problem, shown as it is found: on the board, or on the terminal where there is none.
  def report(block: Block): Unit =
    found.add(block)
    board.lay(renderer.blocks(List(block), width).each { (line: Teletype) => Out.println(line) }) { (board: Board) => board.append(block) }

  def reports(blocks: List[Block]): Unit = blocks.each(report)

  // Every problem found, in order; and whether they were already printed as they came.
  def problems: List[Block] = List.from(found.iterator.nn.asScala)
  def streamed: Boolean = board.absent
