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

import java.util.concurrent as juc

// Work of which only the latest request matters, done apart from the thread that asks for it:
// a request made while one is in hand replaces any other still waiting, so the next to be done
// is always the latest, and those it overtook are never done at all. The search field asks for
// a search at every keystroke, while the websocket's messages are handled one at a time:
// were each search done there, a burst of keystrokes would be searched for in turn, every
// intermediate text included. A worker runs only while there is something to do, on a virtual
// thread, and ends when nothing is waiting, so there is nothing to stop when a tab goes.
final class Latest[request](work: request -> Unit):
  private val waiting: juc.atomic.AtomicReference[Option[request]] =
    juc.atomic.AtomicReference(None)

  private val running: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(false)

  def apply(request: request): Unit =
    waiting.set(Some(request))
    if running.compareAndSet(false, true) then start()

  private def start(): Unit =
    val runnable: Runnable = () => drain()
    java.lang.Thread.ofVirtual().nn.start(runnable)

  // A failure of one request is the work's to report; it does not stop the next.
  private def drain(): Unit =
    waiting.getAndSet(None).nn match
      case Some(request) =>
        try work(request) catch case _: Exception => ()
        drain()

      // Nothing waits: the worker ends, unless a request arrived after `waiting` was found
      // empty and before `running` was cleared, which then found the worker still running and
      // left the request to it.
      case None =>
        running.set(false)
        if waiting.get.nn.isDefined && running.compareAndSet(false, true) then drain()
