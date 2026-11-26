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

// The bolt codes of Fuel, `↯/f.25` to `↯/f.28`: the subdivisions of the letter Fuel shares with
// Pyrocosm and its other tools (`pyrocosm.Bolts`), one for each of Fuel's modules. The record of
// what every code means, and of each error's variants, is `bolt.tel` in propensive/pyrocosm.
// Declared here, in the module every other module of Fuel's depends on.
object FuelBolts:
  final val core     = 25
  final val api      = 26
  final val client   = 27
  final val launcher = 28

  // The code of the `error`th error of a subdivision, or of its `variant`th variant.
  def apply(subdivision: Int, error: Int, variant: Int = 0): fulminate.Bolt =
    pyrocosm.Bolts(subdivision, error, variant)

  // The core's errors, each a kind of problem a document or a file beside it has: a file which
  // cannot be read; a fence which is not closed; a reference which names several entities; a
  // glossary; a presentation; a section's file; a table; of a fence of Scala, the sample file
  // it writes, the fixture compiled before it, a frame of its animation, its run, and the
  // fragment itself; and an Orthant store. The fifth and the ninth were of the translations
  // Fuel once kept itself, and are no longer raised.
  final val unreadable   = 1
  final val unclosed     = 2
  final val ambiguous    = 3
  final val glossary     = 4
  final val presentation = 6
  final val sectionFile  = 7
  final val table        = 8
  final val sample       = 10
  final val fixture      = 11
  final val frame        = 12
  final val run          = 13
  final val fragment     = 14
  final val depot        = 15

  // The API's: a glossary as another language reads it; what the compiler could not read; a
  // link; an overlay; a search's query; and a schema declared twice. The fifth was of an
  // overlay's translation, as Fuel once kept one, and is no longer raised.
  final val glossaryTranslation = 1
  final val unread              = 2
  final val link                = 3
  final val overlay             = 4
  final val query               = 6
  final val schema              = 7

  // The client's, each printed after its code's label or a diagnostic of it: a presentation's
  // alignment; the configuration's sections; a failure of the daemon itself; a command line
  // which was not understood; a tracker; a configured path which does not exist; and the
  // compiler, which failed rather than reporting. The second was `fuel translate`'s, and is
  // no longer raised.
  final val alignment   = 1
  final val sections    = 3
  final val daemon      = 4
  final val usage       = 5
  final val tracker     = 6
  final val missing     = 7
  final val compiler    = 8
