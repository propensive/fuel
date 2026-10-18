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

// Excluded from the umbrella: `Language` and `Token`, which are Pyrocosm's here (the model's own,
// which `Block.Code` carries), not cosmopolite's and harlequin's.
import soundness.{Language as _, Token as _, *}

import pyrocosm.{Block, Inline, Language, Token}

// A fenced code block's rendering, chosen by the fence's info string: the first embedding in
// scope that answers wins, and a fence nobody answers is shown plain under its own language
// name. Amok's `Formattable` (punctuation's, yielding HTML) with blocks as the result, so that
// the same embedding serves the web, the terminal and a fragment.
object Embedding:
  // Scala and Java through harlequin, which tokenises with dotc's own scanner, at tokenized
  // depth: nothing is typechecked yet (M2), so the classpath is not consulted.
  object ScalaCode extends Embedding:
    def embed(info: List[Text], content: Text): Optional[List[Block]] =
      if info.prim == t"scala"
      then List(Block.Code(Language.Scala, highlight(harlequin.Scala, content)))
      else Unset

  object JavaCode extends Embedding:
    def embed(info: List[Text], content: Text): Optional[List[Block]] =
      if info.prim == t"java"
      then List(Block.Code(Language.Java, highlight(harlequin.Java, content)))
      else Unset

  val standard: List[Embedding] = List(ScalaCode, JavaCode)

  // The fallback: the content line by line, as unparsed tokens.
  def plain(info: List[Text], content: Text): Block =
    Block.Code(info.prim.lay(Language.Plain) { (name: Text) => Language(name) }, lines(content))

  def lines(content: Text): List[Block.Line] =
    trimmed(content).cut(t"\n").map: (line: Text) => Block.Line(List(Token.plain(line)))

  // A fence's content ends with the newline that closed its last line, which is not a line.
  def trimmed(content: Text): Text =
    if content.ends(t"\n") then Text(content.s.substring(0, content.s.length - 1).nn) else content

  private def highlight(language: harlequin.ProgrammingLanguage, content: Text): List[Block.Line] =
    import pyrocosm.modelLines
    language.highlight(trimmed(content)).modelLines

trait Embedding:
  def embed(info: List[Text], content: Text): Optional[List[Block]]
