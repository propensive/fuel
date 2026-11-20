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

import codepages.utf8Codepage
import dysasymptotics.linearSize
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Tone}

// A section's file: a TEL file in the section's directory, named beneath the `section` of
// the configuration as its `metadata`, which says what a reader is told of the section — so
// far its `title`, which the navigation shows in place of the section's name:
//
//     title  User guide
//
// Being among the documents, it is in each language they are: an Orthant store which holds
// the section holds its file, and a language reads the title its own text of the file says,
//
//     title  Guide de l'utilisateur
//
// The format is the schema `section` (`fuel/section.schema.tel`), which the file is held to
// in every language.
object SectionFile:
  val none: SectionFile = SectionFile(Unset)

  val schemaResource: Text = t"fuel/section.schema.tel"

  // The schema's document: read through the thread-context classloader, since under Burdock
  // the system one sees only the launcher's own jar.
  def schemaSource: Text =
    Classloader.threadContext.resource(schemaResource).let(_.utf8).or:
      panic(m"the resource $schemaResource is missing")

  lazy val schema: stratiform.Tels =
    import strategies.throwUnsafely
    stratiform.Tels.Validation.validate(schemaSource.read[Tel].as[stratiform.Tels])

  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the section's file has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // The code of a section's file's problem, `↯/f.25.7.<variant>`: a fault the schema finds,
  // which carries the TEL error's own code (1). The second and the third were of a
  // translation Fuel kept itself, and are no longer raised.
  private def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.core, FuelBolts.sectionFile, variant)

  // What a fault the schema finds means in a section's file.
  private def explain(error: Tel.Error, pointer: Text): Text =
    import Tel.Error.Reason
    val last: Text = pointer.cut(t"/").filter(_ != t"").reverse.head.or(t"")

    error.reason match
      case Reason.UnknownKeyword =>
        t"A section's file says its `title`, but not `$last`."

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        t"This has more values than it takes: a value with spaces in it follows two spaces."

      case Reason.NonRepeatableTooMany =>
        t"This says `$last` more than once."

      case _ =>
        error.message.text

  // The diagnostics the schema raises, each at its line of the file.
  private def checked(path: Text, tel: Tel): List[Block] =
    val faults: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error =>
          val pointer: Text = prior.let(_.pointer.encode).or(t"/")
          accrual + (error, pointer, prior.lay(Span.empty)(_.span))

      . protect(Tel.Type.assign(tel, schema))

    faults.items.map: (error, pointer, span) =>
      val line: Int = if span.exists then span.startLine.lay(1)(_.n1) else 1
      val location: Block.Location = Block.Location(path, line.max(1))
      val message: List[Block] = List(Block.paragraph(explain(error, pointer)))
      Block.Diagnostic(Tone.Failure, location, message, code = bolt(1).carrying(error.bolt))

  // A run of prose as the file writes it: its text, the line (from 1) of its keyword, and
  // whether it is on the keyword's line rather than beneath it.
  case class Prose(text: Text, line: Int, inline: Boolean):
    // The line its text begins on.
    def site: Int = if inline then line else line + 1

  // The first `title` of a document read with its positions.
  private def titled(tel: Tel): Optional[Tel.Compound] =
    tel.childCompounds.to[List].seek(_.keyword == t"title")

  // The title of a document read with its positions, as it is written, if there is one.
  private def prose(tel: Tel): Optional[Prose] =
    titled(tel).let: (compound: Tel.Compound) =>
      val words: List[Text] = Compounds.inline(compound).filter(_ != t"")
      val source: List[Text] = Compounds.source(compound).lay(Nil)(List(_)).filter(_ != t"")
      val written: Text = (words + source).join(t" ")

      if written == t"" then Unset
      else Prose(written, Compounds.line(tel, List(t"title")), !words.nil)

  // A section's file read from its text, with what the schema finds wrong with it;
  // or the diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (SectionFile, List[Block]) | Block.Diagnostic =
    import parsing.trackPositions

    recover:
      case error: Tel.Error => Block.Diagnostic.of(error, path, text)

    . protect[(SectionFile, List[Block]) | Block.Diagnostic]:
        val tel: Tel = text.read[Tel]
        (SectionFile(prose(tel).let(_.text)), checked(path, tel))

// What a section's file says: its title, if it has one.
case class SectionFile(title: Optional[Text])
