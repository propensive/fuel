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

import scala.collection.immutable as sci

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Tone}

// A section's file: a TEL file in the directory of the section's definitive text, named on the
// `original` line of the configuration after the directory, as its `metadata`, which says what a reader is
// told of the section — so far its `title`, which the navigation shows in place of the
// section's name:
//
//     title  User guide
//
// Being in the tree, it is translated as the tree is: its translation is the file of the same
// path in a translation's directory, whose prose is a `Translation` of the definitive file's,
//
//     title
//         - User guide
//         + Guide de l'utilisateur
//
// made from the definitive file (`generate`) and held to it (`check`). The format is the
// schema `section` (`fuel/section.schema.tel`).
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

  private def headed(text: Text): Text = pyrocosm.Tels.headed(text)(0)
  private def added(text: Text): Int = pyrocosm.Tels.headed(text)(1)

  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the section's file has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

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
  private def checked(path: Text, text: Text, tel: Tel): List[Block] =
    import strategies.throwUnsafely

    val faults: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error =>
          val pointer: Text = prior.let(_.pointer.encode).or(t"/")
          accrual + (error, pointer, prior.lay(Span.empty)(_.span))

      . protect(Tel.Type.assign(tel, schema))

    faults.items.map: (error, pointer, span) =>
      val line: Int = if span.exists then span.startLine.lay(1)(_.n1) - added(text) else 1
      val location: Block.Location = Block.Location(path, line.max(1))
      Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(explain(error, pointer))))

  private def parsed(text: Text): Optional[Tel] =
    import strategies.throwUnsafely
    import parsing.trackPositions

    try headed(text).read[Tel] catch case error: Tel.Error => Unset

  // A run of prose as the file writes it: its text, the line (from 1) of its keyword, how
  // many lines of the file it is with its keyword, and whether it is on the keyword's line.
  case class Prose(text: Text, line: Int, extent: Int, inline: Boolean):
    // The line its text begins on.
    def site: Int = if inline then line else line + 1

  // The title as it is written, if there is one.
  def title(text: Text): Optional[Prose] =
    parsed(text).let: (tel: Tel) =>
      tel.fields(t"title").to[List].head.let: (node: Tel) =>
        val written: Text = node.atomTexts.to[List].filter(_ != t"").join(t" ")
        val lines: sci.Vector[String] = Translation.split(text)
        val index: Int = lines.indexWhere { line => line == "title" || line.startsWith("title ") }
        val brief: Boolean = index >= 0 && lines(index).trim.nn != "title"

        // Beneath its keyword, a title is every line until the next which is not indented.
        val beneath: Int =
          if brief || index < 0 then 0 else
            val rest: sci.Vector[String] = lines.drop(index + 1)
            val run: Int = rest.indexWhere { line => line.trim.nn != "" && !line.startsWith(" ") }
            val taken: sci.Vector[String] = if run < 0 then rest else rest.take(run)
            taken.reverse.dropWhile(_.trim.nn == "").size

        if written == t"" || index < 0 then Unset
        else Prose(written, index + 1, beneath + 1, brief)

  // A section's file read from its text, with what the schema finds wrong with it;
  // or the diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (SectionFile, List[Block]) | Block.Diagnostic =
    import strategies.throwUnsafely
    import parsing.trackPositions

    try
      val tel: Tel = headed(text).read[Tel]
      (SectionFile(title(text).let(_.text)), checked(path, text, tel))
    catch case error: Tel.Error =>
      val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))

      val location: Optional[Block.Location] = raw.location.let: (place: Block.Location) =>
        place.copy(line = place.line.let(_ - added(text)))

      raw.copy(location = location)

  // What a reader of a translation's language is told: what the translation reads as, and
  // what the definitive file says where it says nothing.
  def foreign(text: Text, definitive: SectionFile): SectionFile =
    val read: Optional[Text] = title(text).let: (prose: Prose) =>
      Translation.parse(prose.text).foreign.text.trim

    SectionFile(read.let { (text: Text) => if text == t"" then Unset else text }.or(definitive.title))

  // The translation of a section's file: the definitive file, its title a `Translation` —
  // that of the translation `old`, brought up to date, or one nobody has translated. `Unset`
  // if the file is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    if parsed(definitive).absent then Unset else
      val lines: sci.Vector[String] = Translation.split(definitive)

      title(definitive).lay(definitive): (prose: Prose) =>
        val translation: Translation = old.let(title(_)).let(_.text) match
          case before: Text => Translation.update(Translation.parse(before), prose.text)
          case _            => Translation.skeleton(prose.text)

        val marked: sci.Vector[String] = Translation.split(translation.serialize).map: line =>
          if line == "" then "" else s"    $line"

        val written: sci.Vector[String] =
          lines.take(prose.line - 1) ++ sci.Vector("title") ++ marked ++
            lines.drop(prose.line - 1 + prose.extent)

        Text(written.mkString("\n"))

  // What is wrong with the translation of a section's file, read from `text` at `path`, of
  // the definitive file `definitive`: what is wrong with it as one, and where its title
  // is not a translation of the definitive title, as `Translation` finds.
  def check(path: Text, text: Text, definitive: Text): List[Block] =
    read(path, text) match
      case diagnostic: Block.Diagnostic =>
        List(diagnostic)

      case (_, faults) =>
        val remedy: Text = t"`fuel translate --update` makes the translation again"

        val findings: List[Translation.Finding] =
          title(definitive).lay(List[Translation.Finding]()): (ours: Prose) =>
            title(text) match
              case theirs: Prose =>
                Translation.findings(theirs.text, ours.text).map(_.shifted(theirs.site - 1))

              case _ =>
                val message: Text =
                  t"This lacks the section's title, which is not translated: $remedy."

                List(Translation.Finding(Tone.Warning, 1, message))

        val orphaned: List[Translation.Finding] =
          if title(definitive).present then Nil
          else title(text).lay(List[Translation.Finding]()): (theirs: Prose) =>
            val message: Text = t"The text this translates has gone: $remedy."
            List(Translation.Finding(Tone.Warning, theirs.line, message))

        val diagnostics: List[Block.Diagnostic] =
          Translation.report(path, text, findings + orphaned)

        faults + diagnostics.map { (diagnostic: Block.Diagnostic) => diagnostic: Block }

  // How much of the translation is done.
  def progress(text: Text, definitive: Text): Translation.Progress =
    title(definitive).lay(Translation.Progress(0, 0, 0, 0, 0)): (ours: Prose) =>
      title(text) match
        case theirs: Prose => Translation.progress(Translation.parse(theirs.text), ours.text)
        case _             => Translation.Progress(0, 0, 0, 0, 1)

  // A section's file at `path` held to its translations, each a language and a path, as
  // `Translations.check` holds a document.
  def verify
    ( path:         Text,
      text:         Text,
      translations: List[(Text, Text)],
      read:         Text => Optional[Text],
      partial:      List[Text] = Nil )
  :   List[Block] =

    translations.bind: (tongue: Text, target: Text) =>
      val missing: List[Block] =
        if partial.has(tongue) then Nil else List(Translations.absent(path, tongue, target))

      read(target).lay(missing): (source: Text) =>
        check(target, source, text)

// What a section's file says: its title, if it has one.
case class SectionFile(title: Optional[Text])
