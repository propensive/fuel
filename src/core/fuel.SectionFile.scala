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

  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the section's file has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // The code of a section's file's problem, `↯/f.25.7.<variant>`: a fault the schema finds,
  // which carries the TEL error's own code (1); and a translation which lacks the title (2), or
  // has one the file it translates does not (3).
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

  // The file as TEL, with the positions of its compounds; `Unset` if it is not TEL.
  private def parsed(text: Text): Optional[Tel] =
    import parsing.trackPositions
    safely(text.read[Tel])

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

  // The title as it is written, if there is one.
  def title(text: Text): Optional[Prose] = parsed(text).let(prose(_))

  // A section's file read from its text, with what the schema finds wrong with it;
  // or the diagnostic which says the text is not TEL.
  def read(path: Text, text: Text): (SectionFile, List[Block]) | Block.Diagnostic =
    import parsing.trackPositions

    recover:
      case error: Tel.Error => Block.Diagnostic.of(error, path, text)

    . protect[(SectionFile, List[Block]) | Block.Diagnostic]:
        val tel: Tel = text.read[Tel]
        (SectionFile(prose(tel).let(_.text)), checked(path, tel))

  // What a reader of a translation's language is told: what the translation reads as, and
  // what the definitive file says where it says nothing.
  def foreign(text: Text, definitive: SectionFile): SectionFile =
    val read: Optional[Text] = title(text).let: (prose: Prose) =>
      Translation.parse(prose.text).foreign.text.trim

    SectionFile(read.let { (text: Text) => if text == t"" then Unset else text }.or(definitive.title))

  // The translation of a section's file: the definitive file as TEL reads it, its title a
  // `Translation` — that of the translation `old`, brought up to date, or one nobody has
  // translated — written as the title's source atom. `Unset` if the file is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    parsed(definitive).let: (tel: Tel) =>
      prose(tel).lay(definitive: Optional[Text]): (prose: Prose) =>
        val translation: Translation = old.let(title(_)).let(_.text) match
          case before: Text => Translation.update(Translation.parse(before), prose.text)
          case _            => Translation.skeleton(prose.text)

        val atoms: Array[Tel.Atom] = Array(Tel.Atom.Source(translation.serialize))

        val replacements: List[Tel.Mutation.Op] = titled(tel).lay(Nil): (compound: Tel.Compound) =>
          val written: Tel.Compound = compound.copy(atoms = atoms)
          List(Tel.Mutation.Op.Replace(Tel.Pointer.Empty / (t"title", 0), written))

        safely(Tel.Mutation(tel, replacements).show)

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

                List(Translation.Finding(Tone.Warning, 1, message, bolt(2)))

        val orphaned: List[Translation.Finding] =
          if title(definitive).present then Nil
          else title(text).lay(List[Translation.Finding]()): (theirs: Prose) =>
            val message: Text = t"The text this translates has gone: $remedy."
            List(Translation.Finding(Tone.Warning, theirs.line, message, bolt(3)))

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
