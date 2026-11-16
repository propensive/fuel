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

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Compilation as _, *}

import dysasymptotics.linearSize
import pyrocosm.standardRendering.given
import pyrocosm.{Block, Filing, TerminalRenderer, Tone, Tool}

object Mistranslated
extends Status(10, t"a translation is not consistent with the text it translates")

object Withheld
extends Status(11, t"a translation was not updated, since the update would have damaged it")

// `fuel translate`: the translations of a project's documents, overlays and glossaries,
// without compiling anything. A translation is where the configuration puts it: the file of
// the same relative path in the directory a `translation` line, beneath a `section`, gives
// its language. The command writes each translation nobody has started — of every document,
// overlay and glossary, or of the files named, into every language configured, or the one
// given with `--language` — and says how far each translation is done and everything wrong
// with any; with `--update`, it brings each translation up to the text it translates,
// leaving the translator the `!` lines to work from.
object Translating:
  // The code of what `fuel translate` reports, `↯/f.27.2.<variant>`: a file in a directory of
  // translations which translates nothing (1); a file named which is a translation (2), or
  // which is nothing that is translated (3); a translation which could not be written (4); a
  // configuration without a directory of translations (5); a file named which no section
  // translates (6); and an update which would damage the translation, and is withheld (7).
  private def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.client, FuelBolts.translating, variant)

  enum Kind:
    case Document, Overlay, Glossary, Section

  // A file which is translated from: its path, what it is, its text, and its translations, a
  // language and a path each.
  case class Source(path: Text, kind: Kind, text: Text, translations: List[(Text, Text)])

  private def read(path: Text): Optional[Text] = readText(path)

  // A file written, in a directory made for it if there is none.
  private def write(path: Text, text: Text): Boolean =
    safely(path.as[Path on Linux]).lay(false)(Filing.replace(_, text))

  // Every overlay file beneath a directory, in order.
  def telFiles(directory: Text): List[Text] =
    safely(directory.as[Path on Linux]).lay(List[Text]()): (root: Path on Linux) =>
      if !Filing.directory(root) then Nil else filesBeneath(root, t".tel")

  private def source(config: Workspace.Config, kind: Kind, path: Text): List[Source] =
    read(path).lay(List[Source]()): (text: Text) =>
      List(Source(path, kind, text, Translations.of(path, config.translations)))

  // Everything the configuration has which may be translated: no file in a directory of
  // translations is among them.
  def sources(config: Workspace.Config): List[Source] =
    val documents: List[Source] = config.documents.bind: (root: Text) =>
      Compilation.files(root, config.translations).bind(source(config, Kind.Document, _))

    val overlays: List[Source] = config.overlays.bind: (root: Text) =>
      telFiles(root).filter(!Translations.held(_, config.translations))
      . filter(!config.glossaries.has(_))
      . filter { (file: Text) => !config.sectionFiles.exists(_(1) == file) }
      . bind(source(config, Kind.Overlay, _))

    val described: List[Source] = config.sectionFiles.bind: (_, file: Text) =>
      source(config, Kind.Section, file)

    val glossaries: List[Source] =
      config.glossaries.filter(!Translations.held(_, config.translations))
      . bind(source(config, Kind.Glossary, _))

    documents + overlays + glossaries + described

  // The names of every term the glossaries define, which a translated term may be mistaken
  // for.
  private def names(config: Workspace.Config): List[Text] =
    config.glossaries.bind: (file: Text) =>
      read(file).lay(List[Text]())(GlossaryTranslations.entries(_).map(_.name))

  // The languages which need not translate a source.
  private def lenient(config: Workspace.Config, source: Source): List[Text] =
    Translations.partial(Translations.within(source.path, config.translations))

  // What is wrong with a source's translations.
  // What is wrong with a source's translations, each read by `reader`: from its file, unless
  // another reader is given.
  def problems
    ( source:  Source,
      partial: List[Text],
      names:   List[Text]                    = Nil,
      reader:  Optional[Text -> Optional[Text]] = Unset )
  :   List[Block] =

    val reading: Text -> Optional[Text] = reader.or((path: Text) => read(path))

    source.kind match
      case Kind.Document =>
        Translations.check(source.path, source.text, source.translations, reading(_), partial)

      case Kind.Overlay =>
        OverlayTranslations.verify
          ( source.path, source.text, source.translations, reading, partial )

      case Kind.Glossary =>
        GlossaryTranslations.verify
          ( source.path, source.text, source.translations, reading, partial, names )

      case Kind.Section =>
        SectionFile.verify(source.path, source.text, source.translations, reading(_), partial)

  // The translation `old` of a source, brought up to the text it translates; `Unset` where
  // that cannot be made, as for an overlay which is not TEL.
  def remake(source: Source, old: Text): Optional[Text] = source.kind match
    case Kind.Document => Translation.update(Translation.parse(old), source.text).serialize
    case Kind.Overlay  => OverlayTranslations.generate(source.text, old)
    case Kind.Glossary => GlossaryTranslations.generate(source.text, old)
    case Kind.Section  => SectionFile.generate(source.text, old)

  // The `+` lines of a translation, each as it reads without its indentation: what a
  // translator wrote, which an update must keep. A passage whose `+` lines are its `-` lines is
  // text shared by both languages, not a translation, and is not among them.
  private def written(text: Text): List[Text] =
    def marked(sigil: Text)(line: Text): Boolean = line == sigil || line.starts(t"$sigil ")

    def scan(lines: List[Text]): List[Text] =
      if lines.nil then Nil else
        val definitive: List[Text] = lines.keep(marked(t"-"))
        val after: List[Text] = lines.skip(definitive.size)
        val foreign: List[Text] = after.keep(marked(t"+"))

        if foreign.nil then scan(lines.skip(definitive.size.max(1)))
        else
          val shared: Boolean = foreign.map(_.skip(1)) == definitive.map(_.skip(1))
          (if shared then Nil else foreign) + scan(after.skip(foreign.size))

    scan(text.cut(t"\n").map(_.trim))

  // What of `left` is not in `right`, each element of `right` taking away one equal element of
  // `left`: a difference of multisets, which Soundness's collections do not yet have
  // (propensive/soundness#2498).
  private def without[element](left: List[element], right: List[element]): List[element] =
    right.fold(left): (rest: List[element], element: element) =>
      rest.where(_ == element).lay(rest): (index: Ordinal) =>
        rest.keep(index.n0) + rest.skip(index.n0 + 1)

  // The `+` lines which an update may leave out: those of the prose, and the entries, which the
  // text translated no longer has.
  private def forsaken(source: Source, old: Text): List[Text] = source.kind match
    case Kind.Overlay =>
      OverlayTranslations.lost(source.text, old).bind: (run: OverlayTranslations.Prose) =>
        written(run.text)

    case Kind.Glossary =>
      GlossaryTranslations.lost(source.text, old).bind: (entry: GlossaryTranslations.Entry) =>
        entry.definition.lay(List[Text]())(written(_))

    case _ =>
      Nil

  // What a diagnostic says, wherever it says it: a failure is the same failure at another line.
  private def gist(block: Block): Optional[(Tone, List[Block])] = block match
    case diagnostic: Block.Diagnostic => (diagnostic.tone, diagnostic.message)
    case _                            => Unset

  // Why the translation `current`, an update of `old` into `tongue` at `target`, must not be
  // written over it, if it must not: a failure `old` did not have; text a translator wrote,
  // left out though what it translates is still there; or an update which, made again, would
  // change it again.
  def hazards(source: Source, tongue: Text, target: Text, old: Text, current: Text): List[Text] =
    val single: Source = source.copy(translations = List((tongue, target)))

    def failures(text: Text): List[(Tone, List[Block])] =
      problems(single, Nil, Nil, (path: Text) => if path == target then text else read(path))
      . bind { (block: Block) => gist(block).lay(List[(Tone, List[Block])]())(List(_)) }
      . filter(_(0) == Tone.Failure)

    val before: List[(Tone, List[Block])] = failures(old)

    val introduced: Int =
      without(failures(current), before).size

    val broken: List[Text] =
      if introduced == 0 then Nil
      else List(t"it would have ${introduced.show} failures which the translation does not have")

    val kept: List[Text] = written(current) + forsaken(source, old)
    val dropped: Int = without(written(old), kept).size

    val lost: List[Text] =
      if dropped == 0 then Nil
      else List(t"it would lose ${dropped.show} lines of translation whose text is still there")

    val unsettled: List[Text] =
      if remake(source, current).lay(true)(_ == current) then Nil
      else List(t"updating it again would change it again")

    broken + lost + unsettled

  // The files in the directories of translations which translate nothing: each a caution.
  private def strays(config: Workspace.Config, targets: List[Translations.Target]): List[Block] =
    val files: List[Text] = targets.map(_.directory).distinct.bind: (directory: Text) =>
      Compilation.sources(directory) + telFiles(directory)

    files.bind: (file: Text) =>
      Translations.original(file, targets).lay(List[Block]()): (tongue: Text, original: Text) =>
        if onDisk(original) then Nil else
          val message: Text =
            t"This would translate `${shown(config, original)}` into `$tongue`, but there is " +
              t"no such file."

          List(Translations.problem(Tone.Warning, file, message, bolt(1)))

  private def progress(source: Source, text: Text): Translation.Progress = source.kind match
    case Kind.Document => Translation.progress(Translation.parse(text), source.text)
    case Kind.Overlay  => OverlayTranslations.progress(text, source.text)
    case Kind.Glossary => GlossaryTranslations.progress(text, source.text)
    case Kind.Section  => SectionFile.progress(text, source.text)

  private def shown(config: Workspace.Config, path: Text): Text =
    if path.starts(t"${config.root}/") then path.skip(config.root.length + 1) else path

  private def summary(progress: Translation.Progress): Text =
    val parts: List[Text] =
      List
        ( (progress.translated, t"translated"), (progress.untranslated, t"to translate"),
          (progress.pending, t"to revise"), (progress.orphaned, t"to delete"),
          (progress.stale, t"out of date") )
      . filter(_(0) > 0)
      . map { (count: Int, what: Text) => t"${count.show} $what" }

    if parts.nil then t"nothing to translate" else parts.join(t", ")

  private def kind(config: Workspace.Config, path: Text): Optional[Kind] =
    if path.ends(t".md") then Kind.Document
    else if config.glossaries.has(path) then Kind.Glossary
    else if config.sectionFiles.exists(_(1) == path) then Kind.Section
    else if path.ends(t".tel") then Kind.Overlay
    else Unset

  // The sources the files name, or every source where none is named; and the files which name
  // nothing that can be translated, each with why.
  private def chosen(config: Workspace.Config, files: List[Text])
  :   (List[Source], List[(Text, fulminate.Bolt)]) =

    val all: List[Source] = sources(config)

    if files.nil then (all, List[(Text, fulminate.Bolt)]()) else
      val found: List[(Text, Optional[Source])] = files.map: (path: Text) =>
        val translation: Boolean = Translations.held(path, config.translations)
        val unlisted: Optional[Source] = kind(config, path).let(source(config, _, path).head)
        (path, all.filter(_.path == path).head.or(if translation then Unset else unlisted))

      val reasons: List[(Text, fulminate.Bolt)] = found.filter(_(1).absent).map: (path: Text, _) =>
        if Translations.held(path, config.translations) then
          (t"${shown(config, path)} is a translation: name the file it translates", bolt(2))
        else
          val message: Text =
            t"${shown(config, path)} could not be read as a document, an overlay, a glossary " +
              t"or a section's file"

          (message, bolt(3))

      (found.bind { (_, source) => source.let(List(_)).or(List[Source]()) }, reasons)

  // A translation nobody has started, written for a translator to fill in. One which exists is
  // left as it is.
  private def supply(config: Workspace.Config, source: Source, target: Text)(using Stdio): Unit =
    if !onDisk(target) then
      val content: Optional[Text] = source.kind match
        case Kind.Document => Translation.skeleton(source.text).serialize
        case Kind.Overlay  => OverlayTranslations.generate(source.text)
        case Kind.Glossary => GlossaryTranslations.generate(source.text)
        case Kind.Section  => SectionFile.generate(source.text)

      content.let: (content: Text) =>
        if write(target, content) then Out.println(t"fuel: wrote ${shown(config, target)}")
        else Out.println(t"fuel: ${bolt(4).label} ${shown(config, target)} could not be written")

  // A source as one language alone sees it.
  private def only(source: Source, tongue: Optional[Text]): Source =
    tongue.lay(source): (tongue: Text) =>
      source.copy(translations = source.translations.filter(_(0) == tongue))

  // The directories of translations, or those of one language.
  private def mirrors(config: Workspace.Config, tongue: Optional[Text])
  :   List[Translations.Target] =

    config.translations.filter: (target: Translations.Target) =>
      tongue.lay(true)(_ == target.tongue)

  // What the configuration lacks for there to be anything to do, if it lacks it.
  private def unconfigured(config: Workspace.Config, tongue: Optional[Text])(using Stdio)
  :   Boolean =

    config.badTranslations.each: (reason: Text, code: fulminate.Bolt) =>
      Out.println(t"fuel: ${code.label} $reason")

    val targets: List[Translations.Target] = mirrors(config, tongue)

    if targets.nil then
      val example: Text = tongue.or(t"fr")

      val into: Text = tongue.lay(t"") { (tongue: Text) => t" into $tongue" }

      Out.println
        ( t"fuel: ${bolt(5).label} the configuration has no directory of translations$into: it " +
            t"names one as `translation $example $example-directory`, beneath the `section` it " +
            t"translates" )

    targets.nil

  // `fuel translate [--language <language>] [<file>...]`: the translations the files lack —
  // every document's and overlay's, where none is named — written for each language
  // configured, or for the one given; then each translation, how far it is done, and
  // everything wrong with any.
  def run(config: Workspace.Config, tongue: Optional[Text], files: List[Text], width: Int)
    ( using Stdio )
  :   Exit | Tool.UsageError.type | NoDocument.type | Mistranslated.type =

    if unconfigured(config, tongue) then Tool.UsageError else
      val (selected, unread) = chosen(config, files)
      val all: List[Source] = selected.map(only(_, tongue))

      unread.each: (reason: Text, code: fulminate.Bolt) =>
        Out.println(t"fuel: ${code.label} $reason")

      all.each: (source: Source) =>
        // A language with a fallback need not translate everything: its translations are
        // written only when it is asked for by name.
        val partial: List[Text] = lenient(config, source)

        source.translations.each: (language: Text, target: Text) =>
          if tongue.present || !partial.has(language) then supply(config, source, target)

        if source.translations.nil && !files.nil then
          Out.println
            ( t"fuel: ${bolt(6).label} ${shown(config, source.path)} is in no `section` the " +
                t"configuration gives a translation" )

      val rows: List[Text] = all.bind: (source: Source) =>
        source.translations.bind: (_, target: Text) =>
          read(target).lay(List[Text]()): (text: Text) =>
            List(t"fuel: ${shown(config, target)}: ${summary(progress(source, text))}")

      val targets: List[Translations.Target] = mirrors(config, tongue)
      val defined: List[Text] = names(config)

      val found: List[Block] =
        all.bind { (source: Source) => problems(source, lenient(config, source), defined) } +
          (if files.nil then strays(config, targets) else Nil)

      if rows.nil && unread.nil then Out.println(t"fuel: there is nothing to translate")
      rows.each { (row: Text) => Out.println(row) }

      if !found.nil then
        Out.println(e"")
        TerminalRenderer().blocks(found, width).each: (line: Teletype) => Out.println(line)

      if !unread.nil then NoDocument else if !found.nil then Mistranslated else Exit.Ok

  // `fuel translate --update [--dry-run] [--language <language>] [<file>...]`: each translation
  // — or those of the files named, or the translations named themselves, or those into one
  // language — brought up to the text it translates. An update is written only if it is safe
  // (`hazards`); with `--dry-run`, nothing is written, and what would be is said.
  def update
    ( config: Workspace.Config, tongue: Optional[Text], files: List[Text], dry: Boolean = false )
    ( using Stdio )
  :   Exit | Tool.UsageError.type | Withheld.type =

    if unconfigured(config, tongue) then Tool.UsageError else
      val all: List[Source] = sources(config).map(only(_, tongue))
      val verb: Text = if dry then t"would update" else t"updated"

      val outcomes: List[Outcome] = all.bind: (source: Source) =>
        source.translations.bind: (language: Text, target: Text) =>
          if !files.nil && !files.has(source.path) && !files.has(target) then Nil
          else read(target).lay(List[Outcome]()): (old: Text) =>
            remake(source, old).lay(List[Outcome]()): (current: Text) =>
              if current == old then List(Outcome.Unchanged) else
                val reasons: List[Text] = hazards(source, language, target, old, current)

                if !reasons.nil then
                  Out.println
                    ( t"fuel: ${bolt(7).label} ${shown(config, target)} is not updated, since " +
                        t"${reasons.join(t"; ")}: this is a fault in fuel, so please report it" )

                  List(Outcome.Refused)
                else
                  if source.kind == Kind.Overlay then
                    OverlayTranslations.lost(source.text, old).each:
                      (run: OverlayTranslations.Prose) =>
                        Out.println
                          ( t"fuel: ${shown(config, target)}: the translation of ${run.title} " +
                              t"is left out, since the overlay no longer has it" )

                  if source.kind == Kind.Glossary then
                    GlossaryTranslations.lost(source.text, old).each:
                      (entry: GlossaryTranslations.Entry) =>
                        Out.println
                          ( t"fuel: ${shown(config, target)}: the translation of " +
                              t"`${entry.name}` is left out, since the glossary no longer has " +
                              t"the term" )

                  val before: Translation.Progress = progress(source, old)
                  val after: Translation.Progress = progress(source, current)

                  if dry || write(target, current) then
                    Out.println
                      ( t"fuel: $verb ${shown(config, target)}: " +
                          t"${changes(before, after)}; then ${summary(after)}" )

                    List(Outcome.Updated)
                  else
                    val label: Text = bolt(4).label
                    Out.println(t"fuel: $label ${shown(config, target)} could not be written")
                    List(Outcome.Refused)

      if outcomes.nil then Out.println(t"fuel: there is no translation to update")
      else if !outcomes.exists(_ != Outcome.Unchanged) then
        Out.println(t"fuel: every translation is of its text as it stands")

      if outcomes.has(Outcome.Refused) then Withheld else Exit.Ok

  enum Outcome:
    case Unchanged, Updated, Refused

  // What an update does to how far a translation is done.
  private def changes(before: Translation.Progress, after: Translation.Progress): Text =
    val revise: Int = after.pending - before.pending
    val fresh: Int = after.untranslated - before.untranslated
    val gone: Int = after.orphaned - before.orphaned

    val parts: List[Text] =
      List
        ( (revise, t"newly to revise"), (fresh, t"newly to translate"),
          (gone, t"newly to delete") )
      . filter(_(0) > 0)
      . map { (count: Int, what: Text) => t"${count.show} $what" }

    if parts.nil then t"no passage changes state" else parts.join(t", ")
