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

import java.nio.file as jnf

import scala.collection.immutable as sci

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Format as _, Compilation as _, Query as _, *}

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.standardRendering.given
import pyrocosm.{Block, Filing, TerminalRenderer, Tone, Tool}

object Mistranslated
extends Status(10, t"a translation is not consistent with the text it translates")

// `fuel translate`: the translations of a project's documents and overlays, without compiling
// anything. A translation is where the configuration puts it: the file of the same relative
// path in the directory a `translation` line, beneath a `document`, gives its language. The
// command writes each translation nobody has started — of every document and overlay, or of
// the files named, into every language configured, or the one given with `--language` — and
// says how far each translation is done and everything wrong with any; with `--update`, it
// brings each translation up to the text it translates, leaving the translator the `!` lines
// to work from.
object Translating:
  enum Kind:
    case Document, Overlay

  // A file which is translated from: its path, what it is, its text, and its translations, a
  // language and a path each.
  case class Source(path: Text, kind: Kind, text: Text, translations: List[(Text, Text)])

  private def read(path: Text): Optional[Text] =
    try Text(jnf.Files.readString(jnf.Path.of(path.s)).nn) catch case _: Exception => Unset

  // A file written, in a directory made for it if there is none.
  private def write(path: Text, text: Text): Boolean =
    try
      Optional(jnf.Path.of(path.s).nn.getParent).let(jnf.Files.createDirectories(_))
      jnf.Files.writeString(jnf.Path.of(path.s), text.s)
      true
    catch case _: Exception => false

  private def exists(path: Text): Boolean = jnf.Files.exists(jnf.Path.of(path.s))

  // Every overlay file beneath a directory, in order.
  def telFiles(directory: Text): List[Text] =
    safely(directory.as[Path on Linux]).lay(List[Text]()): (root: Path on Linux) =>
      if !Filing.directory(root) then Nil
      else List.from(Filing.beneath(root).map(_.encode).filter(_.ends(t".tel")).stdlib.sortBy(_.s))

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
      . bind(source(config, Kind.Overlay, _))

    documents + overlays

  // The languages which need not translate a source.
  private def lenient(config: Workspace.Config, source: Source): List[Text] =
    Translations.partial(Translations.within(source.path, config.translations))

  // What is wrong with a source's translations.
  def problems(source: Source, partial: List[Text]): List[Block] = source.kind match
    case Kind.Document =>
      Translations.check(source.path, source.text, source.translations, read, partial)

    case Kind.Overlay =>
      val reader: Text -> Optional[Text] = (path: Text) => read(path)

      OverlayTranslations.verify
        ( source.path, source.text, source.translations, reader, partial )

  // The files in the directories of translations which translate nothing: each a caution.
  private def strays(config: Workspace.Config, targets: List[Translations.Target]): List[Block] =
    val files: List[Text] = targets.map(_.directory).distinct.bind: (directory: Text) =>
      Compilation.sources(directory) + telFiles(directory)

    files.bind: (file: Text) =>
      Translations.original(file, targets).lay(List[Block]()): (tongue: Text, original: Text) =>
        if exists(original) then Nil else
          val message: Text =
            t"This would translate `${shown(config, original)}` into `$tongue`, but there is " +
              t"no such file."

          List(Translations.problem(Tone.Warning, file, message))

  private def progress(source: Source, text: Text): Translation.Progress = source.kind match
    case Kind.Document => Translation.progress(Translation.parse(text), source.text)
    case Kind.Overlay  => OverlayTranslations.progress(text, source.text)

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

  private def kind(path: Text): Optional[Kind] =
    if path.ends(t".md") then Kind.Document else if path.ends(t".tel") then Kind.Overlay else Unset

  // The sources the files name, or every source where none is named; and the files which name
  // nothing that can be translated, each with why.
  private def chosen(config: Workspace.Config, files: List[Text]): (List[Source], List[Text]) =
    val all: List[Source] = sources(config)

    if files.nil then (all, List[Text]()) else
      val found: List[(Text, Optional[Source])] = files.map: (path: Text) =>
        val translation: Boolean = Translations.held(path, config.translations)
        val unlisted: Optional[Source] = kind(path).let(source(config, _, path).head)
        (path, all.filter(_.path == path).head.or(if translation then Unset else unlisted))

      val reasons: List[Text] = found.filter(_(1).absent).map: (path: Text, _) =>
        if Translations.held(path, config.translations) then
          t"${shown(config, path)} is a translation: name the file it translates"
        else t"${shown(config, path)} could not be read as a document or an overlay"

      (found.bind { (_, source) => source.let(List(_)).or(List[Source]()) }, reasons)

  // A translation nobody has started, written for a translator to fill in. One which exists is
  // left as it is.
  private def supply(config: Workspace.Config, source: Source, target: Text)(using Stdio): Unit =
    if !exists(target) then
      val content: Optional[Text] = source.kind match
        case Kind.Document => Translation.skeleton(source.text).serialize
        case Kind.Overlay  => OverlayTranslations.generate(source.text)

      content.let: (content: Text) =>
        if write(target, content) then Out.println(t"fuel: wrote ${shown(config, target)}")
        else Out.println(t"fuel: ${shown(config, target)} could not be written")

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

    config.badTranslations.each { (reason: Text) => Out.println(t"fuel: $reason") }

    val targets: List[Translations.Target] = mirrors(config, tongue)

    if targets.nil then
      val example: Text = tongue.or(t"fr")

      val into: Text = tongue.lay(t"") { (tongue: Text) => t" into $tongue" }

      Out.println
        ( t"fuel: the configuration has no directory of translations$into: it names one as " +
            t"`translation $example doc/$example`, beneath the `document` it translates" )

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

      unread.each { (reason: Text) => Out.println(t"fuel: $reason") }

      all.each: (source: Source) =>
        // A language with a fallback need not translate everything: its translations are
        // written only when it is asked for by name.
        val partial: List[Text] = lenient(config, source)

        source.translations.each: (language: Text, target: Text) =>
          if tongue.present || !partial.has(language) then supply(config, source, target)

        if source.translations.nil && !files.nil then
          Out.println
            ( t"fuel: ${shown(config, source.path)} is beneath no `document` the configuration " +
                t"gives a translation" )

      val rows: List[Text] = all.bind: (source: Source) =>
        source.translations.bind: (_, target: Text) =>
          read(target).lay(List[Text]()): (text: Text) =>
            List(t"fuel: ${shown(config, target)}: ${summary(progress(source, text))}")

      val targets: List[Translations.Target] = mirrors(config, tongue)

      val found: List[Block] =
        all.bind { (source: Source) => problems(source, lenient(config, source)) } + (if files.nil then strays(config, targets) else Nil)

      if rows.nil && unread.nil then Out.println(t"fuel: there is nothing to translate")
      rows.each { (row: Text) => Out.println(row) }

      if !found.nil then
        Out.println(e"")
        TerminalRenderer().blocks(found, width).each: (line: Teletype) => Out.println(line)

      if !unread.nil then NoDocument else if !found.nil then Mistranslated else Exit.Ok

  // `fuel translate --update [--language <language>] [<file>...]`: each translation — or those
  // of the files named, or the translations named themselves, or those into one language —
  // brought up to the text it translates.
  def update(config: Workspace.Config, tongue: Optional[Text], files: List[Text])(using Stdio)
  :   Exit | Tool.UsageError.type =

    if unconfigured(config, tongue) then Tool.UsageError else
      val all: List[Source] = sources(config).map(only(_, tongue))

      val changed: List[Boolean] = all.bind: (source: Source) =>
        source.translations.bind: (_, target: Text) =>
          if !files.nil && !files.has(source.path) && !files.has(target) then Nil
          else read(target).lay(List[Boolean]()): (old: Text) =>
            val current: Optional[Text] = source.kind match
              case Kind.Document =>
                Translation.update(Translation.parse(old), source.text).serialize

              case Kind.Overlay =>
                OverlayTranslations.generate(source.text, old)

            current.lay(List[Boolean]()): (current: Text) =>
              if current == old then List(false) else
                if source.kind == Kind.Overlay then
                  OverlayTranslations.lost(source.text, old).each:
                    (run: OverlayTranslations.Prose) =>
                      Out.println
                        ( t"fuel: ${shown(config, target)}: the translation of ${run.title} is " +
                            t"left out, since the overlay no longer has it" )

                if write(target, current) then
                  Out.println
                    ( t"fuel: updated ${shown(config, target)}: " +
                        t"${summary(progress(source, current))}" )
                else Out.println(t"fuel: ${shown(config, target)} could not be written")

                List(true)

      if changed.nil then Out.println(t"fuel: there is no translation to update")
      else if !changed.has(true) then
        Out.println(t"fuel: every translation is of its text as it stands")

      Exit.Ok
