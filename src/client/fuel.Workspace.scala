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

import soundness.*

import pyrocosm.Tool
import pyrocosm.decodables.boolean

// The project as the settings describe it: its root (the parent of the `.pyrocosm` holding
// `fuel/config.tel`, or the invocation's directory where there is none), the classpath — the
// dependencies the project's sources, and the embedded code, are compiled against — the
// directories of documents, of the project's own sources (whose API is documented) and of the
// TEL overlays which document it, and the imports a document's references resolve through.
// Relative entries resolve against the root, as flame's do.
object Workspace:
  case class Config
    ( root:      Text,
      classpath: List[Text],
      documents: List[Text],
      options:   List[Text],
      session:   Boolean        = false,
      fixtures:  Optional[Text] = Unset,
      sources:   List[Text]     = Nil,
      overlays:  List[Text]     = Nil,
      imports:   List[Text]     = Nil,
      output:    Optional[Text] = Unset,
      glossaries: List[Text]    = Nil,
      checkLinks: Boolean       = false,
      sync:       List[Sync.Target] = Nil,
      badSync:    List[Text]        = Nil,
      schemas:    List[Text]        = Nil,
      // The translations of each `document` directory; what is wrong with how they are
      // configured; and each such directory with the language it is written in.
      translations:    List[Translations.Target] = Nil,
      badTranslations: List[Text]                = Nil,
      languages:       List[(Text, Text)]        = Nil )

  def load(directory: Text)(using Cli, Interpreter, Configurator): Config =
    // The project the directory is in, as `Tool` finds it: its root is what every path in the
    // configuration resolves against.
    val project: Tool.Project = Fuel.project(directory)
    val root: Text = project.root.encode

    def entries(setting: Optional[Text]): List[Text] = project.entries(setting).map(_.encode)

    val options: List[Text] =
      ui.ScalacOption().lay(Nil: List[Text]): (text: Text) => text.cut(t":").filter(_ != t"")

    val fixtures: Optional[Text] = ui.Fixtures().let(project.resolve(_).encode)

    val imports: List[Text] =
      ui.Imports().lay(Nil: List[Text]): (text: Text) => text.cut(t":").filter(_ != t"")

    val output: Optional[Text] = ui.Output().let(project.resolve(_).encode)

    // A `sync` line names a target whole — `sync github owner/repo` — which a `Setting`, reading
    // a field's first atom alone, cannot; so it is read from the configuration as `Tool` reads
    // `serve` and `port`. A line which names no target is kept, to be reported.
    val syncLines: List[List[Text]] = project.config.lay(Nil: List[List[Text]]): (tel: Tel) =>
      tel.fields(t"sync").to[List].map(_.atomTexts.to[List].filter(_ != t""))

    val sync: List[Sync.Target] = syncLines.bind { words => Sync.Target.parse(words).let(List(_)).or(Nil) }
    val badSync: List[Text] = syncLines.filter(Sync.Target.parse(_).absent).map(_.join(t" "))

    // A `document` may hold more than its directory, which a `Setting` does not read: the
    // `language` it is written in, each `overlay` directory inside it, and a `translation`
    // for each language it is translated into — a language and the directory which mirrors
    // the document's, perhaps with a `fallback` language. So each is read from the
    // configuration whole, as a `sync` line is; what is wrong with one is kept, to be reported.
    def words(tel: Tel): List[Text] = tel.atomTexts.to[List].filter(_ != t"")

    def lines(tel: Tel, keyword: Text): List[List[Text]] =
      tel.fields(keyword).to[List].map(words(_))

    def within(directory: Text, entry: Text): Text =
      if entry.starts(t"/") then entry else t"$directory/$entry"

    case class Group
      ( directory:    Text,
        language:     Optional[Text],
        overlays:     List[Text],
        translations: List[Translations.Target],
        faults:       List[Text] )

    def group(tel: Tel): List[Group] = words(tel).head.lay(List[Group]()): (entry: Text) =>
      val directory: Text = project.resolve(entry).encode
      val languages: List[List[Text]] = lines(tel, t"language")

      val language: Optional[Text] =
        languages.head.let(_.head).let: (tongue: Text) =>
          if Tongue.valid(tongue) then tongue else Unset

      val spoken: List[Text] =
        if languages.nil || (languages.stdlib.size == 1 && language.present) then Nil else
          List
            ( t"`document $entry` has a `language` which is not one language, as `en` or " +
                t"`en-US`" )

      val nested: List[Text] = lines(tel, t"overlay").bind(_.head.let(List(_)).or(Nil))

      val read: List[(List[Text], List[List[Text]])] =
        tel.fields(t"translation").to[List].map { line => (words(line), lines(line, t"fallback")) }

      def target(atoms: List[Text], fallbacks: List[List[Text]]): Optional[Translations.Target] =
        val all: scala.collection.immutable.Vector[Text] = atoms.stdlib.toVector
        val fallback: Optional[Text] = fallbacks.head.let(_.head)

        val sound: Boolean =
          fallbacks.stdlib.size <= 1 && fallbacks.all(_.stdlib.size == 1) &&
            fallback.lay(true)(Tongue.valid(_))

        if all.length != 2 || !Tongue.valid(all(0)) || !sound then Unset
        else Translations.Target(all(0), directory, project.resolve(all(1)).encode, fallback)

      val targets: List[Translations.Target] =
        read.bind { (atoms, fallbacks) => target(atoms, fallbacks).let(List(_)).or(Nil) }

      // A fallback is the document's own language, or another it is translated into.
      def grounded(target: Translations.Target): Boolean =
        target.fallback.lay(true): (fallback: Text) =>
          fallback != target.tongue &&
            (language.lay(false)(_ == fallback) || targets.exists(_.tongue == fallback))

      val malformed: List[Text] =
        read.filter { (atoms, fallbacks) => target(atoms, fallbacks).absent }.map: (atoms, _) =>
          t"`translation ${atoms.join(t" ")}`, of `document $entry`, is not a language and the " +
            t"directory of its translations, perhaps with a `fallback` language, as " +
            t"`translation fr doc/fr`"

      val groundless: List[Text] = targets.filter(!grounded(_)).map: target =>
        t"`translation ${target.tongue}`, of `document $entry`, falls back to " +
          t"`${target.fallback.or(t"")}`, which is neither the document's `language` nor another " +
          t"it is translated into"

      val own: List[Text] =
        targets.filter { target => language.lay(false)(_ == target.tongue) }.map: target =>
          t"`translation ${target.tongue}`, of `document $entry`, is into the language the " +
            t"document is written in"

      val unspoken: List[Text] =
        if read.nil || !languages.nil then Nil else
          List
            ( t"`document $entry` is translated, and does not say its own `language`, as " +
                t"`language en`" )

      val kept: List[Translations.Target] =
        targets.filter { target => !language.lay(false)(_ == target.tongue) }.map:
          (target: Translations.Target) =>
            if grounded(target) then target else target.copy(fallback = Unset)

      List
        ( Group
            ( directory, language, nested.map(within(directory, _)), kept,
              spoken + malformed + groundless + own + unspoken ) )

    val groups: List[Group] =
      project.config.lay(List[Group]()): (tel: Tel) =>
        tel.fields(t"document").to[List].bind(group(_))

    // A `translation` is of a `document`: one at the top level is of nothing.
    val loose: List[Text] = project.config.lay(Nil: List[Text]): (tel: Tel) =>
      lines(tel, t"translation").map: (atoms: List[Text]) =>
        t"`translation ${atoms.join(t" ")}` is not beneath the `document` it translates"

    val translations: List[Translations.Target] = groups.bind(_.translations)
    val badTranslations: List[Text] = groups.bind(_.faults) + loose

    val languages: List[(Text, Text)] = groups.bind: (group: Group) =>
      group.language.lay(List[(Text, Text)]()) { (tongue: Text) => List((group.directory, tongue)) }

    val overlays: List[Text] = (entries(ui.Overlay()) + groups.bind(_.overlays)).distinct

    Config
      ( root,
        entries(ui.Classpath()),
        entries(ui.Document()),
        options,
        ui.Session().or(false),
        fixtures,
        entries(ui.Source()),
        overlays,
        imports,
        output,
        entries(ui.Glossary()),
        ui.CheckLinks().or(false),
        sync,
        badSync,
        entries(ui.Schema()),
        translations,
        badTranslations,
        languages )
