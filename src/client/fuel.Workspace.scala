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
      // The translations of each section's definitive directory; what is wrong with how the
      // sections are configured; and each such directory with the language it is written in.
      translations:    List[Translations.Target] = Nil,
      badTranslations: List[Text]                = Nil,
      languages:       List[(Text, Text)]        = Nil,
      // Each section, a name and the directory of its definitive text.
      sections:        List[(Text, Text)]        = Nil,
      // Each section's file, by the directory of its definitive text.
      sectionFiles:    List[(Text, Text)]        = Nil )

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

    // A `section` is read from the configuration whole, as a `sync` line is, since it holds
    // more than a `Setting` reads: its name and its directory, and beneath it the `definitive`
    // text — the language it is written in, its directory and, in that directory, the file
    // which says what a reader is told of the section — and a `translation` for each language
    // it is translated into, a language and a directory too, perhaps with a `fallback`
    // language. Beneath the `definitive` are what else is found in its directory, and in each
    // translation's at the same path: each `overlay` directory and each `glossary`. Every
    // path is within its parent's: a `definitive` and a `translation` in the section's
    // directory, the others in the definitive's. What is wrong with a section is kept, to be
    // reported, and a section which cannot be read is left out.
    //
    //     section guide doc
    //       definitive en-US en section.tel
    //         overlay api
    //         glossary glossary.tel
    //       translation fr fr
    //       translation fr-CA fr-CA
    //         fallback fr
    def words(tel: Tel): List[Text] = tel.atomTexts.to[List].filter(_ != t"")

    def lines(tel: Tel, keyword: Text): List[List[Text]] =
      tel.fields(keyword).to[List].map(words(_))

    def within(directory: Text, entry: Text): Text =
      if entry.starts(t"/") then entry
      else if entry == t"." || entry == t"./" then directory
      else t"$directory/$entry"

    case class Group
      ( name:         Text,
        directory:    Text,
        language:     Text,
        file:         Optional[Text],
        overlays:     List[Text],
        glossaries:   List[Text],
        translations: List[Translations.Target] )

    // Whether a name can be a section's: it is one segment of a page's address.
    def nameable(name: Text): Boolean =
      name != t"" && !name.s.contains("/") && !name.s.contains("#")

    val form: Text = t"`section guide doc`, with `definitive en-US en` beneath it"

    // A section, if it can be read, and what is wrong with it.
    def group(tel: Tel): (Optional[Group], List[Text]) =
      val atoms: scala.collection.immutable.Vector[Text] = words(tel).stdlib.toVector
      val written: Text = words(tel).join(t" ")

      val misnamed: List[Text] =
        if atoms.length != 2 then
          List
            ( t"`section $written` is not a section's name and its directory, as " +
                t"`section guide doc`" )
        else if !nameable(atoms(0)) then
          List
            ( t"`section $written` names the section `${atoms(0)}`, and a name is one segment " +
                t"of a page's address, so it cannot hold `/` or `#`" )
        else Nil

      // What belongs beneath the `definitive`, or is no longer said.
      val misplaced: List[Text] =
        List(t"overlay", t"glossary").bind: (keyword: Text) =>
          lines(tel, keyword).map: (atoms: List[Text]) =>
            t"`$keyword ${atoms.join(t" ")}`, of `section $written`, belongs beneath the " +
              t"section's `definitive`, whose directory it is in"

      val outdated: List[Text] = lines(tel, t"language").map: (atoms: List[Text]) =>
        t"`language ${atoms.join(t" ")}`, of `section $written`, is said by the section's " +
          t"`definitive`, as `definitive ${atoms.join(t" ")} en`"

      val originals: List[Tel] = tel.fields(t"definitive").to[List]

      // The language, the directory, and the section's file if the line names one.
      val original: Optional[(Text, Text, Optional[Text], Tel)] =
        if originals.stdlib.size != 1 then Unset else originals.head.let: (node: Tel) =>
          val atoms: scala.collection.immutable.Vector[Text] = words(node).stdlib.toVector

          if atoms.length < 2 || atoms.length > 3 || !Tongue.valid(atoms(0)) then Unset
          else (atoms(0), atoms(1), if atoms.length == 3 then atoms(2) else Unset, node)

      val baseless: List[Text] =
        if original.present then Nil
        else if originals.nil then
          List
            ( t"`section $written` does not say where its definitive text is, and in which " +
                t"language, as `definitive en-US en` beneath it" )
        else if originals.stdlib.size > 1 then
          List(t"`section $written` has more than one `definitive`")
        else
          List
            ( t"`definitive ${originals.head.let(words(_).join(t" ")).or(t"")}`, of `section " +
                t"$written`, is not a language and the directory of the text written in it, " +
                t"perhaps with the section's file in that directory, as `definitive en-US en " +
                t"section.tel`" )

      if atoms.length != 2 || !nameable(atoms(0)) then (Unset, misnamed + misplaced + outdated)
      else original.lay((Unset: Optional[Group], baseless + misplaced + outdated)):
        (language: Text, entry: Text, file: Optional[Text], node: Tel) =>
          val home: Text = project.resolve(atoms(1)).encode
          val directory: Text = within(home, entry)
          val nested: List[Text] = lines(node, t"overlay").bind(_.head.let(List(_)).or(Nil))
          val defined: List[Text] = lines(node, t"glossary").bind(_.head.let(List(_)).or(Nil))

          // The section's file is named on the `definitive` line, not beneath it.
          val former: List[Text] = lines(node, t"details").map: (atoms: List[Text]) =>
            t"`details ${atoms.join(t" ")}`, of `section $written`, is written on the " +
              t"`definitive` line, after its directory, as `definitive $language $entry " +
              t"${atoms.join(t" ")}`"

          val read: List[(List[Text], List[List[Text]])] =
            tel.fields(t"translation").to[List].map: line =>
              (words(line), lines(line, t"fallback"))

          def target(atoms: List[Text], fallbacks: List[List[Text]])
          :   Optional[Translations.Target] =

            val all: scala.collection.immutable.Vector[Text] = atoms.stdlib.toVector
            val fallback: Optional[Text] = fallbacks.head.let(_.head)

            val sound: Boolean =
              fallbacks.stdlib.size <= 1 && fallbacks.all(_.stdlib.size == 1) &&
                fallback.lay(true)(Tongue.valid(_))

            if all.length != 2 || !Tongue.valid(all(0)) || !sound then Unset
            else Translations.Target(all(0), directory, within(home, all(1)), fallback)

          val targets: List[Translations.Target] =
            read.bind { (atoms, fallbacks) => target(atoms, fallbacks).let(List(_)).or(Nil) }

          // A fallback is the section's own language, or another it is translated into.
          def grounded(target: Translations.Target): Boolean =
            target.fallback.lay(true): (fallback: Text) =>
              fallback != target.tongue &&
                (language == fallback || targets.exists(_.tongue == fallback))

          val malformed: List[Text] =
            read.filter { (atoms, fallbacks) => target(atoms, fallbacks).absent }.map: (atoms, _) =>
              t"`translation ${atoms.join(t" ")}`, of `section $written`, is not a language and " +
                t"the directory of its translations, within the section's, perhaps with a " +
                t"`fallback` language, as `translation fr fr`"

          val groundless: List[Text] = targets.filter(!grounded(_)).map: target =>
            t"`translation ${target.tongue}`, of `section $written`, falls back to " +
              t"`${target.fallback.or(t"")}`, which is neither the section's definitive " +
              t"language nor another it is translated into"

          val own: List[Text] = targets.filter(_.tongue == language).map: target =>
            t"`translation ${target.tongue}`, of `section $written`, is into the language the " +
              t"section is written in"

          val same: List[Text] = targets.filter(_.directory == directory).map: target =>
            t"`translation ${target.tongue}`, of `section $written`, is in the directory of " +
              t"the text it translates"

          val kept: List[Translations.Target] =
            targets.filter { target => target.tongue != language && target.directory != directory }
            . map { target => if grounded(target) then target else target.copy(fallback = Unset) }

          ( Group
              ( atoms(0), directory, language, file.let(within(directory, _)),
                nested.map(within(directory, _)),
                defined.map(within(directory, _)), kept ),
            misplaced + outdated + former + malformed + groundless + own + same )

    val read: List[(Optional[Group], List[Text])] =
      project.config.lay(List[(Optional[Group], List[Text])]()): (tel: Tel) =>
        tel.fields(t"section").to[List].map(group(_))

    val groups: List[Group] = read.bind(_(0).let(List(_)).or(Nil))

    // What is of a `section`, at the top level, is of nothing; and a `document` in the
    // configuration is what a `section` was.
    val loose: List[Text] = project.config.lay(Nil: List[Text]): (tel: Tel) =>
      val stray: List[Text] = List(t"translation", t"definitive").bind: keyword =>
        lines(tel, keyword).map: (atoms: List[Text]) =>
          t"`$keyword ${atoms.join(t" ")}` is not beneath the `section` it is of"

      val former: List[Text] = lines(tel, t"document").map: (atoms: List[Text]) =>
        t"`document ${atoms.join(t" ")}` is no longer how a directory of documents is " +
          t"configured: it is a section, as $form"

      stray + former

    // The sections: each `section` of the configuration under its name. A directory given
    // another way — the `--document` flag, a variable — has nowhere to say more, and is the
    // section named after it, read in place of the configuration's.
    val stated: List[Text] = entries(ui.Document())

    val configured: Boolean =
      stated == project.atoms(t"document").map(project.resolve(_).encode)

    def after(root: Text): Text = fuel.Compilation.name(root)

    val sections: List[(Text, Text)] =
      if !configured then stated.map(root => (after(root), root))
      else groups.map(group => (group.name, group.directory))

    // Two sections of one name would have one address.
    val shared: List[Text] =
      sections.map(_(0)).distinct.filter(name => sections.filter(_(0) == name).stdlib.size > 1)
      . map: (name: Text) =>
          t"more than one `section` is named `$name`: each has a name of its own"

    val translations: List[Translations.Target] = groups.bind(_.translations)
    val badTranslations: List[Text] = read.bind(_(1)) + loose + shared

    val languages: List[(Text, Text)] = groups.map(group => (group.directory, group.language))

    val sectionFiles: List[(Text, Text)] = groups.bind: (group: Group) =>
      group.file.lay(List[(Text, Text)]()) { (file: Text) => List((group.directory, file)) }

    val overlays: List[Text] = (entries(ui.Overlay()) + groups.bind(_.overlays)).distinct

    Config
      ( root,
        entries(ui.Classpath()),
        sections.map(_(1)),
        options,
        ui.Session().or(false),
        fixtures,
        entries(ui.Source()),
        overlays,
        imports,
        output,
        (entries(ui.Glossary()) + groups.bind(_.glossaries)).distinct,
        ui.CheckLinks().or(false),
        sync,
        badSync,
        entries(ui.Schema()),
        translations,
        badTranslations,
        languages,
        sections,
        sectionFiles )
