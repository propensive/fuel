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
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.Tool
import pyrocosm.decodables.boolean

// The project as the settings describe it: its root (the parent of the `.pyrocosm` holding
// `fuel/config.tel`, or the invocation's directory where there is none), the classpath — the
// dependencies the project's sources, and the embedded code, are compiled against — the
// directories of documents, of the project's own sources (whose API is documented) and of the
// TEL overlays which document it, and the imports a document's references resolve through.
// Relative entries resolve against the root, as flame's do.
object Workspace:
  // A configuration's sections, as the schema has them: what `section`, `original` and
  // `translation` say, each member a keyword which may be written as a child or, in this
  // order, on its parent's line.
  case class Translation(language: Text, path: Text, fallback: Optional[Text] = Unset)

  case class Original
    ( language: Text,
      path:     Text,
      metadata: Optional[Text] = Unset,
      overlay:  List[Text]     = Nil,
      glossary: List[Text]     = Nil )

  case class Section
    ( name:        Text,
      path:        Text,
      original:    Original,
      translation: List[Translation] = Nil )

  case class Sections(section: List[Section] = Nil)

  // The schema of the configuration, a TEL schema document among the resources: read through
  // the thread-context classloader, since under Burdock the system one sees only the
  // launcher's own jar.
  val schemaResource: Text = t"fuel/config.schema.tel"

  def schemaSource: Text =
    Classloader.threadContext.resource(schemaResource).let(_.utf8).or:
      panic(m"the resource $schemaResource is missing")

  lazy val schema: stratiform.Tels =
    import strategies.throwUnsafely
    stratiform.Tels.Validation.validate(schemaSource.read[Tel].as[stratiform.Tels])

  private case class Faults(items: List[(Tel.Error, Text)] = Nil)
  extends Error(m"the configuration has ${items.stdlib.size} faults"):
    def +(error: Tel.Error, pointer: Text): Faults = Faults(items + List((error, pointer)))

  // Every fault the schema finds in a configuration, with the path of what it concerns.
  def faults(tel: Tel): List[(Tel.Error, Text)] =
    import strategies.throwUnsafely

    val found: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error => accrual + (error, prior.let(_.pointer.encode).or(t"/"))

      . protect(Tel.Type.assign(tel, schema))

    found.items

  // What a fault the schema finds in a section means, by the path of what it concerns:
  // `section`, the section's index, and the keywords and indices beneath it.
  private def explain(tel: Tel, error: Tel.Error, path: List[Text]): Text =
    import Tel.Error.Reason

    val keywords: List[Text] = path.filter { segment => safely(segment.as[Int]).absent }
    val last: Text = keywords.reverse.head.or(t"")
    val parent: Text = keywords.reverse.skip(1).head.or(t"")
    val index: Int = path.skip(1).head.let { text => safely(text.as[Int]) }.or(0)

    val named: Text =
      tel.fields(t"section").to[List].stdlib.lift(index).map: (node: Tel) =>
        t"`section ${node.atomTexts.to[List].filter(_ != t"").join(t" ")}`"

      . getOrElse(t"a `section`")

    error.reason match
      case Reason.UnknownKeyword =>
        if parent == t"section" && (last == t"overlay" || last == t"glossary" || last == t"metadata")
        then t"`$last`, of $named, belongs beneath the section's `original`, whose directory it is in"
        else if last == t"language" && parent == t"section" then
          t"`language`, of $named, is said by the section's `original`, as `original en-US en`"
        else if last == t"definitive" then
          t"`definitive`, of $named, is now written `original`, as `original en-US en`"
        else if last == t"details" then
          t"`details`, of $named, is now the `metadata` of the section's `original`, written " +
            t"after its directory, as `original en-US en section.tel`"
        else t"`$last` is not something a `$parent` says, in $named"

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        if last == t"section" || parent == t"" then
          t"$named is not a section's name and its directory, as `section guide doc`"
        else if last == t"original" then
          t"the `original` of $named is not a language and a directory, perhaps with the " +
            t"section's file in it, as `original en-US en section.tel`"
        else if last == t"translation" then
          t"a `translation` of $named is not a language and a directory within the section's, " +
            t"perhaps with a fallback language, as `translation fr fr`"
        else t"a `$last` of $named has more values than it takes"

      case Reason.RequiredMemberAbsent =>
        if last == t"original" then
          t"$named does not say where its original text is, and in which language, as " +
            t"`original en-US en` beneath it"
        else if parent == t"section" then
          t"$named is not a section's name and its directory, as `section guide doc`"
        else if parent == t"original" then
          t"the `original` of $named is not a language and a directory, as `original en-US en`"
        else if parent == t"translation" then
          t"a `translation` of $named is not a language and a directory within the " +
            t"section's, as `translation fr fr`"
        else t"a `$parent` of $named needs a `$last`"

      case Reason.NonRepeatableTooMany =>
        t"$named says `$last` more than once"

      case _ =>
        t"${error.message.text}, in $named"

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
      // The translations of each section's original directory; what is wrong with how the
      // sections are configured; and each such directory with the language it is written in.
      translations:    List[Translations.Target] = Nil,
      badTranslations: List[Text]                = Nil,
      languages:       List[(Text, Text)]        = Nil,
      // Each section, a name and the directory of its original text.
      sections:        List[(Text, Text)]        = Nil,
      // Each section's file, by the directory of its original text.
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

    // A `section` is read from the configuration whole, to the schema: its `name` and its
    // `path`, and beneath it the `original` text — its `language`, its `path` and its
    // `metadata`, the file which says what a reader is told of the section — and a
    // `translation` for each language it is translated into, a `language` and a `path` too,
    // perhaps with a `fallback` language. Beneath the `original` are what else is found in
    // its directory, and in each translation's at the same path: each `overlay` directory and
    // each `glossary`. Every path is within its parent's: an `original` and a `translation`
    // in the section's directory, the others in the original's. Each of these is a member
    // with a keyword, which may be written as a child, or in order on its parent's line:
    //
    //     section guide doc
    //       original en-US en section.tel
    //         overlay api
    //         glossary glossary.tel
    //       translation fr fr
    //       translation fr-CA fr-CA
    //         fallback fr
    //
    // What is wrong with the sections is kept, to be reported; where the schema finds a
    // fault in them, none is read.
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

    val form: Text = t"`section guide doc`, with `original en-US en` beneath it"

    // The faults the schema finds in the sections, each as a sentence.
    val unschematic: List[Text] = project.config.lay(Nil: List[Text]): (tel: Tel) =>
      faults(tel).bind: (error: Tel.Error, pointer: Text) =>
        val path: List[Text] = pointer.cut(t"/").filter(_ != t"")
        if path.head.or(t"") != t"section" then Nil else List(explain(tel, error, path))

    // A section as it is written, made the directories and files it names; and what is wrong
    // with it which the schema cannot say.
    def group(section: Section): (Optional[Group], List[Text]) =
      val written: Text = t"${section.name} ${section.path}"
      val original: Original = section.original
      val language: Text = original.language
      val home: Text = project.resolve(section.path).encode
      val directory: Text = within(home, original.path)

      val misnamed: List[Text] =
        if nameable(section.name) then Nil else
          List
            ( t"`section $written` names the section `${section.name}`, and a name is one " +
                t"segment of a page's address, so it cannot hold `/` or `#`" )

      val unspoken: List[Text] =
        if Tongue.valid(language) then Nil else
          List
            ( t"`original $language ${original.path}`, of `section $written`, is in `$language`, " +
                t"which is not a language, as `en` or `en-US`" )

      def sound(translation: Translation): Boolean =
        Tongue.valid(translation.language) && translation.fallback.lay(true)(Tongue.valid(_))

      val targets: List[Translations.Target] = section.translation.filter(sound(_)).map: line =>
        Translations.Target(line.language, directory, within(home, line.path), line.fallback)

      // A fallback is the section's own language, or another it is translated into.
      def grounded(target: Translations.Target): Boolean =
        target.fallback.lay(true): (fallback: Text) =>
          fallback != target.tongue &&
            (language == fallback || targets.exists(_.tongue == fallback))

      val malformed: List[Text] = section.translation.filter(!sound(_)).map: line =>
        t"`translation ${line.language} ${line.path}`, of `section $written`, is not into a " +
          t"language, as `fr` or `pt-BR`, or falls back to what is not one"

      val groundless: List[Text] = targets.filter(!grounded(_)).map: target =>
        t"`translation ${target.tongue}`, of `section $written`, falls back to " +
          t"`${target.fallback.or(t"")}`, which is neither the section's original language " +
          t"nor another it is translated into"

      val own: List[Text] = targets.filter(_.tongue == language).map: target =>
        t"`translation ${target.tongue}`, of `section $written`, is into the language the " +
          t"section is written in"

      val same: List[Text] = targets.filter(_.directory == directory).map: target =>
        t"`translation ${target.tongue}`, of `section $written`, is in the directory of " +
          t"the text it translates"

      val kept: List[Translations.Target] =
        targets.filter { target => target.tongue != language && target.directory != directory }
        . map { target => if grounded(target) then target else target.copy(fallback = Unset) }

      val found: Optional[Group] =
        if !misnamed.nil || !unspoken.nil then Unset else
          Group
            ( section.name, directory, language, original.metadata.let(within(directory, _)),
              original.overlay.map(within(directory, _)),
              original.glossary.map(within(directory, _)), kept )

      (found, misnamed + unspoken + malformed + groundless + own + same)

    val read: List[(Optional[Group], List[Text])] =
      if !unschematic.nil then List((Unset, unschematic)) else
        project.config.lay(List[(Optional[Group], List[Text])]()): (tel: Tel) =>
          safely(tel.as[Sections]).lay(List[(Optional[Group], List[Text])]()): sections =>
            sections.section.map(group(_))

    val groups: List[Group] = read.bind(_(0).let(List(_)).or(Nil))

    // What is of a `section`, at the top level, is of nothing; and a `document` in the
    // configuration is what a `section` was.
    val loose: List[Text] = project.config.lay(Nil: List[Text]): (tel: Tel) =>
      val stray: List[Text] = List(t"translation", t"original").bind: keyword =>
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
