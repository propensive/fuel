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
// TEL overlays which document it, the Orthant store which holds those of them that are in its
// working tree, and the imports a document's references resolve through. Relative entries
// resolve against the root, as flame's do.
object Workspace:
  // A configuration's store and its sections, as the schema has them: what `store` and
  // `section` say, each member a keyword which may be written as a child or, in this order,
  // on its parent's line.
  case class Depository(path: Text, at: List[Text] = Nil)

  case class Section
    ( name:     Text,
      path:     Text,
      language: Optional[Text] = Unset,
      metadata: Optional[Text] = Unset,
      overlay:  List[Text]     = Nil,
      glossary: List[Text]     = Nil )

  case class Sections(store: Optional[Depository] = Unset, section: List[Section] = Nil)

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
  extends Error(m"the configuration has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text): Faults = Faults(items + List((error, pointer)))

  // Every fault the schema finds in a configuration, with the path of what it concerns.
  def faults(tel: Tel): List[(Tel.Error, Text)] =
    val found: Faults =
      validate[Tel.Focus](Faults()):
        case error: Tel.Error => accrual + (error, prior.let(_.pointer.encode).or(t"/"))

      . protect(Tel.Type.assign(tel, schema))

    found.items

  // The code of what is wrong with the configuration's sections, `↯/f.27.3.<variant>`: a fault
  // the schema finds, which carries the TEL error's own code (1); a section's name which cannot
  // be one (2); a section written in what is not a language (3); an `original` or a
  // `translation`, as a section's languages were configured, beneath no `section` (8); a
  // `document`, as a section was configured (9); two sections of one name (10); and no
  // section at all (11). The fourth to the seventh were of a `translation`, and are no longer
  // raised: a section's languages are those of the Orthant store which holds it, whose own
  // problems have the store's code (`Depot`).
  private[fuel] def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.client, FuelBolts.sections, variant)

  // What a fault the schema finds in a section means, by the path of what it concerns:
  // `section`, the section's index, and the keywords and indices beneath it.
  private def explain(tel: Tel, error: Tel.Error, path: List[Text]): Text =
    import Tel.Error.Reason

    val keywords: List[Text] = path.filter { segment => safely(segment.as[Int]).absent }
    val last: Text = keywords.reverse.head.or(t"")
    val parent: Text = keywords.reverse.skip(1).head.or(t"")
    val index: Int = path.skip(1).head.let { text => safely(text.as[Int]) }.or(0)

    val named: Text =
      tel.fields(t"section").to[List].at(Ordinal.zerary(index)).lay(t"a `section`"):
        (node: Tel) => t"`section ${node.atomTexts.to[List].filter(_ != t"").join(t" ")}`"

    // How a store and a section are written.
    val stored: Text = t"`store .`, with `at release=1.2.0` beneath it"

    error.reason match
      case Reason.UnknownKeyword =>
        if last == t"original" || last == t"translation" || last == t"definitive" then
          t"`$last`, of $named, is no longer how a section's languages are said: they are " +
            t"those of the Orthant store which holds its documents, as $stored, and what " +
            t"`original` held — `metadata`, `overlay`, `glossary` — is beneath the section itself"
        else if last == t"fallback" then
          t"`fallback`, of $named, is no longer said: in an Orthant store a language reads " +
            t"what nobody has translated as the language above it has it"
        else if last == t"details" then
          t"`details`, of $named, is now the section's `metadata`, as `metadata section.tel`"
        else t"`$last` is not something a `$parent` says, in $named"

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        if last == t"section" || parent == t"" then
          t"$named is not a section's name and its directory, perhaps with its language and " +
            t"its file, as `section guide doc en section.tel`"
        else if last == t"store" then
          t"`store` is the directory an Orthant store is in, as $stored"
        else t"a `$last` of $named has more values than it takes"

      case Reason.RequiredMemberAbsent =>
        if parent == t"section" then
          t"$named is not a section's name and its directory, as `section guide doc`"
        else if parent == t"store" then
          t"`store` does not say the directory the Orthant store is in, as $stored"
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
      // What is wrong with how the sections and the store are configured, with its code; and
      // each section's directory with the language it is written in, where that is known.
      badSections:     List[(Text, fulminate.Bolt)] = Nil,
      languages:       List[(Text, Text)]           = Nil,
      // Each section, a name and its directory.
      sections:        List[(Text, Text)]           = Nil,
      // Each section's file, by the section's directory.
      sectionFiles:    List[(Text, Text)]           = Nil,
      // The Orthant store which holds the documents within its working tree, in each language.
      depot:           Optional[Depot]              = Unset ):

    // Where a file of the documentation is read from: the store, for what is in its working
    // tree, as the language the documents are written in has it, or as another does; and
    // the filesystem for everything else.
    def library: Library = depot.lay(Library.Disk)(_.library())
    def library(tongue: Text): Library = depot.lay(Library.Disk)(_.library(tongue))

    // The languages the documents are translated into: the store's, but the one they are
    // written in.
    def tongues: List[Text] = depot.lay(Nil)(_.tongues)

    // A problem of a file as a language reads it, which says so.
    def attributed(block: pyrocosm.Block, tongue: Text): pyrocosm.Block =
      depot.lay(block)(_.attributed(block, tongue))

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
    // `path`, perhaps the `language` it is written in, and its `metadata`, the file which
    // says what a reader is told of the section; and beneath it what else is found in its
    // directory, each `overlay` directory and each `glossary`. Every path is within its
    // parent's: a section's in the project's, the others in the section's. Each of these is
    // a member with a keyword, which may be written as a child, or in order on its parent's
    // line. A `store` is read so too: the directory an Orthant store is in, and beneath it
    // an `at` for the value of each axis but the language's which the documents are read at:
    //
    //     store .
    //       at release=1.2.0
    //     section guide docs
    //       metadata section.tel
    //       overlay api
    //       glossary glossary.tel
    //
    // A section whose directory is within the store's working tree is read from the store,
    // in each language the store has, and is written in the first of them; any other is
    // read from the filesystem, in the one language it says, or says nothing of. What is
    // wrong with the sections is kept, to be reported; where the schema finds a fault in
    // them, none is read.
    def words(tel: Tel): List[Text] = tel.atomTexts.to[List].filter(_ != t"")

    def lines(tel: Tel, keyword: Text): List[List[Text]] =
      tel.fields(keyword).to[List].map(words(_))

    // A path without its `.` and `..` segments, each `..` taking the segment before it: a
    // file is named one way, whichever way the configuration reaches it, so that a file in
    // the store's working tree is known to be.
    def direct(path: Text): Text =
      val segments: List[Text] =
        path.cut(t"/").fold(List[Text]()): (done: List[Text], segment: Text) =>
          if segment == t"." || (segment == t"" && !done.nil) then done
          else if segment == t".." && !done.nil && done.last != t"" then done.keep(done.size - 1)
          else done + List(segment)

      segments.join(t"/")

    def within(directory: Text, entry: Text): Text =
      if entry.starts(t"/") then direct(entry)
      else if entry == t"." || entry == t"./" then direct(directory)
      else direct(t"$directory/$entry")

    case class Group
      ( name:       Text,
        directory:  Text,
        language:   Optional[Text],
        file:       Optional[Text],
        overlays:   List[Text],
        glossaries: List[Text] )

    // Whether a name can be a section's: it is one segment of a page's address.
    def nameable(name: Text): Boolean =
      name != t"" && !name.contains(t"/") && !name.contains(t"#")

    val form: Text = t"`section guide doc`"

    // A problem's sentence with its code.
    type Problem = (Text, fulminate.Bolt)

    // The faults the schema finds in the sections, each as a sentence.
    val unschematic: List[Problem] = project.config.lay(Nil: List[Problem]): (tel: Tel) =>
      faults(tel).bind: (error: Tel.Error, pointer: Text) =>
        val path: List[Text] = pointer.cut(t"/").filter(_ != t"")

        if path.head.or(t"") != t"section" && path.head.or(t"") != t"store" then Nil
        else List((explain(tel, error, path), bolt(1).carrying(error.bolt)))

    // The configuration as the schema reads it, where it finds no fault.
    val written: Optional[Sections] =
      if !unschematic.nil then Unset
      else project.config.let { (tel: Tel) => safely(tel.as[Sections]) }

    // The store, opened, with what is wrong with it.
    val opened: (Optional[Depot], List[Problem]) =
      written.let(_.store).lay((Unset, List[Problem]())):
        (store: Depository) => Depot.open(direct(project.resolve(store.path).encode), store.at)

    val depot: Optional[Depot] = opened(0)
    val unstored: List[Problem] = opened(1)

    // A section as it is written, made the directories and files it names; and what is wrong
    // with it which the schema cannot say. A section within the store's working tree is
    // written in the store's first language, whatever it says.
    def group(section: Section): (Optional[Group], List[Problem]) =
      val written: Text = t"${section.name} ${section.path}"
      val directory: Text = direct(project.resolve(section.path).encode)

      val stored: Boolean = depot.lay(false): (depot: Depot) =>
        directory == depot.root || depot.document(directory).present

      val language: Optional[Text] =
        if stored then depot.let(_.original).or(section.language) else section.language

      val misnamed: List[Text] =
        if nameable(section.name) then Nil else
          List
            ( t"`section $written` names the section `${section.name}`, and a name is one " +
                t"segment of a page's address, so it cannot hold `/` or `#`" )

      val unspoken: List[Text] = language.lay(Nil): (language: Text) =>
        if Tongue.valid(language) then Nil else
          List
            ( t"`section $written` is written in `$language`, which is not a language, as " +
                t"`en` or `en-US`" )

      val found: Optional[Group] =
        if !misnamed.nil || !unspoken.nil then Unset else
          Group
            ( section.name, directory, language, section.metadata.let(within(directory, _)),
              section.overlay.map(within(directory, _)),
              section.glossary.map(within(directory, _)) )

      val problems: List[List[Problem]] =
        List(misnamed, unspoken).indexed.map:
          (texts: List[Text], index: Ordinal) => texts.map((_, bolt(index.n0 + 2)))

      (found, problems.bind(identity(_)))

    val read: List[(Optional[Group], List[Problem])] =
      if !unschematic.nil then List((Unset, unschematic))
      else written.lay(List[(Optional[Group], List[Problem])]())(_.section.map(group(_)))

    val groups: List[Group] = read.bind(_(0).let(List(_)).or(Nil))

    // An `original` or a `translation`, as a section's languages were configured, is of
    // nothing at the top level; and a `document` in the configuration is what a `section`
    // was.
    val loose: List[Problem] = project.config.lay(Nil: List[Problem]): (tel: Tel) =>
      val stray: List[Problem] = List(t"translation", t"original").bind: keyword =>
        lines(tel, keyword).map: (atoms: List[Text]) =>
          val message: Text =
            t"`$keyword ${atoms.join(t" ")}` is no longer how a section's languages are " +
              t"said: they are those of the Orthant store which holds its documents"

          (message, bolt(8))

      val former: List[Problem] = lines(tel, t"document").map: (atoms: List[Text]) =>
        val message: Text =
          t"`document ${atoms.join(t" ")}` is no longer how a directory of documents is " +
            t"configured: it is a section, as $form"

        (message, bolt(9))

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
    val shared: List[Problem] =
      sections.map(_(0)).distinct.filter(name => sections.filter(_(0) == name).size > 1)
      . map: (name: Text) =>
          (t"more than one `section` is named `$name`: each has a name of its own", bolt(10))

    val badSections: List[Problem] = read.bind(_(1)) + unstored + loose + shared

    val languages: List[(Text, Text)] = groups.bind: (group: Group) =>
      group.language.lay(List[(Text, Text)]()): (language: Text) =>
        List((group.directory, language))

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
        badSections,
        languages,
        sections,
        sectionFiles,
        depot )
