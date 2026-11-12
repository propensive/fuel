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
      schemas:    List[Text]        = Nil )

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

    Config
      ( root,
        entries(ui.Classpath()),
        entries(ui.Document()),
        options,
        ui.Session().or(false),
        fixtures,
        entries(ui.Source()),
        entries(ui.Overlay()),
        imports,
        output,
        entries(ui.Glossary()),
        ui.CheckLinks().or(false),
        sync,
        badSync,
        entries(ui.Schema()) )
