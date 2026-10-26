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
      output:    Optional[Text] = Unset )

  def load(directory: Text)(using Cli, Interpreter, Configurator): Config =
    // The file's directory is `.pyrocosm/fuel`, so the root is three levels up.
    val root: Text =
      Fuel.repoFile(directory).lay(directory): (file: Path on Linux) =>
        val pyrocosm: Optional[Path on Linux] = file.parent.let { (dir: Path on Linux) => dir.parent }
        val project: Optional[Path on Linux] = pyrocosm.let { (dir: Path on Linux) => dir.parent }
        project.lay(directory) { (dir: Path on Linux) => dir.encode }

    def entries(setting: Optional[Text]): List[Text] =
      val split: List[Text] = setting.lay(Nil: List[Text]) { (text: Text) => text.cut(t":").filter(_ != t"") }
      split.map { (entry: Text) => if entry.starts(t"/") then entry else t"$root/$entry" }

    val options: List[Text] =
      ui.ScalacOption().lay(Nil: List[Text]) { (text: Text) => text.cut(t":").filter(_ != t"") }

    val fixtures: Optional[Text] =
      ui.Fixtures().let { (entry: Text) => if entry.starts(t"/") then entry else t"$root/$entry" }

    val imports: List[Text] =
      ui.Imports().lay(Nil: List[Text]) { (text: Text) => text.cut(t":").filter(_ != t"") }

    val output: Optional[Text] =
      ui.Output().let { (entry: Text) => if entry.starts(t"/") then entry else t"$root/$entry" }

    Config
      ( root, entries(ui.Classpath()), entries(ui.Document()), options, ui.Session().or(false), fixtures,
        entries(ui.Source()), entries(ui.Overlay()), imports, output )
