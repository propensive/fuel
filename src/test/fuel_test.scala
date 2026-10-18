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

import soundness.{Language as _, Token as _, *}

import dysasymptotics.linearSize
import pyrocosm.{Block, Inline, Language, Token}

object Tests extends Suite(m"Fuel tests"):
  val intro: Text = t"""# Getting started

Some *prose*, with `inline code`.

```scala
val x: Int = 42
```

```sh
make run
```
"""

  def run(): Unit =
    suite(m"Markdown to blocks"):
      test(m"a heading, a paragraph and two fences become four blocks"):
        Doc.parse(t"doc/intro.md", intro).blocks.size

      . assert(_ == 4)

      test(m"the title is the first level-one heading"):
        Doc.parse(t"doc/intro.md", intro).title

      . assert(_ == t"Getting started")

      test(m"a document without a heading is titled by its filename"):
        Doc.parse(t"doc/notes/todo.md", t"just a line\n").title

      . assert(_ == t"todo")

      test(m"a scala fence is highlighted into several tokens"):
        Doc.parse(t"doc/intro.md", intro).blocks match
          case _ :: _ :: Block.Code(Language.Scala, line :: _, _) :: _ => line.tokens.size > 1
          case _                                                       => false

      . assert(_ == true)

      test(m"a fence in an unknown language is plain, under that language"):
        Doc.parse(t"doc/intro.md", intro).blocks match
          case _ :: _ :: _ :: Block.Code(language, lines, _) :: _ => (language.name, lines.size)
          case _                                                  => (t"", 0)

      . assert(_ == (t"sh", 1))

    suite(m"Sections"):
      test(m"a section is named by its directory's last segment"):
        Compilation.name(t"/home/me/project/doc/modules/")

      . assert(_ == t"modules")
