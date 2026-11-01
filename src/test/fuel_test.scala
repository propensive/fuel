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

import scala.collection.immutable as sci

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Compilation as _, Language as _, Token as _, Query as _, *}

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Inline, Language, Purview, TerminalRenderer, Token, Tone}
import systems.javaBaseSystem

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

  // The suite's own classpath, from its classloader (so it is the same under fume, which runs
  // the suite in-process, as under plain java), carrying the Scala library the fragments need.
  val classpath: LocalClasspath = LocalClasspath.of(Classloader[Tests.type])

  // The sample's model, from the Inspection suite, for the Overlays suite to document.
  @scala.caps.unsafe.untrackedCaptures
  var api0: Api = Api.empty

  @scala.caps.unsafe.untrackedCaptures
  var chainSvg: Text = t""

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
          case _ :: _ :: Block.Code(Language.Scala, line :: _, _, _, _, _) :: _ =>
            line.tokens.size > 1

          case _ =>
            false

      . assert(_ == true)

      test(m"a fence in an unknown language is plain, under that language"):
        Doc.parse(t"doc/intro.md", intro).blocks match
          case _ :: _ :: _ :: Block.Code(language, lines, _, _, _, _) :: _ =>
            (language.name, lines.size)

          case _ =>
            (t"", 0)

      . assert(_ == (t"sh", 1))

    suite(m"Preambles"):
      val preamble: Text = t"""context term
id setup
follows intro
errors show
highlight Hel..rld  the greeting
error oops
  caption  a caption beneath
"""

      test(m"a preamble's fields decode"):
        Preamble.parse(preamble) match
          case p: Preamble => (p.context, p.id, p.follows, p.errors)
          case _           => Unset

      . assert(_ == (t"term", t"setup", List(t"intro"), Preamble.Errors.Show))

      test(m"a mark's caption may follow the selection or sit beneath it"):
        Preamble.parse(preamble) match
          case p: Preamble => p.marks.map(_.caption)
          case _           => Nil

      . assert(_ == List(t"the greeting", t"a caption beneath"))

      test(m"a preamble that is not TEL is its error"):
        Preamble.parse(t"context term\n   three spaces\n") match
          case error: Tel.Error => true
          case _                => false

      . assert(_ == true)

      test(m"a preamble that is not TEL is reported as a diagnostic at its line in the file"):
        val text: Text =
          t"# Title\n\n```scala fuel\ncontext term\n   three spaces\n##\nval x = 1\n```\n"

        Doc.parse(t"doc/bad.md", text).problems match
          case Block.Diagnostic(Tone.Failure, location, _, _, _) :: _ =>
            (location.let(_.path), location.let(_.line))

          case _ =>
            (Unset, Unset)

      . assert(_ == (t"doc/bad.md", 5))

      test(m"a fence splits at the `##` line"):
        Preamble.split(t"context term\n##\nval x = 1\nval y = 2\n")

      . assert(_ == (t"context term", t"val x = 1\nval y = 2\n"))

      test(m"a fence without a `##` line is all code"):
        Preamble.split(t"val x = 1\n")

      . assert(_ == (Unset, t"val x = 1\n"))

      test(m"a selection finds its range by its ends"):
        Selection.parse(t"Hel..rld").findIn(t"say Hello, world now")

      . assert(_ == (4, 16))

      test(m"a literal selection finds itself"):
        Selection.parse(t"world").findIn(t"say Hello, world now")

      . assert(_ == (11, 16))

      test(m"a range over two lines becomes one note spanning them"):
        val note: Block.Note = Notes.of(t"abc\ndef\nghi", 2, 5, Block.Note.Style.Highlight)
        (note.line, note.start, note.end, note.last)

      . assert(_ == (0, 2, 1, 1))

      test(m"a range of no width is a caret"):
        Notes.of(t"abc\ndef", 5, 5, Block.Note.Style.Erroneous).caret

      . assert(_ == true)

      test(m"a fragment's code is numbered from its line in the file"):
        val text: Text = t"# Title\n\n```scala fuel\nid one\n##\nval x = 1\n```\n"

        Doc.parse(t"doc/n.md", text).blocks match
          case _ :: Block.Code(_, _, _, start, _, _) :: _ => start
          case _                                          => Unset

      . assert(_ == 6)

    suite(m"Compile checking"):

      def problems(markdown: Text): List[Block] =
        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", markdown, checker).problems

      test(m"a well-typed fragment has no problems"):
        problems(t"```scala\nval n: Int = 1\n```\n")

      . assert(_ == Nil)

      test(m"an ill-typed fragment is a problem: a diagnostic at its line"):
        problems(t"```scala\nval n: Int = \"oops\"\n```\n") match
          case Block.Diagnostic(Tone.Failure, location, _, excerpt, _) :: Nil =>
            (location.let(_.path), location.let(_.line), excerpt.size)

          case _ =>
            (Unset, Unset, 0)

      . assert(_ == (t"doc/check.md", 2, 1))

      test(m"an ill-typed fragment expected to fail is not a problem"):
        problems(t"```scala fuel\nerrors show\n##\nval n: Int = \"oops\"\n```\n")

      . assert(_ == Nil)

      test(m"a fragment may follow another"):
        val first: Text = t"```scala fuel\nid setup\n##\nval base: Int = 1\n```\n"
        val second: Text = t"```scala fuel\nfollows setup\n##\nval n: Int = base + 1\n```\n"
        problems(first + second)

      . assert(_ == Nil)

      test(m"an error becomes an erroneous note on its line"):
        val fence: Text =
          t"```scala fuel\nerrors show\n##\nval a: Int = 1\nval n: Int = \"oops\"\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", fence, checker).blocks match
            case Block.Code(_, _, note :: _, _, _, _) :: Block.Diagnostic(_, _, _, _, _) :: _ =>
              (note.line, note.style)

            case _ =>
              (-1, Block.Note.Style.Param)

      . assert(_ == (1, Block.Note.Style.Erroneous))

    suite(m"Animation"):
      val animatedFence: Text =
        t"```scala fuel\nid steps\nframe\n  code\n      val x: Int = 1\nframe\n" +
          t"  code\n      val x: Int = 1\n" +
          t"      val y: Int = x + 1\nframe\n  code val z: Int = 2\n##\n```\n"

      test(m"a frame's code is its `code` child, indented beneath it or on the line"):
        val text: Text =
          t"frame\n  code\n      val x = 1\n      val y = 2\nframe\n  code val z = 3\nloop\n"

        Preamble.parse(text) match
          case p: Preamble => (p.frames.map(_.code), p.autoplay, p.loop)
          case _           => (Nil: List[Optional[Text]], true, true)

      . assert(_ == (List(t"val x = 1\nval y = 2", t"val z = 3"), false, true))

      test(m"a frame's label, hold, at and language are read"):
        val text: Text =
          t"frame\n  code\n      val x = 1\n  label the *first* step\n" +
            t"  hold 2\n  at 12.5\n  language java\n"

        Preamble.parse(text) match
          case p: Preamble =>
            p.frames.head.let: (frame: Preamble.Frame) =>
              val emphasised: Boolean = frame.label.exists:
                case Inline.Emphasis(_) => true
                case _                  => false

              (Blocks.plain(frame.label), emphasised, frame.hold, frame.at, frame.language)

          case _ =>
            Unset

      . assert(_ == (t"the first step", true, 2.0, 12.5, t"java"))

      test(m"a frame's marks are its own, refined beneath them by line and occurrence"):
        val text: Text =
          t"frame\n  code\n      val x = x\n  highlight x  the variable\n" +
            t"    line 1\n    occurrence 2\n"

        Preamble.parse(text) match
          case p: Preamble =>
            val own: Optional[List[(Block.Note.Style, Optional[Text], Optional[Int], Int)]] =
              p.frames.head.let: (frame: Preamble.Frame) =>
                frame.marks.map: (mark: Preamble.Mark) =>
                  (mark.style, mark.caption, mark.selection.line, mark.selection.occurrence)

            (p.marks.size, own)

          case _ =>
            Unset

      . assert(_ == (0, List((Block.Note.Style.Highlight, t"the variable", 1, 2))))

      test(m"a block-level language is read"):
        Preamble.parse(t"language java\n") match
          case p: Preamble => p.language
          case _           => Unset

      . assert(_ == t"java")

      test(m"a selection may name a line and an occurrence, and finds nothing beyond them"):
        ( Selection(t"x", Unset, line = 2).findIn(t"x = 1\ny = x"),
          Selection(t"x", Unset, occurrence = 2).findIn(t"x + x + x"),
          Selection(t"x", Unset, line = 3).findIn(t"x\ny"),
          Selection(t"x", Unset, occurrence = 4).findIn(t"x + x + x") )

      . assert(_ == ((10, 11), (4, 5), Unset, Unset))

      test(m"a frame without code is a problem at its line, and is dropped"):
        val text: Text =
          t"# T\n\n```scala fuel\nframe\n  code\n      val x = 1\nframe\n    val y = 2\n##\n```\n"

        val doc = Doc.parse(t"doc/anim.md", text)

        val problem = doc.problems match
          case Block.Diagnostic(Tone.Failure, location, paragraph :: _, _, _) :: Nil =>
            val second: Boolean = paragraph match
              case Block.Paragraph(Inline.Textual(message) :: _) => message.starts(t"Frame 2")
              case _                                             => false

            (location.let(_.line), second)

          case _ =>
            (Unset, false)

        val frames = doc.blocks match
          case _ :: Block.Code(_, _, _, _, animation, _) :: _ => animation.let(_.slides.size)
          case _                                              => Unset

        (problem, frames)

      . assert(_ == ((7, true), 1))

      test(m"a frame's details reach the animation, and a block-level mark every frame it matches"):
        val text: Text =
          t"```scala fuel\nhighlight x\nframe\n  code\n      val x: Int = 1\n" +
            t"frame\n  label a *second* value\n  hold 2\n  at 12.5\n" +
            t"  highlight y  the new one\n  code\n      val y: Int = 2\n##\n```\n"

        Doc.parse(t"doc/anim.md", text).blocks match
          case Block.Code(_, _, notes, _, animation, _) :: Nil =>
            animation.let: (animation: Block.Animation) =>
              val second: Block.Slide = animation.slides.skip(1).head.or(Block.Slide(-1))
              val label: Text = Blocks.plain(second.label)
              (notes.size, animation.slides.map(_.notes.size), label, second.hold, second.at)

          case _ =>
            Unset

      . assert(_ == (1, List(1, 1), t"a second value", 2.0, 12.5))

      test(m"a java frame is highlighted without the checker, in its own language"):
        val text: Text =
          t"```scala fuel\nframe\n  code\n      val x: Int = 1\nframe\n" +
            t"  language java\n  code\n      int x = 1;\n##\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          val doc = Doc.parse(t"doc/anim.md", text, checker)

          doc.blocks match
            case Block.Code(_, _, _, _, animation, _) :: Nil =>
              animation.let: (animation: Block.Animation) =>
                ( doc.problems.size,
                  animation.slides.skip(1).head.let(_.language),
                  animation.lines(1).head.let(_.size > 1) )

            case _ =>
              Unset

      . assert(_ == (0, Language.Java, true))

      test(m"a java fence with a preamble is read by the preamble embedding"):
        Doc.parse(t"doc/j.md", t"```java fuel\nid j\n##\nint x = 1;\n```\n").blocks match
          case Block.Code(language, line :: _, _, _, _, _) :: Nil =>
            (language, line.tokens.size > 1)

          case _ =>
            Unset

      . assert(_ == (Language.Java, true))

      test(m"a fragment follows the last scala frame of an animation"):
        val first: Text =
          t"```scala fuel\nid setup\nframe\n  code\n      val base: Int = 1\nframe\n" +
            t"  language java\n  code\n      int base = 1;\n##\n```\n"

        val second: Text = t"```scala fuel\nfollows setup\n##\nval n: Int = base + 1\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", first + second, checker).problems

      . assert(_ == Nil)

      test(m"an animated fence becomes one block whose lines are the first frame"):
        Doc.parse(t"doc/anim.md", animatedFence).blocks match
          case Block.Code(_, lines, _, _, animation, _) :: Nil =>
            (lines.size, animation.let(_.slides.size), animation.let(_.lines(1).size))

          case _ =>
            (0, Unset, Unset)

      . assert(_ == (1, 3, 2))

      test(m"a surviving token keeps its atom across frames"):
        Doc.parse(t"doc/anim.md", animatedFence).blocks match
          case Block.Code(_, _, _, _, animation, _) :: Nil =>
            animation.let: (animation: Block.Animation) =>
              val first: List[Text] = animation.placements.filter(_.slide == 0).map(_.atom)
              val second: List[Text] = animation.placements.filter(_.slide == 1).map(_.atom)
              first.all(second.has(_))

          case _ =>
            Unset

      . assert(_ == true)

      test(m"a frame that does not compile is a problem naming the frame"):
        val fence: Text =
          t"```scala fuel\nframe\n  code\n      val x: Int = 1\nframe\n  code\n" +
            t"      val x: Int = \"oops\"\n##\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/anim.md", fence, checker).problems match
            case Block.Diagnostic(Tone.Failure, _, message, _, _) :: Nil =>
              message match
                case Block.Paragraph(Inline.Textual(text) :: _) :: _ => text.starts(t"Frame 2:")
                case _                                               => false

            case _ =>
              false

      . assert(_ == true)

    suite(m"Sessions"):
      def problems(markdown: Text, fixture: Optional[(Text, Text)] = Unset): List[Block] =
        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", markdown, checker, true, fixture).problems

      def blocks(markdown: Text): List[Block] =
        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", markdown, checker, true).blocks

      def line(problem: Block): Optional[Int] = problem match
        case Block.Diagnostic(_, location, _, _, _) => location.let(_.line)
        case _                                      => Unset

      test(m"a fragment sees a value an earlier one defined"):
        problems(t"```scala\nval a: Int = 1\n```\n\n```scala\nval b: Int = a + 1\n```\n")

      . assert(_ == Nil)

      test(m"a bare expression is a statement of the session"):
        problems(t"```scala\nval a: Int = 1\n```\n\n```scala\na + 1\n```\n")

      . assert(_ == Nil)

      test(m"`restart` begins the session afresh"):
        val text: Text =
          t"```scala\nval a: Int = 1\n```\n```scala fuel\nrestart\n##\nval b: Int = a\n```\n"

        problems(text).map(line(_))

      . assert(_ == List(7))

      test(m"a name defined again hides the earlier definition"):
        problems:
          t"```scala\nval a: Int = 1\n```\n```scala\nval a: Int = 2\n```\n" +
            t"```scala\nval b: Int = a + 1\n```\n"

      . assert(_ == Nil)

      test(m"an earlier fragment's imports are in scope for a later one"):
        problems:
          t"```scala\nimport scala.collection.mutable.ArrayBuffer\nval a: Int = 1\n```\n" +
            t"```scala\nval buffer: ArrayBuffer[Int] = ArrayBuffer(a)\n```\n"

      . assert(_ == Nil)

      test(m"a fixture is compiled before the first fragment"):
        val fixture: (Text, Text) = (t"doc/fixtures/check.scala", t"val fixed: Int = 1")
        problems(t"```scala\nfixed + 1\n```\n", fixture)

      . assert(_ == Nil)

      test(m"a fixture which does not compile is a problem at its own path"):
        val fixture: (Text, Text) = (t"doc/fixtures/check.scala", t"val fixed: Int = \"x\"")

        problems(t"```scala\nval n: Int = 1\n```\n", fixture) match
          case Block.Diagnostic(_, location, _, _, _) :: Nil =>
            (location.let(_.path), location.let(_.line))

          case _ =>
            Unset

      . assert(_ == (t"doc/fixtures/check.scala", 1))

      test(m"an `errors show` fragment does not join the session"):
        val text: Text =
          t"```scala\nval a: Int = 1\n```\n" +
            t"```scala fuel\nerrors show\n##\nval c: Int = \"x\"\n```\n" +
            t"```scala\nval d: Int = c\n```\n"

        problems(text).map(line(_))

      . assert(_ == List(10))

      test(m"a parsed fragment passes a type error and fails a syntax error"):
        ( problems(t"```scala fuel\nparse\n##\nval n: Int = \"x\"\n```\n").size,
          problems(t"```scala fuel\nparse\n##\nval = 1\n```\n").size )

      . assert(_ == (0, 1))

      test(m"a compiled fragment reports a type error, and passes a feature flag"):
        val feature: Text =
          t"```scala fuel\ncompile\nfeature experimental.captureChecking\n##\nval n: Int = 1\n```\n"

        val typed: Text = t"```scala fuel\ncompile\n##\nval n: Int = \"x\"\n```\n"
        (problems(typed).size, problems(feature).size)

      . assert(_ == (1, 0))

      test(m"a line marked as not compiling must not compile, and is noted"):
        val text: Text = t"```scala\nval n: Int = 1\nval m: Int = \"x\" // does not compile\n```\n"

        val note: Optional[(Int, Block.Note.Style)] = blocks(text) match
          case Block.Code(_, _, note :: _, _, _, _) :: _ => (note.line, note.style)
          case _                                         => Unset

        (problems(text).size, note)

      . assert(_ == (0, (1, Block.Note.Style.Erroneous)))

      test(m"a line marked as not compiling which compiles is a problem at its line"):
        val text: Text = t"```scala\nval n: Int = 1\nval m: Int = 2 // does not compile\n```\n"
        problems(text).map(line(_))

      . assert(_ == List(3))

      test(m"a diagnostic in a session is at its line and column in the file"):
        problems(t"```scala\nval a: Int = 1\n```\n```scala\nval b: Int = \"oops\"\n```\n") match
          case Block.Diagnostic(_, location, _, _, _) :: Nil =>
            (location.let(_.line), location.let(_.column))

          case _ =>
            Unset

      . assert(_ == (5, 14))

      test(m"a document is titled by its first heading, of any level"):
        Doc.parse(t"doc/x.md", t"## Hello\n\ntext\n").title

      . assert(_ == t"Hello")

      test(m"an HTML comment is no block"):
        Doc.parse(t"doc/x.md", t"<!-- doccheck: skip -->\n\n```scala\nval x = 1\n```\n").blocks.size

      . assert(_ == 1)

    suite(m"Showing"):
      test(m"a document is compiled by its path, and an animated block has steps in the terminal"):
        import hyphenations.englishHyphenation
        import textMetrics.uniformMetric
        import tableStyles.thickTableStyle
        import palettes.solarizedDarkGaugePalette

        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)

        Compilation.document(t"$here/doc/markdown.md").let: (doc: Doc) =>
          val renderer = TerminalRenderer(justify = true)
          val rows: List[Text] = renderer.blocks(doc.blocks, 80).map(_.plain)
          (rows.count(_.starts(t"step 1 of")), rows.exists(_.starts(t"step 3 of 3")))

      . assert(_ == (1, true))

    suite(m"Presentations"):
      val talk: Text = t"""title  Effects, gently

slide
  markdown
      # Effects, gently
      A *short* talk.

slide
  code scala
      val x = 1

slide
  code scala
      val x = 1
      val y = x + 1

slide
  code scala
      val x = 1
      val y = x + 1
      val z = y * 2

slide
  code java
      int x = 1;

slide
  markdown
      The end.
"""

      def read(text: Text): Optional[(Presentation, List[Block])] =
        Presentation.read(t"talk.tel", text) match
          case (presentation: Presentation, raised: List[Block] @unchecked) =>
            (presentation, raised)

          case _ =>
            Unset

      def leaves(text: Text): List[Block.Leaf] =
        read(text).lay(Nil): (presentation: Presentation, _) =>
          val scala = Embedding.ScalaCode(Checker.tokenized, t"talk.tel")

          Presentation.deck(presentation, scala, Linker.none) match
            case Block.Deck(leaves) => leaves
            case _                  => Nil

      test(m"a presentation is its title and its slides, each with its parts"):
        read(talk).let: (presentation: Presentation, problems: List[Block]) =>
          val lines: List[Int] = presentation.slides.map(_.line)
          (presentation.title, presentation.slides.size, lines, problems.size)

      . assert(_ == (t"Effects, gently", 6, List(3, 8, 12, 17, 23, 27), 0))

      test(m"a run of code slides in one language is one animated block, its frames the slides"):
        val all: List[Block.Leaf] = leaves(talk)

        val animated: List[Int] = all.bind: (leaf: Block.Leaf) =>
          leaf.content.bind:
            case Block.Code(_, _, _, _, animation: Block.Animation, _) =>
              List(animation.slides.size)

            case _ =>
              Nil

        (all.size, animated)

      . assert(_ == (4, List(3)))

      test(m"markdown on a slide is blocks, and code on a slide is not numbered"):
        val all: List[Block.Leaf] = leaves(talk)

        val first: Optional[Text] = all.head.let(_.content.head).let:
          case Block.Heading(1, content, _) => Blocks.plain(content)
          case _                            => Unset

        val numbered: List[Optional[Int]] = all.bind(_.content).bind:
          case code: Block.Code => List(code.start)
          case _                => Nil

        (first, numbered.all(_.absent))

      . assert(_ == (t"Effects, gently", true))

      test(m"a part a slide does not understand, and an empty slide, are reported at their line"):
        read(t"slide\n  picture cat.png\n\nslide\n").lay(Nil) { (_, problems) => problems }.map:
          case Block.Diagnostic(_, location, _, _, _) => location.let(_.line)
          case _                                      => Unset

      . assert(_ == List(1, 1, 4))

      def codes(text: Text): List[Block.Code] =
        leaves(text).bind(_.content).bind:
          case code: Block.Code => List(code)
          case _                => Nil

      test(m"a slide's code may say what a fence's preamble says, its code beneath `code`"):
        val slide: Text =
          t"slide\n  code scala\n    highlight y  a new name\n    code\n" +
            t"        val x = 1\n        val y = x + 1\n"

        codes(slide).bind(_.notes).map: (note: Block.Note) =>
          (note.style, Blocks.plain(note.caption), note.line)

      . assert(_ == List((Block.Note.Style.Highlight, t"a new name", 1)))

      test(m"a slide's code with frames of its own is an animation alone, not joined to others"):
        def frame(code: Text): Text = t"    frame\n      code\n$code"
        val second: Text = t"          val a = 1\n          val b = 2\n"
        val first: Text = t"          val a = 1\n"
        val framed: Text = t"slide\n  code scala\n" + frame(first) + frame(second)
        val plain: Text = t"slide\n  code scala\n      val c = 3\n"

        codes(framed + plain).map(_.animation.let(_.slides.size))

      . assert(_ == List(2, Unset))

      test(m"each slide of a run is a frame, with the slide's label"):
        def step(label: Text, code: Text): Text =
          t"slide\n  code scala\n    label  $label\n    code\n        $code\n"

        val run: Text = step(t"first", t"val x = 1") + step(t"second", t"val y = 2")

        def labels(animation: Block.Animation): List[Text] =
          animation.slides.map(_.label).map(Blocks.plain)

        codes(run).bind(_.animation.let(labels).or(Nil))

      . assert(_ == List(t"first", t"second"))

      test(m"a presentation is shown on a stage"):
        Presenter.interface(t"Talk", Block.Deck(Nil)).hints.has[pyrocosm.hints.web.Stage.type]

      . assert(_ == true)

      test(m"a presentation which is not TEL is a diagnostic"):
        Presentation.read(t"talk.tel", t"slide\n      too deep\n  markdown\n") match
          case diagnostic: Block.Diagnostic => true
          case _                            => false

      . assert(_ == true)

    suite(m"Incremental compilation"):
      // A checker which counts the fragments it is asked to check.
      class Counting(inner: Checker) extends Checker:
        var checks: Int = 0

        def check(code: Text, mode: Preamble.Mode, context: Optional[harlequin.Scala.Context])
        :   SourceCode =

          checks += 1
          inner.check(code, mode, context)

        def enter(name: Text, unit: Text): Boolean = inner.enter(name, unit)

        def build(sources: Map[Text, Text], output: Text): List[Notice] =
          inner.build(sources, output)

        def context: Optional[dotty.tools.dotc.core.Contexts.Context] = inner.context
        def compiles: Boolean = inner.compiles

      def document(b: Text, prose: Text = t""): Text =
        t"# Fragments\n\n${prose}Some prose.\n\n```scala\nval a = 1\n```\n\nMore prose.\n\n" +
          t"```scala\n$b\n```\n\n```scala\nval c = 3\n```\n"

      // The checks a second compile makes, after a first of `first`, and the two compiles'
      // documents.
      def twice(first: Text, second: Text, session: Boolean, path: Text): (Int, Doc, Doc) =
        Checking.session(classpath, Nil): (checker: Checker) =>
          val cache: Cache = Cache.Transient()
          val counting = Counting(checker)

          def parse(text: Text): Doc =
            Doc.parse(path, text, counting, session, Unset, Linker.none, cache, t"base")

          val one: Doc = parse(first)
          counting.checks = 0
          val two: Doc = parse(second)
          (counting.checks, one, two)

      def starts(doc: Doc): List[Optional[Int]] = doc.blocks.bind:
        case code: Block.Code => List(code.start)
        case _                => Nil

      test(m"an unchanged document is compiled from what was kept, to the same blocks"):
        val unchanged: Text = document(t"val b = a + 1")
        val (checks, one, two) = twice(unchanged, unchanged, true, t"doc/s1.md")
        (checks, one.blocks == two.blocks, one.problems == two.problems)

      . assert(_ == (0, true, true))

      test(m"a change of prose compiles no fence, and the fences' lines move with them"):
        val longer: Text = document(t"val b = a + 1", t"Two lines\nof prose.\n\n")
        val (checks, one, two) = twice(document(t"val b = a + 1"), longer, true, t"doc/s2.md")

        val paragraph: List[Inline] = two.blocks.at(Sec).lay(Nil):
          case Block.Paragraph(content) => content
          case _                        => Nil

        (checks, starts(two) == starts(one).map(_.let(_ + 3)), Blocks.plain(paragraph))

      . assert(_ == (0, true, t"Two lines of prose."))

      test(m"in a session, a changed fence is compiled with those after it, and not those before"):
        twice(document(t"val b = a + 1"), document(t"val b = a + 2"), true, t"doc/s3.md")(0)

      . assert(_ == 2)

      test(m"outside a session, a changed fence is compiled alone"):
        twice(document(t"val b = 2"), document(t"val b = 3"), false, t"doc/s4.md")(0)

      . assert(_ == 1)

      test(m"a document compiled through a cache has the blocks it has without one"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val path = java.nio.file.Path.of(s"$here/doc/markdown.md")
        val text: Text = Text(java.nio.file.Files.readString(path).nn)

        Checking.session(classpath, Nil): (checker: Checker) =>
          val plain: Doc = Doc.parse(t"markdown.md", text, checker, true)
          val cache: Cache = Cache.Transient()

          val kept: Doc =
            Doc.parse(t"markdown.md", text, checker, true, Unset, Linker.none, cache, t"base")

          (plain.blocks == kept.blocks, plain.problems.size == kept.problems.size)

      . assert(_ == (true, true))

    suite(m"Output directory"):
      test(m"a prose with linkages and anchors round-trips as BinTEL"):
        val linkages: List[Linkage] =
          List
            ( Linkage(3, Linkage.Target.Document(t"doc", t"a.md", t"top")),
              Linkage(4, Linkage.Target.Site(t"/api/x", Unset)),
              Linkage(5, Linkage.Target.Anchor(t"set-up")),
              Linkage(6, Linkage.Target.Url(t"https://example.com/")),
              Linkage(7, Linkage.Target.Sigil(t"#Nope")) )

        val linked: Output.Prose =
          Output.Prose(List(Block.paragraph(t"x")), Nil, linkages, List(t"top"))

        Output.Prose.decode(Output.Prose.encode(linked)) == linked

      . assert(_ == true)

      val fragment: Output.Fragment =
        Output.Fragment(List(Block.paragraph(t"code")), Nil, true, List(t"x"), Nil, t"val x = 1", 4)

      val prose: Output.Prose =
        Output.Prose(List(Block.paragraph(t"prose"), Output.sentinel(0)), Nil)

      def directory(): Text =
        Text(java.nio.file.Files.createTempDirectory("fuel-output").nn.toString)

      def objects(output: Text): Int =
        java.nio.file.Files.list(java.nio.file.Path.of(output.s, "objects")).nn.count.toInt

      test(m"what a compile keeps, the next reads back"):
        val output = directory()
        val first = Store(output, false)
        first.fragment(t"k1", fragment)
        first.prose(t"k2", prose)
        first.finish(t"1.0", t"classpath", t"environment")
        val second = Store(output, false)
        (second.fragment(t"k1") == fragment, second.prose(t"k2") == prose, second.hits.get)

      . assert(_ == (true, true, 2))

      test(m"a record the next compile does not use is deleted"):
        val output = directory()
        val first = Store(output, false)
        first.fragment(t"k1", fragment)
        first.prose(t"k2", prose)
        first.finish(t"1.0", t"classpath", t"environment")
        val second = Store(output, false)
        second.fragment(t"k1")
        second.finish(t"1.0", t"classpath", t"environment")
        (objects(output), Store(output, false).prose(t"k2").absent)

      . assert(_ == (1, true))

      test(m"`--force` keeps nothing"):
        val output = directory()
        val first = Store(output, false)
        first.fragment(t"k1", fragment)
        first.finish(t"1.0", t"classpath", t"environment")
        Store(output, true).fragment(t"k1").absent

      . assert(_ == true)

      test(m"the manifest names its inputs, and each document by its section"):
        val output = directory()
        val store = Store(output, false)
        store.document(t"doc/modules", Doc(t"json.md", t"JSON", List(Block.paragraph(t"text"))))
        store.finish(t"1.0", t"classpath", t"environment")

        Store.manifest(output).let: (manifest: Output.Manifest) =>
          val documents = manifest.documents.map: (d: Output.Manifest.Document) =>
            (d.section, d.path, d.title)

          (manifest.fuel, manifest.classpath, documents)

      . assert(_ == (t"1.0", t"classpath", List((t"doc/modules", t"json.md", t"JSON"))))

    suite(m"Output"):
      test(m"a compiled document's blocks round-trip through BinTEL as a fragment's output"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)

        Compilation.document(t"$here/doc/markdown.md").lay(t"unreadable"): (doc: Doc) =>
          val fragment: Output.Fragment =
            Output.Fragment
              ( doc.blocks, doc.problems, true, List(t"x"), List(t"import a.*"), t"val x = 1", 7 )

          val data: Data = Output.Fragment.encode(fragment)

          Output.Fragment.decode(data).lay(t"undecodable"): (back: Output.Fragment) =>
            if back == fragment then t"same" else t"different"

      . assert(_ == t"same")

    suite(m"Inspection"):
      // A project's own source, compiled in a session on the suite's classpath and saved, as
      // `fuel compile` compiles the configured sources; then walked, with the saved output as
      // the project's own and the classpath as its dependencies.
      val sample: Text = t"""package sample

trait Shape[+A]:
  def area: Double

case class Doc(title: String, size: Int = 0) extends Shape[Int]:
  def area: Double = 0.0
  def shout: String = title.toUpperCase
  private def secret: Int = 1

object Doc:
  def parse(text: String): Doc = Doc(text)
  def parse(text: String, size: Int): Doc = Doc(text, size)

object Util:
  extension (text: String) def twice: String = text + text
  type Pair[A] = (A, A)
  given Ordering[Doc] = Ordering.by(_.title)
  val limit: Int = 10
"""

      val output: Text =
        Text(java.nio.file.Files.createTempDirectory("fuel-api").nn.toString)

      val entries: List[Text] = classpath().cut(t":").filter(_ != t"")

      lazy val notices: List[Notice] =
        Checking.session(classpath, Nil): (checker: Checker) =>
          checker.build(Map(t"Sample.scala" -> sample), output)

      lazy val api: Api | Throwable =
        notices
        Inspection.walk(List(output), Nil, entries)

      api0 = api match
        case api: Api => api
        case _        => Api.empty

      def entity(key: Text): Optional[Api.Entity] = api match
        case api: Api => api(key)
        case _        => Unset

      test(m"the sources compile, and their TASTy is saved"):
        (notices.filter(_.importance == Importance.Error).size, Inspection.tastyIn(output).size > 0)

      . assert(_ == (0, true))

      test(m"the inspection yields a model"):
        api match
          case api: Api => t"${api.entities.size.toString} entities"

          case error: Throwable =>
            val frames = error.getStackTrace.nn.take(6).map(_.toString)
            t"${error.toString}: ${java.lang.String.join("; ", frames*).nn}"

      . assert(_.ends(t"entities"))

      test(m"a case class and its companion are one entity, with a template and a definition"):
        entity(t"sample.Doc").let: (doc: Api.Entity) =>
          (doc.own, doc.template.let(_.kind), doc.definition.let(_.kind))

      . assert(_ == (true, Api.Kind.CaseClass, Api.Kind.Object))

      test(m"a field is a member of the type, and a companion method of the term"):
        ( entity(t"sample.Doc#title").let(_.definition.let(_.kind)),
          entity(t"sample.Doc.parse").let(_.definition.let(_.kind)) )

      . assert(_ == (Api.Kind.Val, Api.Kind.Def))

      test(m"an entity's page is at its path, with `~` after a type and `.` after a term"):
        (entity(t"sample.Doc#title").let(_.url), entity(t"sample.Doc.parse").let(_.url))

      . assert(_ == (t"/api/sample.Doc~title", t"/api/sample.Doc.parse"))

      test(m"a path leads back to its entity"):
        api match
          case api: Api => api.locate(t"sample.Doc~title").let(_.key)
          case _        => Unset

      . assert(_ == t"sample.Doc#title")

      test(m"a package is an entity whose members are its types and terms, declared once"):
        entity(t"sample").let: (p: Api.Entity) => (p.members.has(t"sample.Doc"), p.overloads.size)

      . assert(_ == (true, 0))

      test(m"a private member is not walked"):
        entity(t"sample.Doc#secret").present

      . assert(_ == false)

      test(m"a method's signature has its clauses and its result, and its overloads are kept"):
        entity(t"sample.Doc.parse").let: (e: Api.Entity) =>
          ( e.definition.let(_.clauses.size),
            e.definition.let(_.result.let(_.qualified)),
            e.overloads.size )

      . assert(_ == (1, t"sample.Doc", 1))

      test(m"a class's parents are its declared ones"):
        entity(t"sample.Doc").let(_.template.let(_.parents.map(_.qualified)))

      . assert(_ == List(t"sample.Shape[scala.Int]"))

      test(m"an extension keeps the clause it extends apart from its own"):
        entity(t"sample.Util.twice").let(_.definition).let: (d: Api.Declaration) =>
          (d.kind, d.group.size, d.clauses.size)

      . assert(_ == (Api.Kind.Extension, 1, 0))

      test(m"a type alias has its parameters and its right-hand side"):
        entity(t"sample.Util.Pair").let(_.template).let: (d: Api.Declaration) =>
          (d.kind, d.clauses.size, d.result.let(_.qualified))

      . assert(_ == (Api.Kind.Type, 1, t"(A, A)"))

      test(m"a method's parameters are members of its type form, one entity across its overloads"):
        ( entity(t"sample.Doc.parse#text").let(_.definition.let(_.kind)),
          entity(t"sample.Doc.parse#size").let(_.definition.let(_.result.let(_.qualified))),
          entity(t"sample.Doc.parse#text").let(_.overloads.size),
          entity(t"sample.Doc.parse#text").let(_.url) )

      . assert(_ == (Api.Kind.Param, t"scala.Int", 0, t"/api/sample.Doc.parse~text"))

      test(m"a class's, an alias's and an extension's parameters are entities, type and term"):
        ( entity(t"sample.Shape#A").let(_.template.let(_.kind)),
          entity(t"sample.Util.Pair#A").let(_.template.let(_.kind)),
          entity(t"sample.Util.twice#text").let(_.definition.let(_.kind)) )

      . assert(_ == (Api.Kind.TypeParam, Api.Kind.TypeParam, Api.Kind.Param))

      test(m"a case class's field is its constructor's parameter, and stays a field"):
        entity(t"sample.Doc#title").let: (e: Api.Entity) =>
          (e.definition.let(_.kind), e.overloads.size, e.isParameter)

      . assert(_ == (Api.Kind.Val, 0, false))

      test(m"a parameter is not in the navigation"):
        api match
          case api: Api =>
            def names(nodes: List[Block.TreeNode]): List[Text] =
              nodes.bind: (node: Block.TreeNode) =>
                List(Blocks.plain(node.label)) + names(node.children)

            names(ApiPages.tree(api, t"sample.Shape").roots).has(t"A")

          case _ => true

      . assert(_ == false)

      test(m"a given and a val are declarations of their kinds"):
        ( entity(t"sample.Util.given_Ordering_Doc").let(_.definition.let(_.kind)),
          entity(t"sample.Util.limit").let(_.definition.let(_.kind)) )

      . assert(_ == (Api.Kind.Given, Api.Kind.Val))

      test(m"a dependency jar is walked through the session's symbol table"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val jar: Text = t"$here/out/fuel/launcher/assembly.dest/out.jar"

        val result: Api | Throwable =
          Checking.session(classpath, Nil): (checker: Checker) =>
            checker.context.lay[Api | Throwable](Api.empty): context =>
              Inspection.classpath(List(jar), context)

        result match
          case api: Api =>
            val option: Optional[Api.Entity] = api(t"scala.Option")
            val mapped: Optional[Api.Entity] = api(t"scala.Option#map")

            ( api.entities.size > 1000,
              option.let(_.own),
              option.let(_.template.let(_.kind)),
              mapped.present,
              mapped.let(_.definition.let(_.result.let(_.qualified))) )

          case error: Throwable =>
            error.toString

      . assert(_ == (true, false, Api.Kind.Class, true, t"scala.Option[B]"))

      // A companion's members bring a few types stenography cannot render (a literal type, a
      // type with a companion's prefix): under one in a thousand, and under one in ten
      // thousand shown as `?`.
      test
        ( m"every type of a dependency renders, all but a few through stenography " +
          m"whole and almost none as `?`" ):

        val rendered: Int = Inspection.rendered.get

        ( rendered > 10000,
          Inspection.fallbacks.get * 1000 < rendered,
          Inspection.unrendered.get * 10000 < rendered )

      . assert(_ == (true, true, true))

      test
        ( m"a dependency jar is walked before a build, in a session on " +
          m"that jar under the project's options" ):

        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val jar: Text = t"$here/out/fuel/launcher/assembly.dest/out.jar"
        val out: Text = Text(java.nio.file.Files.createTempDirectory("fuel-probe").nn.toString)

        val path = java.nio.file.Path.of(s"$here/.pyrocosm/fuel/config.tel")
        val config: Text = Text(java.nio.file.Files.readString(path).nn)

        val options: List[Text] =
          config.cut(t"\n").filter(_.starts(t"scalac-option ")).map(_.skip(14).trim)

        val sizes: (Int, Int) =
          Checking.session(List(jar), options): (checker: Checker) =>
            def size(): Int = checker.context.lay(0): context =>
              Inspection.classpath(List(jar), context) match
                case api: Api => api.entities.size
                case _        => -1

            val before: Int = size()
            checker.build(Map(t"Probe.scala" -> sample), out)
            val after: Int = size()
            (before, after)

        // After a compile under these options the walk reads next to nothing, which is why
        // the compile step walks the dependencies first.
        (sizes(0) > 10000, sizes(1) < sizes(0))

      . assert(_ == (true, true))

      test(m"a symbolic name is encoded in its path, and decoded from it"):
        Api.unpath(Api.path(stenography.Designator(t"a.B"), t"+:")).name

      . assert(_ == t"+:")

      // A second source, compiled and walked as the first, with what applies to a type: an
      // extension in its companion, its package, elsewhere, in a base's companion, on a bound,
      // on a context bound a given of the type satisfies (one in the companion, one not, and
      // one inside a given with a body), on a context bound nothing satisfies, and unbounded.
      val extended: Text = t"""package extended

trait Shape[+A]:
  def area: Double

object Shape:
  extension (shape: Shape[?]) def describe: String = "shape"

case class Doc(title: String, size: Int = 0) extends Shape[Int]:
  def area: Double = 0.0

object Doc:
  extension (doc: Doc) def pages: Int = doc.size
  given Show[Doc] = _.title

object Util:
  given Ordering[Doc] = Ordering.by(_.title)

trait Show[T]:
  def show(value: T): String

trait Missing[T]

object Showing:
  extension [T: Show](value: T) def shown: String = summon[Show[T]].show(value)
  extension [T <: Shape[?]](value: T) def areaTwice: Double = value.area * 2
  extension [T](value: T) def same: T = value
  extension [T: Ordering](value: T) def ordered: T = value
  extension [T: Missing](value: T) def never: T = value

object Chars:
  given docText: Show[Doc] with
    def show(value: Doc): String = value.title
    extension (doc: Doc) def words: Int = 1

extension (doc: Doc) def titled: String = doc.title
"""

      lazy val decorated: Api =
        val output2: Text =
          Text(java.nio.file.Files.createTempDirectory("fuel-api2").nn.toString)

        Checking.session(classpath, Nil): (checker: Checker) =>
          checker.build(Map(t"Extended.scala" -> extended), output2)

        Inspection.walk(List(output2), Nil, entries) match
          case api: Api => Applicables.decorate(api)
          case _        => Api.empty

      def applicable(key: Text): List[(Text, Int, Int, Int, Optional[Text])] =
        decorated(key).lay(Nil: List[Api.Applicable])(_.extensions).map: (e: Api.Applicable) =>
          (e.key, e.tier, e.distance, e.reach, e.via)

      val showDoc: Api.Shape =
        Api.Shape.Named(t"extended.Show", List(Api.Shape.Named(t"extended.Doc")))

      test
        ( m"a given with a body is one given, providing its first parent, " +
          m"which its signature shows" ):

        decorated(t"extended.Chars.docText").let: (e: Api.Entity) =>
          e.definition.let: (d: Api.Declaration) =>
            (d.kind, d.modifiers, d.result.let(_.qualified), d.provides, e.overloads.size)

      . assert(_ == (Api.Kind.Given, Nil, t"extended.Show[extended.Doc]", showDoc, 0))

      val showT: Api.Shape = Api.Shape.Named(t"extended.Show", List(Api.Shape.Param(t"T")))

      test(m"an extension's receiver and requirements are shapes, a context bound a requirement"):
        decorated(t"extended.Showing.shown").let(_.definition).let: (d: Api.Declaration) =>
          (d.receiver, d.requires)

      . assert(_ == (Api.Shape.Param(t"T"), List(showT)))

      val shapeOfAny: Api.Shape =
        Api.Shape.Named(t"extended.Shape", List(Api.Shape.Named(t"scala.Any")))

      test(m"a bounded type parameter's receiver has its bound, and a class its bases"):
        ( decorated(t"extended.Showing.areaTwice").let(_.definition.let(_.receiver)),
          decorated(t"extended.Doc").let(_.bases) )

      . assert(_ == (Api.Shape.Param(t"T", shapeOfAny), List(t"extended.Shape")))

      val ranked: List[(Text, Int, Int, Int, Optional[Text])] =
        List
          ( (t"extended.Doc.pages", 0, 0, 0, Unset),
            (t"extended.titled", 1, 0, 1, Unset),
            (t"extended.Chars.docText.words", 2, 0, 2, t"extended.Chars.docText"),
            (t"extended.Shape.describe", 3, 1, 0, Unset),
            (t"extended.Showing.areaTwice", 4, 1, 2, Unset),
            (t"extended.Showing.shown", 5, 0, 2, t"extended.Doc.given_Show_Doc"),
            (t"extended.Showing.ordered", 5, 2, 2, t"extended.Util.given_Ordering_Doc") )

      test
        ( m"a type's extensions are ranked: companion, package, elsewhere, a base's, a bound's, " +
          m"then by a given, nearest first" ):

        applicable(t"extended.Doc")

      . assert(_ == ranked)

      test
        ( m"an extension needing a given the type has none of is not the type's; " +
          m"a base's and a bound's are" ):

        applicable(t"extended.Shape").map(_(0))

      . assert(_ == List(t"extended.Shape.describe", t"extended.Showing.areaTwice"))

      test
        ( m"an unbounded extension, and one needing a type class " +
          m"nothing provides, extend any value" ):

        applicable(t"scala.Any").map: (e: (Text, Int, Int, Int, Optional[Text])) => (e(0), e(1))

      . assert(_ == List((t"extended.Showing.never", 7), (t"extended.Showing.same", 7)))

      val givensOfDoc: List[Text] =
        List
          ( t"extended.Doc.given_Show_Doc",
            t"extended.Chars.docText",
            t"extended.Util.given_Ordering_Doc" )

      test(m"a type's givens are those for it, nearest first, and a type class's its instances"):
        (decorated(t"extended.Doc").let(_.givens), decorated(t"extended.Show").let(_.givens))

      . assert(_ == (givensOfDoc, List(t"extended.Chars.docText", t"extended.Doc.given_Show_Doc")))

      val companions: List[Text] =
        List(t"In the companion object", t"In the companion object of Shape")

      test(m"a type's page lists its extensions by source, then its givens, and points to `Any`"):
        decorated(t"extended.Doc").let: (doc: Api.Entity) =>
          val scope: Scope = Scope.of(doc, Nil)
          val resolver: Resolver = Resolver(decorated, scope)
          val page: List[Block] = ApiPages.page(decorated, doc, resolver, scope, Nil)

          val headings: List[Text] = page.bind:
            case Block.Heading(3, content, _) => List(Blocks.plain(content))
            case _                            => Nil

          val leads: List[Text] = page.bind:
            case Block.Paragraph(content) => List(Blocks.plain(content))
            case _                        => Nil

          ( headings,
            leads.filter(_.starts(t"In the companion")),
            leads.exists(_.starts(t"By import")),
            leads.exists(_.starts(t"The extensions of every value")) )

      . assert(_ == (List(t"Extensions", t"Givens"), companions, true, true))

      test(m"the extensions an object declares are grouped under the clause they extend"):
        api0(t"sample.Util").let: (util: Api.Entity) =>
          val scope: Scope = Scope.of(util, Nil)
          val page: List[Block] = ApiPages.page(api0, util, Resolver(api0, scope), scope, Nil)

          page.bind:
            case code: Block.Code => code.lines.map(_.tokens.map(_.text).join)
            case _                => Nil

        . or(Nil)

      . assert(_.has(t"extension (text: String)"))

      val sampleMarks: List[Inline] =
        List(Inline.Import(t"/api/sample", t"sample"), Inline.Bookmark(t"/api/sample", t"sample"))

      test
        ( m"a package's heading carries its import toggle and " +
          m"bookmark; a type's, its bookmark alone" ):

        def marks(key: Text): List[Inline] =
          api0(key).let: (entity: Api.Entity) =>
            val scope: Scope = Scope.of(entity, Nil)
            val page: List[Block] = ApiPages.page(api0, entity, Resolver(api0, scope), scope, Nil)

            page.bind:
              case Block.Heading(1, content, _) =>
                content.filter:
                  case Inline.Import(_, _) | Inline.Bookmark(_, _) => true
                  case _                                           => false

              case _ =>
                Nil

          . or(Nil)

        (marks(t"sample"), marks(t"sample.Doc"))

      . assert(_ == (sampleMarks, List(Inline.Bookmark(t"/api/sample.Doc", t"sample.Doc"))))

    suite(m"Search"):
      import Query.{Form, Name, TypeQuery}
      import Api.Kind

      // An unparseable query is the empty one, which no assertion below expects.
      def query(text: Text): Query = Query.read(text) match
        case query: Query => query
        case _            => Query()

      def fails(text: Text): Boolean = Query.read(text) match
        case error: Query.Error => true
        case _                  => false

      def tq(name: Text, args: TypeQuery*): TypeQuery = TypeQuery(name, List(args*))

      // The sample with its dependencies, as the daemon models a project: the standard library
      // gives `String` its alias and `List` its subtypes.
      val full: Api =
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val jar: Text = t"$here/out/fuel/launcher/assembly.dest/out.jar"

        val dependencies: Api | Throwable =
          Checking.session(classpath, Nil): (checker: Checker) =>
            checker.context.lay[Api | Throwable](Api.empty): context =>
              Inspection.classpath(List(jar), context)

        dependencies match
          case api: Api => api.merge(api0)
          case _        => api0

      // The keys of the sample's hits, in order, each once.
      def keys(text: Text): List[Text] =
        val hits = full.search(query(text)).hits.stdlib
        List.from(hits.map(_.entity.key).filter(_.starts(t"sample.")).distinct)

      def count(text: Text): Int =
        full.search(query(text)).hits.stdlib.count(_.entity.key.starts(t"sample."))

      suite(m"Parsing"):
        test(m"+List"):
          query(t"+List")

        . assert(_ == Query(results = List(tq(t"List"))))

        test(m"+List[Int]"):
          query(t"+List[Int]")

        . assert(_ == Query(results = List(tq(t"List", tq(t"Int")))))

        test(m"-Either[Int, String]"):
          query(t"-Either[Int, String]")

        . assert(_ == Query(parameters = List(tq(t"Either", tq(t"Int"), tq(t"String")))))

        val listOfInt: Query =
          Query(results = List(tq(t"List", tq(t"Int"))), parameters = List(tq(t"String")))

        test(m"+List[Int] -String"):
          query(t"+List[Int] -String")

        . assert(_ == listOfInt)

        val tuple: Query =
          Query
            ( results = List(tq(t"scala.Tuple3", tq(t"Int"), tq(t"Double"), tq(t"String"))),
              parameters = List(tq(t"String")) )

        test(m"a tuple is a TupleN"):
          query(t"+(Int, Double, String) -String")

        . assert(_ == tuple)

        val functions: Query =
          Query
            ( results = List(tq(t"List", tq(t"scala.Function1", tq(t"String"), tq(t"Int")))),
              parameters = List(tq(t"scala.ContextFunction1", tq(t"Int"), tq(t"Int"))) )

        test(m"a function is a FunctionN, and a context function a ContextFunctionN"):
          query(t"+List[String => Int] -(Int ?=> Int)")

        . assert(_ == functions)

        val fuzzy: Query =
          Query
            ( names = List(Name(t"testing", false)),
              results = List(tq(t"String")),
              parameters = List(tq(t"Int")) )

        test(m"a bare word is a fuzzy name"):
          query(t"testing +String -Int")

        . assert(_ == fuzzy)

        val typeMember: Query =
          Query
            ( form = Form.Type,
              names = List(Name(t"testing", false)),
              results = List(tq(t"String")),
              parameters = List(tq(t"Int")) )

        test(m"# is a type member"):
          query(t"#testing +String -Int")

        . assert(_ == typeMember)

        val termMember: Query = Query(form = Form.Term, names = List(Name(t"testing", false)))

        test(m". is a term member, glued or not"):
          (query(t".testing"), query(t". testing"))

        . assert(_ == (termMember, termMember))

        val packaged: Query =
          Query
            ( packages = List(t"my.package.name"),
              form = Form.Type,
              names = List(Name(t"Testing", false)) )

        test(m"/ is a package, dotted, spaced or not"):
          (query(t"/my.package.name #Testing"), query(t"/ mypackage"))

        . assert(_ == (packaged, Query(packages = List(t"mypackage"))))

        val trailing: Query =
          Query
            ( packages = List(t"my.package.name"),
              form = Form.Type,
              names = List(Name(t"Testing", false)),
              results = List(tq(t"List", tq(t"scala.Function1", tq(t"String"), tq(t"Int")))),
              parameters = List(tq(t"Int")) )

        test(m"trailing space is nothing"):
          query(t"/my.package.name #Testing +List[String => Int] -Int ")

        . assert(_ == trailing)

        test(m"backticks are an exact name"):
          query(t"`parse`")

        . assert(_ == Query(names = List(Name(t"parse", true))))

        test(m"backticks make a symbol a name"):
          query(t"`+` `::`")

        . assert(_ == Query(names = List(Name(t"+", true), Name(t"::", true))))

        test(m"backticks make a keyword a name"):
          query(t"`type`")

        . assert(_ == Query(names = List(Name(t"type", true))))

        test(m"a kind keyword"):
          query(t"def parse")

        . assert(_ == Query(kinds = sci.Set(Kind.Def), names = List(Name(t"parse", false))))

        test(m"case class is one keyword; case alone is a case"):
          (query(t"case class").kinds, query(t"case").kinds)

        . assert(_ == (sci.Set(Kind.CaseClass), sci.Set(Kind.Case)))

        test(m"a modifier keyword"):
          query(t"inline def")

        . assert(_ == Query(kinds = sci.Set(Kind.Def), modifiers = sci.Set(t"inline")))

        test(m"? is given and ~ is extension"):
          (query(t"?").kinds, query(t"given").kinds, query(t"~").kinds)

        . assert(_ == (sci.Set(Kind.Given), sci.Set(Kind.Given), sci.Set(Kind.Extension)))

        test(m"type is a type or a type parameter"):
          query(t"type").kinds

        . assert(_ == sci.Set(Kind.Type, Kind.TypeParam))

        test(m"private warns"):
          query(t"private").warnings.size

        . assert(_ == 1)

        test(m"an unterminated backtick fails"):
          fails(t"`parse")

        . assert(_ == true)

        test(m"an unterminated bracket fails"):
          fails(t"+List[Int")

        . assert(_ == true)

        test(m"a bare symbol fails"):
          fails(t"::")

        . assert(_ == true)

        test(m"a sigil without a type fails"):
          fails(t"+")

        . assert(_ == true)

      suite(m"Lattice"):
        val lattice: Lattice = full.lattice

        def sub(a: Text, b: Text): Boolean =
          lattice.isSubtype(lattice.id(a).or(-1), lattice.id(b).or(-1))

        test(m"Doc is a Shape"):
          sub(t"sample.Doc", t"sample.Shape")

        . assert(_ == true)

        test(m"Doc is Any and AnyRef, not AnyVal"):
          ( sub(t"sample.Doc", t"scala.Any"),
            sub(t"sample.Doc", t"scala.AnyRef"),
            sub(t"sample.Doc", t"scala.AnyVal") )

        . assert(_ == (true, true, false))

        test(m"Int is AnyVal, not AnyRef"):
          (sub(t"scala.Int", t"scala.AnyVal"), sub(t"scala.Int", t"scala.AnyRef"))

        . assert(_ == (true, false))

        test(m"Nothing is beneath everything; Null beneath references only"):
          ( sub(t"scala.Nothing", t"sample.Doc"),
            sub(t"scala.Null", t"sample.Doc"),
            sub(t"scala.Null", t"scala.Int") )

        . assert(_ == (true, true, false))

        test
          ( m"a base's distance is its place in the linearisation, " +
            m"a universal type's beyond them" ):

          def distance(a: Text, b: Text): Int =
            lattice.distance(lattice.id(a).or(-1), lattice.id(b).or(-1))

          ( distance(t"sample.Doc", t"sample.Doc"),
            distance(t"sample.Doc", t"sample.Shape"),
            distance(t"sample.Doc", t"scala.AnyRef") > 1,
            distance(t"sample.Shape", t"sample.Doc") )

        . assert(_ == (0, 1, true, -1))

        test(m"String is a CharSequence"):
          sub(t"java.lang.String", t"java.lang.CharSequence")

        . assert(_ == true)

        test(m"Predef.String expands to java.lang.String"):
          full.expand(Api.Shape.Named(t"scala.Predef.String"))

        . assert(_ == Api.Shape.Named(t"java.lang.String"))

        val int: Api.Shape = Api.Shape.Named(t"scala.Int")

        test(m"Pair[Int] expands to a tuple"):
          full.expand(Api.Shape.Named(t"sample.Util.Pair", List(int)))

        . assert(_ == Api.Shape.Named(t"scala.Tuple2", List(int, int)))

        val shapeId: Int = lattice.id(t"sample.Shape").or(-1)

        test(m"a type's written parents, in order, and the types which name it"):
          val doc: Int = lattice.id(t"sample.Doc").or(-1)
          val shape: Int = lattice.id(t"sample.Shape").or(-1)

          ( lattice.parents(doc),
            lattice.children(shape).contains(doc),
            lattice.primaryParent(doc),
            lattice.parents(shape) )

        . assert(_ == (scala.List(shapeId), true, shapeId, scala.Nil))

      suite(m"Hierarchy"):
        import Hierarchy.{Edge, Graph, Node, Role}

        def node(key: Text, role: Role): Node =
          val name: Text = Text(key.s.substring(key.s.lastIndexOf('.') + 1).nn)
          val prefix: Text = Text(key.s.substring(0, (key.s.length - name.s.length - 1).max(0)).nn)
          Node(key, name, prefix, key, role)

        def laid(graph: Graph): Hierarchy.Layout =
          Hierarchy.layout(graph).or(Hierarchy.Layout(Nil, Nil, Nil, 0, 0))

        def box(layout: Hierarchy.Layout, key: Text): Hierarchy.Box =
          layout.boxes.stdlib.find(_.node.key == key).get

        val chain: Graph =
          val nodes: List[Node] =
            List
              ( node(t"sample.A", Role.Current),
                node(t"sample.B", Role.Ancestor),
                node(t"scala.Any", Role.Universal) )

          val edges: List[Edge] =
            List(Edge(t"sample.A", t"sample.B"), Edge(t"sample.B", t"scala.Any"))

          Graph(nodes, edges, t"sample.A")

        chainSvg = Hierarchy.layout(chain).let(Hierarchy.svg(_)).or(t"")

        test(m"a chain is one column, Any on top"):
          val layout: Hierarchy.Layout = laid(chain)
          val a: Hierarchy.Box = box(layout, t"sample.A")
          val b: Hierarchy.Box = box(layout, t"sample.B")
          val any: Hierarchy.Box = box(layout, t"scala.Any")

          ( layout.boxes.size,
            any.y < b.y && b.y < a.y,
            a.centre == b.centre && b.centre == any.centre )

        . assert(_ == (3, true, true))

        val diamond: Graph =
          val nodes: List[Node] =
            List
              ( node(t"d.D", Role.Current),
                node(t"d.B", Role.Ancestor),
                node(t"d.C", Role.Ancestor),
                node(t"d.A", Role.Ancestor) )

          val edges: List[Edge] =
            List
              ( Edge(t"d.D", t"d.B"),
                Edge(t"d.D", t"d.C"),
                Edge(t"d.B", t"d.A"),
                Edge(t"d.C", t"d.A") )

          Graph(nodes, edges, t"d.D")

        test(m"a diamond's middle shares a layer without overlap, and its ends align"):
          val layout: Hierarchy.Layout = laid(diamond)
          val a: Hierarchy.Box = box(layout, t"d.A")
          val b: Hierarchy.Box = box(layout, t"d.B")
          val c: Hierarchy.Box = box(layout, t"d.C")
          val d: Hierarchy.Box = box(layout, t"d.D")
          val apart: Boolean = b.x + b.w <= c.x || c.x + c.w <= b.x
          (b.y == c.y, apart, (a.centre - d.centre).abs <= 1)

        . assert(_ == (true, true, true))

        val traits: Graph =
          val nodes: List[Node] =
            List
              ( node(t"t.T", Role.Current),
                node(t"t.P", Role.Ancestor),
                node(t"t.Q", Role.Ancestor),
                node(t"t.R", Role.Ancestor) )

          Graph(nodes, List(t"t.P", t"t.Q", t"t.R").map(Edge(t"t.T", _)), t"t.T")

        test(m"a type with three parents sits beneath their middle"):
          val layout: Hierarchy.Layout = laid(traits)

          val parents: scala.List[Hierarchy.Box] =
            scala.List(t"t.P", t"t.Q", t"t.R").map(box(layout, _))

          val mean: Int = parents.map(_.centre).sum/3

          ( parents.map(_.y).distinct.size,
            (box(layout, t"t.T").centre - mean).abs <= 1,
            layout.stems.size )

        . assert(_ == (1, true, 3))

        val skipping: Graph =
          val nodes: List[Node] =
            List
              ( node(t"s.A", Role.Current),
                node(t"s.B", Role.Ancestor),
                node(t"s.C", Role.Ancestor) )

          val edges: List[Edge] =
            List(Edge(t"s.A", t"s.C"), Edge(t"s.A", t"s.B"), Edge(t"s.B", t"s.C"))

          Graph(nodes, edges, t"s.A")

        test(m"a long edge passes its intermediate layer clear of the box there"):
          val layout: Hierarchy.Layout = laid(skipping)
          val long: Hierarchy.Route = layout.routes.stdlib.find(_.edge == Edge(t"s.A", t"s.C")).get
          val b: Hierarchy.Box = box(layout, t"s.B")
          val through: Int = long.points(2)._1
          (long.points.length >= 5, through < b.x || through > b.x + b.w)

        . assert(_ == (true, true))

        // A parent with five children, hand-built, for peers and the note of more.
        val family: Api =
          def entity(name: Text, parent: Optional[Text]): Api.Entity =
            val shapes: List[Api.Shape] = parent.let(Api.Shape.Named(_)).let(List(_)).or(Nil)
            val template: Api.Declaration = Api.Declaration(Api.Kind.Class, parentShapes = shapes)
            val top: stenography.Designator = stenography.Designator.Top(t"p")
            Api.Entity(top, name, true, template = template, bases = parent.let(List(_)).or(Nil))

          val children: scala.List[Api.Entity] =
            scala.List(t"C1", t"C2", t"C3", t"C4", t"C5").map(entity(_, t"p.P"))

          val all: scala.List[Api.Entity] =
            entity(t"P", Unset) :: entity(t"Lone", Unset) :: children

          Api(all.map { e => e.key -> e }.toMap)

        val sibling: Optional[Graph] = family(t"p.C1").let: (e: Api.Entity) =>
          Hierarchy.of(family, e, Scope.document(Nil))

        test(m"three peers are drawn, and the rest noted"):
          sibling.let: (g: Graph) =>
            ( g.nodes.stdlib.count(_.role == Role.Peer),
              g.nodes.stdlib.filter(_.role == Role.More).map(_.name) )

        . assert(_ == (3, scala.List(t"…and 1 more")))

        test(m"the parent's children are peers under it, and the drawing says so"):
          sibling.let(Hierarchy.alt(_))

        . assert(_ == t"C1 extends P; a sibling of C2, C3, C4 and …and 1 more under P.")

        test(m"the SVG links each box in-app, by a relative path, with nothing honeycomb rejects"):
          val svg: Text = sibling.let(Hierarchy.layout(_)).let(Hierarchy.svg(_)).or(t"")
          val digits: Boolean = svg.s.matches("(?s).*\\s[A-Za-z-]*[0-9][A-Za-z0-9-]*=\".*")

          ( svg.contains(t"pyro-locate"),
            svg.contains(t"""href="p.C2""""),
            svg.contains(t"<line"),
            svg.contains(t"xlink"),
            digits,
            svg.contains(t"font-family: var(--pyro-font-mono)") )

        . assert(_ == (true, true, false, false, false, true))

        test(m"a name with angle brackets is escaped"):
          val nodes: List[Node] =
            List(node(t"x.Foo<Bar>", Role.Current), node(t"x.Base", Role.Ancestor))

          val graph: Graph = Graph(nodes, List(Edge(t"x.Foo<Bar>", t"x.Base")), t"x.Foo<Bar>")
          Hierarchy.layout(graph).let(Hierarchy.svg(_)).or(t"").contains(t"Foo&lt;Bar&gt;")

        . assert(_ == true)

        def figures(key: Text): List[Block.Figure] =
          api0(key).let: (entity: Api.Entity) =>
            val scope: Scope = Scope.of(entity, Nil)

            ApiPages.page(api0, entity, Resolver(api0, scope), scope, Nil).bind:
              case figure: Block.Figure => List(figure)
              case _                    => Nil

          . or(Nil)

        test(m"Doc's page draws its hierarchy after its signature, naming its parent as written"):
          api0(t"sample.Doc").let: (entity: Api.Entity) =>
            val scope: Scope = Scope.of(entity, Nil)
            val page: List[Block] = ApiPages.page(api0, entity, Resolver(api0, scope), scope, Nil)
            val at: Int = page.stdlib.indexWhere { case _: Block.Figure => true; case _ => false }

            val after: Boolean = page.stdlib.lift(at - 1) match
              case Some(_: Block.Code) => true
              case _                   => false

            (after, figures(t"sample.Doc").map { f => Blocks.plain(f.figure.alt) })

        . assert(_ == (true, List(t"Doc extends Shape[Int].")))

        test(m"Shape's page names the type which extends it"):
          figures(t"sample.Shape").map: f => Blocks.plain(f.figure.alt)

        . assert(_ == List(t"Shape; extended by Doc."))

        test(m"an object, and a class with nothing but AnyRef above it, draw nothing"):
          val lone: Optional[Graph] = family(t"p.Lone").let: (e: Api.Entity) =>
            Hierarchy.of(family, e, Scope.document(Nil))

          (figures(t"sample.Util").size, lone.present)

        . assert(_ == (0, false))

      suite(m"Rendering"):
        import stenography.{Designator, Syntax}

        def named(name: Text, tpe: Text, contextual: Boolean = false): Syntax =
          Syntax.Named(contextual, name, Syntax.Simple(Designator(tpe)))

        def clause(items: Syntax*): Syntax = Syntax.Sequence('(', List(items*))
        def ret(tpe: Text): Syntax = Syntax.Simple(Designator(tpe))
        val scope: Scope = Scope.document(Nil)

        def shown(name: Text, declaration: Api.Declaration, width: Int): List[Text] =
          Signatures.block(name, declaration, Api.empty, scope, width).lines
          . map(_.tokens.map(_.text).join)

        val simple: Api.Declaration =
          Api.Declaration(Api.Kind.Def, clauses = List(clause()), result = ret(t"ReturnType"))

        test(m"a short signature is one line"):
          shown(t"short", simple, 88)

        . assert(_ == List(t"def short(): ReturnType"))

        test(m"a long name breaks only before its result, after the hard space"):
          shown(t"thisIsALongerMethodName", simple, 30)

        . assert(_ == List(t"def thisIsALongerMethodName()", t":   ReturnType"))

        val firstClause: Syntax = clause(named(t"first", t"First"))

        val multi: Api.Declaration =
          val second: Syntax = clause(named(t"second", t"Second"), named(t"third", t"Third"))
          val clauses: List[Syntax] = List(firstClause, second)
          Api.Declaration(Api.Kind.Def, clauses = clauses, result = ret(t"ReturnType"))

        val multiShown: List[Text] =
          List
            ( t"def multiParams(first: First)",
              t"  ( second: Second, third: Third )",
              t":   ReturnType" )

        test(m"a second clause goes on its own line, padded within"):
          shown(t"multiParams", multi, 40)

        . assert(_ == multiShown)

        val wide: Api.Declaration =
          val rest: Syntax =
            clause
              ( named(t"second", t"Second"),
                named(t"third", t"Third"),
                named(t"fourth", t"Fourth"),
                named(t"fifth", t"Fifth") )

          val clauses: List[Syntax] = List(firstClause, rest)
          Api.Declaration(Api.Kind.Def, clauses = clauses, result = ret(t"ReturnType"))

        val wideShown: List[Text] =
          List
            ( t"def multiParams(first: First)",
              t"  ( second: Second,",
              t"    third:  Third,",
              t"    fourth: Fourth,",
              t"    fifth:  Fifth )",
              t":   ReturnType" )

        test(m"a clause too wide for a line puts each parameter on one, its types aligned"):
          shown(t"multiParams", wide, 40)

        . assert(_ == wideShown)

        val usingWide: List[Text] = List(t"def f(x: X)(using alpha: Alpha, beta: Beta): R")

        val usingNarrow: List[Text] =
          List(t"def f(x: X)", t"  ( using alpha: Alpha,", t"          beta:  Beta )", t":   R")

        test(m"a using clause says using once, its later parameters aligned beneath the first"):
          val contextual: Syntax =
            clause(named(t"alpha", t"Alpha", true), named(t"beta", t"Beta", true))

          val clauses: List[Syntax] = List(clause(named(t"x", t"X")), contextual)

          val decl: Api.Declaration =
            Api.Declaration(Api.Kind.Def, clauses = clauses, result = ret(t"R"))

          (shown(t"f", decl, 80), shown(t"f", decl, 20))

        . assert(_ == (usingWide, usingNarrow))

        test(m"a nameless using parameter is its type alone"):
          val clauses: List[Syntax] = List(clause(named(t"", t"Tactic", true)))

          val decl: Api.Declaration =
            Api.Declaration(Api.Kind.Def, clauses = clauses, result = ret(t"R"))

          shown(t"f", decl, 80)

        . assert(_ == List(t"def f(using Tactic): R"))

        val cell: Text =
          t"(first: First)\n( second: Second,\n  third:  Third,\n" +
            t"  fourth: Fourth,\n  fifth:  Fifth )\n" +
            t":   ReturnType"

        test(m"a table cell holds the clauses and result on lines of their own"):
          Signatures.cell(wide, Api.empty, scope, 40).tokens.map(_.text).join

        . assert(_ == cell)

        test(m"no signature in the model carries a compiler-made parameter name"):
          full.entities.values.toList.filter(_.own).flatMap(_.declarations.stdlib)
          . flatMap(_.clauses.stdlib)
          . map(_.qualified)
          . count(_.contains(t"$$"))

        . assert(_ == 0)

        test(m"a type with a companion is one row, and an export is marked as one"):
          val aliasType: Api.Declaration =
            Api.Declaration(Api.Kind.Type, result = ret(t"q.Text"), exported = true)

          val aliasTerm: Api.Declaration =
            Api.Declaration(Api.Kind.Val, exported = true, result = ret(t"q.Text"))

          val exported: Api.Entity =
            Api.Entity
              ( Designator.Top(t"p"),
                t"Text",
                true,
                definition = aliasTerm,
                template = aliasType,
                alias = Designator(t"q.Text") )

          val paired: Api.Entity =
            Api.Entity
              ( Designator.Top(t"p"),
                t"Doc",
                true,
                definition = Api.Declaration(Api.Kind.Val),
                template = Api.Declaration(Api.Kind.CaseClass),
                overloads = List(Api.Declaration(Api.Kind.Object)) )

          val api: Api = Api(sci.Map(exported.key -> exported, paired.key -> paired))

          def kinds(entity: Api.Entity): List[Text] =
            ApiPages.rows(entity, true, api, scope, Resolver(api, scope)).map: row =>
              Blocks.plain(row.cells.stdlib.head.content)

          (kinds(exported), kinds(paired))

        . assert(_ == (List(t"export"), List(t"case class")))

        test(m"member tables share fixed column widths"):
          ApiPages.table(t"Name", t"Signature", List(Block.Row(Nil))).bind:
            case Block.Table(columns, _, _) => columns.map(_.sizing)
            case _                          => Nil

        . assert(_ == List(14, 22, 64).map(Block.Sizing.Fixed(_)))

        test(m"a hierarchy's boxes are square-cornered and the drawing keeps its own width"):
          val svg: Text = chainSvg
          (svg.contains(t"rx="), svg.contains(t"width: min("), svg.contains(t"px; height: auto"))

        . assert(_ == (false, false, true))

      suite(m"Results"):
        test(m"+Shape finds what returns a Doc"):
          keys(t"+Shape")

        . assert(_ == List(t"sample.Doc.parse"))

        test(m"+Shape[Int] finds a Doc through its parent; +Shape[String] does not"):
          (keys(t"+Shape[Int]"), keys(t"+Shape[String]"))

        . assert(_ == (List(t"sample.Doc.parse"), Nil))

        test(m"+Doc"):
          keys(t"+Doc")

        . assert(_ == List(t"sample.Doc.parse"))

        test(m"+Double"):
          keys(t"+Double")

        . assert(_ == List(t"sample.Doc#area", t"sample.Shape#area"))

        val values: List[Text] =
          List(t"sample.Doc#area", t"sample.Doc#size", t"sample.Shape#area", t"sample.Util.limit")

        test(m"+AnyVal finds the value-typed"):
          keys(t"+AnyVal")

        . assert(_ == values)

        test(m"+Any finds everything with a result"):
          keys(t"+Any").size

        . assert(_ >= 8)

        test(m"+String is java.lang.String"):
          (keys(t"+String"), keys(t"+java.lang.String"))

        . assert: (a, b) =>
            a == b && a == List(t"sample.Doc#shout", t"sample.Doc#title", t"sample.Util.twice")

        test(m"+Ordering[Doc] finds the given"):
          keys(t"+Ordering[Doc]")

        . assert(_ == List(t"sample.Util.given_Ordering_Doc"))

        test(m"a type nothing is called says so"):
          full.search(query(t"+Nonesuch99")).notices.size

        . assert(_ == 1)

        test(m"+List is every List"):
          full.search(query(t"+List")).hits.size > 100

        . assert(_ == true)

      suite(m"Parameters"):
        test(m"-String finds what takes a String, a receiver among them"):
          keys(t"-String")

        . assert(_ == List(t"sample.Doc", t"sample.Doc.parse", t"sample.Util.twice"))

        test(m"-CharSequence finds nothing taking a String, a String being beneath it"):
          keys(t"-CharSequence")

        . assert(_ == Nil)

        test(m"-Int finds the overload with a size"):
          (keys(t"-Int"), count(t"-Int"))

        . assert(_ == (List(t"sample.Doc", t"sample.Doc.parse"), 2))

        test(m"-Nothing finds every parameterised declaration; -Any only what takes Any"):
          (keys(t"-Nothing"), keys(t"-Any"))

        . assert(_ == (List(t"sample.Doc", t"sample.Doc.parse", t"sample.Util.twice"), Nil))

        test(m"-Shape finds nothing in the sample, but something takes Any"):
          (keys(t"-Shape"), full.search(query(t"-Shape")).hits.size > 0)

        . assert(_ == (Nil, true))

      suite(m"Names and criteria"):
        test(m"an exact name"):
          (keys(t"`parse`"), count(t"`parse`"))

        . assert(_ == (List(t"sample.Doc.parse"), 2))

        test(m"a fuzzy name, at radius one"):
          keys(t"pars")

        . assert(_ == List(t"sample.Doc.parse"))

        test(m"a name differing in case is exact"):
          full.search(query(t"Parse")).hits.stdlib.filter(_.entity.key.starts(t"sample."))
          . forall(_.exact)

        . assert(_ == true)

        test(m"inline finds nothing; private finds nothing and warns"):
          (count(t"inline"), count(t"private"), full.search(query(t"private")).notices.size)

        . assert(_ == (0, 0, 1))

        test(m"/sample.Util -String"):
          keys(t"/sample.Util -String")

        . assert(_ == List(t"sample.Util.twice"))

        test(m"# and . choose the type or the term"):
          ( full.search(query(t"/sample #Doc")).hits.map(_.declaration.kind),
            full.search(query(t"/sample .Doc")).hits.map(_.declaration.kind) )

        . assert(_ == (List(Kind.CaseClass), List(Kind.Object)))

        test(m"? finds the given and ~ the extension"):
          (keys(t"/sample ?"), keys(t"/sample ~"))

        . assert(_ == (List(t"sample.Util.given_Ordering_Doc"), List(t"sample.Util.twice")))

        test(m"+Doc -Int is one overload"):
          count(t"+Doc -Int")

        . assert(_ == 1)

        test(m"type Pair"):
          keys(t"type Pair")

        . assert(_ == List(t"sample.Util.Pair"))

        test(m"val"):
          keys(t"/sample val")

        . assert(_ == List(t"sample.Doc#size", t"sample.Doc#title", t"sample.Util.limit"))

        test(m"the search builds quickly and widely"):
          val start: Long = java.lang.System.nanoTime
          val count: Int = full.search(query(t"+Any")).hits.size
          ((java.lang.System.nanoTime - start)/1000000L < 2000L, count > 1000)

        . assert(_ == (true, true))

    suite(m"Reporting"):
      val text: Text = t"# Title\n\nSee [the guide](other.md) here.\n\n```scala\nval x = 1\n"

      def codes(blocks: List[Block]): List[Block.Code] = blocks.bind:
        case code: Block.Code => List(code)
        case _                => Nil

      test(m"a plain excerpt numbers the line and its neighbours, underlining the span"):
        val code: Block.Code =
          codes(Excerpts.plain(text, 3, Block.Note.Style.Erroneous, 5, 11)).stdlib.head

        (code.start, code.lines.size, code.notes.map { n => (n.line, n.start, n.end) })

      . assert(_ == (2, 3, List((1, 4, 15))))

      test(m"an excerpt at a target underlines its occurrence, or the whole line without one"):
        val found: Block.Code =
          codes(Excerpts.plainAt(text, 3, t"other.md", Block.Note.Style.Caution)).stdlib.head

        val whole: Block.Code =
          codes(Excerpts.plainAt(text, 3, t"nowhere", Block.Note.Style.Caution)).stdlib.head

        (found.notes.map { n => (n.start, n.end) }, whole.notes.map { n => (n.start, n.end) })

      . assert(_ == (List((16, 24)), List((0, 31))))

      test(m"a Scala excerpt is highlighted and placed by line and column"):
        val source: Text = t"object A:\n  val x: Int = \"no\"\n"
        val (place, blocks) = Excerpts.scala(source, 25, 29, Block.Note.Style.Erroneous)
        val lines = codes(blocks).stdlib.head.lines.stdlib
        (place, lines.exists(_.tokens.stdlib.exists(_.accent == Token.Accent.Keyword)))

      . assert(_ == ((2, 16), true))

      // A fence left open is told apart from one which simply ends the file by the glossary's
      // definitions, which follow the text and which an open fence swallows.
      val glossary: Glossary =
        Glossary(List(Glossary.Term(t"guide", t"A guide.", Unset, t"glossary.tel", 1)))

      def parse(glossary: Glossary): Doc =
        Doc.parse
          ( t"guide.md",
            text,
            Checker.tokenized,
            false,
            Unset,
            Linker.none,
            Unset,
            t"",
            t"docs",
            glossary )

      test(m"a document keeps its text, and an unclosed fence shows its line"):
        val doc: Doc = parse(glossary)

        val problems = doc.problems.bind:
          case Block.Diagnostic(_, location, message, excerpt, _) =>
            val paragraphs: List[Inline] = message.bind:
              case Block.Paragraph(content) => content
              case _                        => Nil

            List((location.let(_.text), Blocks.plain(paragraphs), codes(excerpt).map(_.start)))

          case _ =>
            Nil

        (doc.text == text, problems)

      . assert(_ == (true, List((t"guide.md:5", t"This fence is not closed.", List(4)))))

      test(m"a broken link's problem shows the line it is on, the link underlined"):
        val doc: Doc = parse(Glossary.none)
        val corpus: Corpus = Corpus(List(Corpus.Section(t"docs", t"/docs", List(doc), Nil)))

        Links.check(corpus, Api.empty, Glossary.none).bind:
          case Block.Diagnostic(_, _, _, excerpt, _) =>
            codes(excerpt).bind(_.notes.map { n => (n.start, n.end) })

          case _ =>
            Nil

      . assert(_ == List((16, 24)))

      test
        ( m"progress is the weight done over the weight planned, " +
          m"and a phase may be finished whole" ):

        import stdios.fileDescriptorStdio
        import termcapDefinitions.basicTermcap
        val progress: Progress = Progress(Unset, 80)
        progress.plan(Progress.Phase.Documents, 2)
        progress.plan(Progress.Phase.Links, 1)
        val start: Double = progress.fraction
        progress.step(Progress.Phase.Documents)
        val half: Double = progress.fraction
        progress.step(Progress.Phase.Documents)
        progress.finish(Progress.Phase.Links)
        (start, (half*100).round, progress.fraction, progress.streamed)

      . assert(_ == (0.0, 43L, 1.0, true))

    suite(m"Overlays"):
      val overlay: Text = t"""base sample
import scala

info  a sample package
detail
    All about _sample_.

entry .Doc   a document
  detail
      A `Doc` has a *title*.
  import scala.util
  entry #title   its title
  entry #shout   its title, loudly
  entry #area    nothing much
  entry .parse   parses a document
    entry #text   the text to parse
  entry #whisper  its title, quietly
    until 0.2.0  sample.Doc#shout

entry .Util   utilities
  hidden
entry .Shape   a shape
entry .nowhere   nothing of the sort
"""

      lazy val read: Overlay | Block.Diagnostic = Overlay.read(t"doc/api/sample.tel", overlay)

      def api: Api = Tests.this.api0

      lazy val merged: Optional[(Api, List[Block])] = read match
        case overlay: Overlay => Overlay.merge(api, List(overlay))
        case _                => Unset

      def plain(message: List[Block]): Text =
        val inlines: List[Inline] = message.bind:
          case Block.Paragraph(content) => content
          case _                        => Nil

        Blocks.plain(inlines)

      def problems(tone: Tone): List[Text] =
        merged.lay(Nil: List[Block]) { (m: (Api, List[Block])) => m(1) }.bind:
          case Block.Diagnostic(tone0, location, message, _, _) if tone0 == tone =>
            val path: Text = location.let(_.path).or(t"?")
            List(t"$path:${location.let(_.line).or(0).toString}: ${plain(message)}")

          case _ =>
            Nil

      test(m"an overlay is read: its base, its package's info and detail, and its imports"):
        read match
          case overlay: Overlay =>
            (overlay.base, overlay.doc.info, overlay.doc.detail.let(_.trim), overlay.doc.imports)

          case other =>
            other

      . assert(_ == (t"sample", t"a sample package", t"All about _sample_.", List(t"scala")))

      val docEntry =
        ( t"Doc",
          false,
          t"a document",
          t"A `Doc` has a *title*.",
          8,
          List(t"scala", t"scala.util"),
          5 )

      test
        ( m"an entry has its name, kind, info (inline), detail (source), line " +
          m"and inherited imports" ):

        read match
          case overlay: Overlay =>
            overlay.entries.head.let: (e: Overlay.Entry) =>
              ( e.name,
                e.isType,
                e.doc.info,
                e.doc.detail.let(_.trim),
                e.line,
                e.doc.imports,
                e.entries.size )

          case other =>
            other

      . assert(_ == docEntry)

      test(m"a leaf entry's detail is its source atom"):
        read match
          case overlay: Overlay => overlay.entries.at(Sec).let(_.entries.head.let(_.doc.detail))
          case other            => other

      . assert(_ == Unset)

      test(m"an entry's children are placed by their parent's kind"):
        merged.let(_(0)(t"sample.Doc#title").let(_.doc.let(_.info)))

      . assert(_ == t"its title")

      val nowhere: Text =
        t"doc/api/sample.tel:23: `.nowhere` is documented here, " +
          t"but no such member of `sample` is declared."

      test(m"an entry for a member nothing declares is a failure at its line"):
        problems(Tone.Failure)

      . assert(_ == List(nowhere))

      test(m"a parameter is documented as a member of its method's type form"):
        merged.let(_(0)(t"sample.Doc.parse#text").let(_.doc.let(_.info)))

      . assert(_ == t"the text to parse")

      def failures(text: Text): List[Text] = Overlay.read(t"x.tel", text) match
        case overlay: Overlay =>
          val lines: List[Text] = overlay.problems.bind:
            case Block.Diagnostic(_, location, message, _, _) =>
              List(t"${location.let(_.line).or(0).toString}: ${plain(message)}")

            case _ =>
              Nil

          List.from(lines.stdlib.sortBy(_.cut(t":").head.s.toInt))

        case diagnostic: Block.Diagnostic => List(t"unread")

      val formatFailures: List[Text] =
        List
          ( t"2: `domain` is not part of an overlay: An overlay may hold `base`, `import`, " +
            t"`info`, `detail`, `refer`, `hidden`, `until`, `entry`.",
            t"4: `info` is given more than once here.",
            t"6: This entry's info is already given on its line.",
            t"7: `detial` is not part of an overlay: An entry may hold `import`, `info`, " +
              t"`detail`, `refer`, `hidden`, `until`, `entry`.",
            t"10: `#title` already has an entry here.",
            t"11: An `entry` must name the member it documents, as `.name` or `#Name`." )

      test(m"an overlay's format is checked, each failure at its line"):
        failures(t"""base sample
domain sample
info  one
info  two
entry .Doc   a document
  info  again
  detial
      oops
  entry #title  its title
  entry #title  its title again
  entry
""")

      . assert(_ == formatFailures)

      val tooLong: Text =
        t"4: This info is 141 characters long, and may be at most 140: say the rest in a `detail`."

      test(m"an info may be 140 characters as it reads, and no more"):
        val fits: Text = t"`${"a".repeat(140).nn.tt}`"
        val over: Text = "a".repeat(141).nn.tt
        failures(t"base sample\nentry .Doc  $fits\nentry .Shape\n  info $over\n")

      . assert(_ == List(tooLong))

      test(m"an entry marked `until` need not exist, and has a page which is removed"):
        merged.let(_(0)(t"sample.Doc#whisper")).let: (e: Api.Entity) =>
          (e.removed, e.doc.let(_.until.let(_.replacement)))

      . assert(_ == (true, t"sample.Doc#shout"))

      val undocumented: List[Text] =
        List
          ( t"Sample.scala:6: `sample.Doc#size` is not documented: add `entry #size` under " +
            t"`sample.Doc` in doc/api/sample.tel.",
            t"Sample.scala:4: `sample.Shape#area` is not documented: add `entry #area` under " +
              t"`sample.Shape` in doc/api/sample.tel." )

      test
        ( m"an undocumented public member is a caution where it is declared; " +
          m"a hidden entity's members are not" ):

        problems(Tone.Warning)

      . assert(_ == undocumented)

    suite(m"Resolution"):
      def resolver(scope: Scope): Resolver = Resolver(Tests.this.api0, scope)

      def linked(scope: Scope, reference: Text): Optional[Text] =
        resolver(scope).link(reference) match
          case Inline.Destination.Local(path, _) => path
          case _                                 => Unset

      test(m"a qualified name resolves as written"):
        linked(Scope.document(Nil), t"sample.Doc")

      . assert(_ == t"/api/sample.Doc")

      test(m"a leaf resolves through an import"):
        linked(Scope.document(List(t"sample")), t"Doc")

      . assert(_ == t"/api/sample.Doc")

      test(m"a leaf does not resolve without a scope"):
        linked(Scope.document(Nil), t"Doc")

      . assert(_ == Unset)

      val members: (Text, Text, Text, Text) =
        ( t"/api/sample.Doc.parse",
          t"/api/sample.Doc~title",
          t"/api/sample.Doc.parse",
          t"/api/sample.Doc~shout" )

      test(m"a member resolves through its enclosing entity, and by Amok's `.`/`#` forms"):
        val doc: Api.Entity = Tests.this.api0(t"sample.Doc").or(Api.Entity(Unset, t"?"))
        val scope: Scope = Scope.of(doc, Nil)

        ( linked(scope, t"parse"),
          linked(scope, t"#title"),
          linked(scope, t".parse"),
          linked(scope, t"Doc#shout") )

      . assert(_ == members)

      test(m"a fragment's imports are learned"):
        val r: Resolver = resolver(Scope.document(Nil))
        r.learn(List(t"import sample.*"))

        r.link(t"Util") match
          case Inline.Destination.Local(path, _) => path
          case _                                 => Unset

      . assert(_ == t"/api/sample.Util")

      test(m"a typed token links only when it is the name its type names"):
        val r: Resolver = resolver(Scope.document(Nil))

        val doc: stenography.Syntax =
          stenography.Syntax.Simple(stenography.Designator(t"sample.Doc"))

        (r.link(t"Doc", doc).present, r.link(t"x", doc).present)

      . assert(_ == (true, false))

      val docPage: Inline.Destination = Inline.Destination.Local(t"/api/sample.Doc")

      test(m"a signature is tokens, its designators linked, and written in full under an import"):
        val api: Api = Tests.this.api0

        val parse: Api.Declaration =
          api(t"sample.Doc.parse").let(_.definition).or(Api.Declaration(Api.Kind.Def))

        val tokens = Signatures.declaration(t"parse", parse, api, Scope.document(List(t"sample")))
        (tokens.map(_.text).join(t""), tokens.seek(_.text == t"Doc").let(_.link))

      . assert(_ == (t"def parse(text: String): sample.Doc", docPage))

      test(m"a signature is elided by the enclosing scope alone"):
        val api: Api = Tests.this.api0

        val parse: Api.Declaration =
          api(t"sample.Doc.parse").let(_.definition).or(Api.Declaration(Api.Kind.Def))

        api(t"sample.Doc").let: (doc: Api.Entity) =>
          Signatures.declaration(t"parse", parse, api, Scope.of(doc, Nil)).map(_.text).join(t"")

      . assert(_ == t"def parse(text: String): Doc")

      val purview: List[Purview.Entry] =
        List
          ( Purview.Entry(t"/api/sample", t"sample"),
            Purview.Entry(t"/api/scala.Predef", t"scala.Predef") )

      test(m"the project's imports are a reader's starting purview, one entry per page"):
        Scope.purview(List(t"sample", t"scala.Predef")).imports

      . assert(_ == purview)

      test(m"the standard scope follows the project's -Yimports and -Yno-predef options"):
        ( Scope.standardOf(Nil),
          Scope.standardOf(List(t"-Yno-predef")),
          Scope.standardOf(List(t"-Yimports:java.lang,proscenium,soundness")),
          Scope.standardOf(List(t"-Yno-imports")) )

      . assert: result =>
          result ==
            ( Scope.standard,
              List(Designator(t"scala"), Designator(t"java.lang")),
              List(Designator(t"java.lang"), Designator(t"proscenium"), Designator(t"soundness")),
              Nil )

      val qualifiedSample: List[(Text, Optional[Text], Boolean)] =
        List
          ( (t"sample", t"/api/sample", false),
            (t".", Unset, true),
            (t"Doc", t"/api/sample.Doc", true) )

      def described(tokens: List[Token]): List[(Text, Optional[Text], Boolean)] =
        tokens.map: (token: Token) =>
          val path: Optional[Text] = token.link match
            case Inline.Destination.Local(path, _) => path
            case _                                 => Unset

          (token.text, path, token.chained)

      test(m"a qualified designator in a signature links each segment, chained to the one before"):
        val designator = stenography.Designator(t"sample.Doc")
        described(Signatures.designator(designator, Tests.this.api0, Scope.document(Nil)))

      . assert(_ == qualifiedSample)

      test(m"a qualified reference in prose is a link for each segment"):
        described(resolver(Scope.document(Nil)).tokens(t"sample.Doc"))

      . assert(_ == qualifiedSample)

      test(m"a leaf, a member of the page's own entity, and what does not resolve are one token"):
        val doc: Api.Entity = Tests.this.api0(t"sample.Doc").or(Api.Entity(Unset, t"?"))
        val imported: Resolver = resolver(Scope.document(List(t"sample")))
        val own: Resolver = resolver(Scope.of(doc, Nil))

        ( imported.tokens(t"Doc").size,
          own.tokens(t"#title").size,
          imported.tokens(t"nowhere.Nothing").map(_.link.present) )

      . assert(_ == (1, 1, List(false)))

      test(m"in code, a linked name and the linked member which follows it are chained"):
        def linked(text: Text, path: Text): Token =
          Token(text, Token.Accent.Term, Unset, Inline.Destination.Local(path))

        val dot: Token = Token.plain(t".")
        val space: Token = Token.plain(t" ")
        val com: List[Token] = List(linked(t"com", t"/api/com"), dot)
        val example: List[Token] = List(linked(t"example", t"/api/com.example"), dot)
        val foo: List[Token] = List(linked(t"Foo", t"/api/com.example~Foo"), space)
        val unrelated: Token = linked(t"b", t"/api/elsewhere.b")
        val elsewhere: List[Token] = List(linked(t"a", t"/api/a"), dot, unrelated)
        val tokens: List[Token] = com + example + foo + elsewhere

        Linker.chain(tokens).map(_.chained)

      . assert(_ == List(false, true, true, true, true, false, false, false, false))

      val samplePage: Inline.Destination = Inline.Destination.Local(t"/api/sample")
      val sampleLink: Inline = Inline.Link(samplePage, Inline.text(t"sample"))
      val dot: Inline = Inline.Toned(Tone.Muted, Inline.text(t"."))
      val heading: Block = Block.Paragraph(List(sampleLink, dot))

      test(m"a page has its heading, signature and member rows anchored by name"):
        val api: Api = Tests.this.api0
        val doc: Api.Entity = api(t"sample.Doc").or(Api.Entity(Unset, t"?"))
        val scope: Scope = Scope.of(doc, Nil)
        val page = ApiPages.page(api, doc, resolver(scope), scope, Nil)

        val rows: List[Text] = page.bind:
          case Block.Table(_, rows, _) => rows.bind(_.id.let(List(_)).or(Nil))
          case _                       => Nil

        (page.head, rows.has(t"title"), rows.has(t"parse"))

      . assert(_ == (heading, true, true))

      test(m"the tree links each node to its page, open along the way to the current one"):
        val tree = ApiPages.tree(Tests.this.api0, t"sample.Doc")

        tree.roots.head.let: (n: Block.TreeNode) =>
          (n.destination, n.open, n.children.map(_.label).map(Blocks.plain))

      . assert(_ == (samplePage, true, List(t"Doc", t"Shape", t"Util")))

    suite(m"References"):
      def resolver(page: Optional[Text] = Unset): Resolver =
        Resolver(Tests.this.api0, Scope.document(List(t"sample")), page)

      def doc(text: Text, linker: Linker = resolver(t"/doc/doc/a.md")): Doc =
        Doc.parse(t"a.md", text, Checker.tokenized, false, Unset, linker, section = t"doc")

      // The first inline code of the first paragraph: its text, accent and link.
      def token(doc: Doc): Optional[(Text, pyrocosm.Token.Accent, Optional[Inline.Destination])] =
        def snippet(inlines: List[Inline]): Optional[pyrocosm.Token] = inlines match
          case Inline.Snippet(_, token :: _) :: _ => token
          case _ :: tail                          => snippet(tail)
          case _                                  => Unset

        val paragraph = doc.blocks.seek:
          case Block.Paragraph(_) => true
          case _                  => false

        paragraph match
          case Block.Paragraph(inlines) =>
            snippet(inlines).let: (t: pyrocosm.Token) => (t.text, t.accent, t.link)

          case _ =>
            Unset

      def symbols(doc: Doc): List[Text] = doc.references.bind: (reference: Linkage) =>
        reference.target match
          case Linkage.Target.Sigil(text)    => List(text)
          case _                             => Nil

      val docPage: Inline.Destination = Inline.Destination.Local(t"/api/sample.Doc")

      test(m"`#Name` names the type, shown without its sigil in the type's accent"):
        token(doc(t"See `#Doc`.\n"))

      . assert(_ == (t"Doc", pyrocosm.Token.Accent.Typal, docPage))

      val utilLinks: List[Optional[Inline.Destination]] =
        List
          ( Inline.Destination.Local(t"/api/sample.Util"),
            Unset,
            Inline.Destination.Local(t"/api/sample.Util.limit") )

      test
        ( m"`.name` names the term, through its path, each segment " +
          m"linked and the last in its kind's accent" ):

        doc(t"See `.Util.limit`.\n").blocks match
          case Block.Paragraph(_ :: Inline.Snippet(_, tokens) :: _) :: _ =>
            (tokens.map(_.text).join(t""), tokens.map(_.link), tokens.reverse.head.let(_.accent))

          case _ =>
            Unset

      . assert(_ == (t"Util.limit", utilLinks, pyrocosm.Token.Accent.Term))

      test
        ( m"a sigil which nothing of its kind answers is shown as " +
          m"written, unlinked, and recorded" ):

        val shape: Doc = doc(t"See `.Shape` and `#Util`.\n")
        (token(shape).let(_(0)), token(shape).let(_(2)), symbols(shape))

      . assert(_ == (t".Shape", Unset, List(t".Shape", t"#Util")))

      test(m"a path or a selector after a dot is code as written, not a reference"):
        val paths: Doc = doc(t"See `.pyrocosm/fuel/config.tel`, `.sN:checked`, `..` and `.`.\n")
        (token(paths).let(_(0)), paths.references)

      . assert(_ == (t".pyrocosm/fuel/config.tel", Nil))

      test
        ( m"inline code without a sigil which resolves to nothing stays " +
          m"plain, and is no reference" ):

        val plain: Doc = doc(t"See `nothing`.\n")
        (token(plain).let(_(0)), token(plain).let(_(2)), plain.references)

      . assert(_ == (t"nothing", Unset, Nil))

      val setup: Inline.Destination = Inline.Destination.Local(t"/doc/doc/a.md", t"setup")

      test(m"a name a fence defined links to the fence, by its id"):
        val text: Text = t"```scala fuel\nid setup\n##\nval base: Int = 1\n```\n\nSee `base`.\n"
        val d: Doc = doc(text)

        val id: Optional[Text] = d.blocks match
          case Block.Code(_, _, _, _, _, id) :: _ => id
          case _                                  => Unset

        (token(d).let(_(2)), id, d.anchors)

      . assert(_ == (setup, t"setup", List(t"setup")))

      test(m"a fence without an id is anchored by its number"):
        token(doc(t"```scala\nval base: Int = 1\n```\n\nSee `base`.\n")).let(_(2))

      . assert(_ == Inline.Destination.Local(t"/doc/doc/a.md", t"fragment-1"))

      test
        ( m"a fence's definition shadows an entity of the same name, " +
          m"and the latest definition wins" ):

        val text: Text =
          t"```scala\nval Doc: Int = 1\n```\n```scala\nval Doc: Int = 2\n```\n\nSee `Doc`.\n"

        token(doc(text)).let(_(2))

      . assert(_ == Inline.Destination.Local(t"/doc/doc/a.md", t"fragment-2"))

      test(m"a sigil picks a fence's definition by its kind"):
        val text: Text =
          t"```scala\nval x: Int = 1\nclass Y\n```\n\nSee `#x`, `.x`, `#Y` and `.Y`.\n"

        symbols(doc(text))

      . assert(_ == List(t"#x", t".Y"))

      test(m"without a page a fence's definition links nowhere, and the API answers instead"):
        token(doc(t"```scala\nval Doc: Int = 1\n```\n\nSee `Doc`.\n", resolver())).let(_(2))

      . assert(_ == Inline.Destination.Local(t"/api/sample.Doc"))

    suite(m"Anchors and links"):
      def doc(path: Text, text: Text, section: Text = t"doc"): Doc =
        Doc.parse(path, text, Checker.tokenized, false, Unset, Linker.none, section = section)

      def links(doc: Doc): List[Inline.Destination] =
        def inlines(content: List[Inline]): List[Inline.Destination] = content.bind:
          case Inline.Link(destination, inner) => destination :: inlines(inner)
          case Inline.Annotated(inner, _)      => inlines(inner)
          case _                               => Nil

        doc.blocks.bind:
          case Block.Paragraph(content) => inlines(content)
          case _                        => Nil

      def ids(doc: Doc): List[Text] = doc.blocks.bind:
        case Block.Heading(_, _, id) => id.let(List(_)).or(Nil)
        case _                       => Nil

      def lines(problems: List[Block]): List[Optional[Int]] = problems.map:
        case Block.Diagnostic(_, location, _, _, _) => location.let(_.line)
        case _                                      => Unset

      val slugs: List[Text] = List(t"getting-started", t"set-up--run", t"set-up--run-1")

      test(m"a heading is anchored by its slug, numbered when repeated"):
        val d: Doc = doc(t"a.md", t"# Getting started\n\n## Set-up & run\n\n## Set-up & run\n")
        (ids(d), d.anchors)

      . assert(_ == (slugs, slugs))

      val fragments: List[Inline.Destination] =
        List
          ( Inline.Destination.Local(t"/doc/doc/a.md", t"set-up"),
            Inline.Destination.Local(t"/doc/doc/a.md", t"nope") )

      test(m"a `#fragment` link leads to this page, and must name an anchor on it"):
        val d: Doc = doc(t"a.md", t"## Set-up\n\n[here](#set-up)\n\n[there](#nope)\n")
        val corpus: Corpus = Corpus(List(Corpus.Section(t"doc", t"/r", List(d))))
        (links(d), lines(Links.check(corpus, Api.empty, Glossary.none)))

      . assert(_ == (fragments, List(5)))

      val upward: List[Inline.Destination] =
        List(Inline.Destination.Local(t"/doc/doc/x.md"), Inline.Destination.External(t"../../x.md"))

      test(m"a relative link is resolved against the document's directory, within its section"):
        val a: Doc = doc(t"a.md", t"[b](guide/b.md)\n")
        val b: Doc = doc(t"guide/b.md", t"[up](../x.md)\n\n[out](../../x.md)\n")
        val x: Doc = doc(t"x.md", t"# X\n")
        val corpus: Corpus = Corpus(List(Corpus.Section(t"doc", t"/r", List(a, b, x))))
        (links(a), links(b), lines(Links.check(corpus, Api.empty, Glossary.none)))

      . assert(_ == (List(Inline.Destination.Local(t"/doc/doc/guide/b.md")), upward, List(3)))

      test(m"a site link must name an entity, a member of it, or a glossary term"):
        val text: Text =
          t"[a](/api/sample.Doc#parse)\n\n[b](/api/sample.Nope)\n\n" +
            t"[c](/api/sample.Doc#nope)\n\n[d](/glossary#monad)\n"

        val d: Doc = doc(t"a.md", text)
        val corpus: Corpus = Corpus(List(Corpus.Section(t"doc", t"/r", List(d))))
        val glossary: Glossary = Glossary(List(Glossary.Term(t"monad", t"x", Unset, t"g.tel", 1)))

        ( lines(Links.check(corpus, Tests.this.api0, glossary)),
          lines(Links.check(corpus, Tests.this.api0, Glossary.none)) )

      . assert(_ == (List(3, 5), List(3, 5, 7)))

      test(m"a URL is recorded, and not fetched unless asked"):
        val d: Doc = doc(t"a.md", t"[s](https://example.com/)\n")
        val corpus: Corpus = Corpus(List(Corpus.Section(t"doc", t"/r", List(d))))
        (links(d), d.references.size, Links.check(corpus, Api.empty, Glossary.none).size)

      . assert(_ == (List(Inline.Destination.External(t"https://example.com/")), 1, 0))

      test(m"a document's URL encodes each segment"):
        Doc.url(t"doc modules", t"a b/c.md")

      . assert(_ == t"/doc/doc%20modules/a%20b/c.md")

    suite(m"Glossary"):
      val tel: Text = t"""define monad
  link https://example.com/monads
  definition
      A *monad* is a monoid in the category of endofunctors.

define type class
  definition  A set of operations over a type, given apart from it.
"""

      val glossary: Glossary = Glossary.read(t"doc/glossary.tel", tel) match
        case glossary: Glossary => glossary
        case _                  => Glossary.none

      def doc(text: Text, glossary: Glossary = glossary): Doc =
        Doc.parse
          ( t"a.md",
            text,
            Checker.tokenized,
            false,
            Unset,
            Linker.none,
            section = t"doc",
            glossary = glossary )

      // The first paragraph's second inline, after the text which leads to the term.
      def term(doc: Doc): Optional[Inline] = doc.blocks match
        case Block.Paragraph(_ :: inline :: _) :: _ => inline
        case _                                      => Unset

      def failures(problems: List[Block]): List[Optional[(Tone, Optional[Int])]] = problems.map:
        case Block.Diagnostic(tone, location, _, _, _) => (tone, location.let(_.line))
        case _                                         => Unset

      val monadText: Text = t"A *monad* is a monoid in the category of endofunctors."
      val typeClassText: Text = t"A set of operations over a type, given apart from it."

      val terms =
        List
          ( (t"monad", t"monad", 1, t"https://example.com/monads", monadText),
            (t"type class", t"type-class", 6, Unset, typeClassText) )

      test(m"a glossary's terms are read, with their lines, links, slugs and definitions"):
        glossary.terms.map: (term: Glossary.Term) =>
          (term.name, term.slug, term.line, term.link, term.definition.trim)

      . assert(_ == terms)

      test(m"a term defined twice, or which cannot be a label, is a failure"):
        val monad: Glossary.Term = Glossary.Term(t"Monad", t"y", Unset, t"other.tel", 4)
        val unlabelled: Glossary.Term = Glossary.Term(t"a[b]", t"z", Unset, t"other.tel", 9)
        val again: Glossary = Glossary(List(monad, unlabelled))
        val (merged, problems) = Glossary.merge(List(glossary, again))
        (merged.terms.size, failures(problems))

      . assert(_ == (2, List((Tone.Failure, 4), (Tone.Failure, 9))))

      val monadPage: Inline.Destination = Inline.Destination.External(t"https://example.com/monads")
      val monadLink: Inline = Inline.Link(monadPage, Inline.text(t"monad"))

      val monadDefinition: List[Inline] =
        List
          ( Inline.Textual(t"A "),
            Inline.Emphasis(Inline.text(t"monad")),
            Inline.Textual(t" is a monoid in the category of endofunctors.") )

      val typeClassPage: Inline.Destination = Inline.Destination.Local(t"/glossary", t"type-class")
      val typeClassLink: Inline = Inline.Link(typeClassPage, Inline.text(t"Type Class"))
      val typeClassDefinition: List[Inline] = Inline.text(typeClassText)
      val annotatedMonad: Inline = Inline.Annotated(List(monadLink), monadDefinition)
      val annotatedTypeClass: Inline = Inline.Annotated(List(typeClassLink), typeClassDefinition)

      test(m"a bracketed term is annotated with its definition and leads to its link, in any case"):
        (term(doc(t"A [monad] here.\n")), term(doc(t"A [Type Class] here.\n")))

      . assert(_ == (annotatedMonad, annotatedTypeClass))

      val mine: Inline =
        Inline.Link(Inline.Destination.External(t"https://mine/"), Inline.text(t"monad"))

      test(m"the author's own reference definition wins, and brackets around no term stay text"):
        val plain: Doc = doc(t"A [nothing] here.\n")

        val text: Optional[Text] = plain.blocks match
          case Block.Paragraph(content) :: _ => Blocks.plain(content)
          case _                             => Unset

        (term(doc(t"A [monad] here.\n\n[monad]: https://mine/\n")), text, plain.references)

      . assert(_ == (mine, t"A [nothing] here.", Nil))

      test(m"the glossary page anchors each term by its slug"):
        glossary.page(Linker.none).bind:
          case Block.Heading(2, _, id) => id.let(List(_)).or(Nil)
          case _                       => Nil

      . assert(_ == List(t"monad", t"type-class"))

      test(m"a fence left open is a failure at its line, and the document is read as written"):
        val d: Doc = doc(t"Text.\n\n```scala\nval x = 1\n")
        (failures(d.problems), d.blocks.size)

      . assert(_ == (List((Tone.Failure, 3)), 2))

    suite(m"Sections"):
      test(m"a section is named by its directory's last segment"):
        Compilation.name(t"/home/me/project/doc/modules/")

      . assert(_ == t"modules")
