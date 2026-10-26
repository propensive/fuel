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

// Excluded from the umbrella: `Compilation`, anthology's, which would outrank fuel's own.
import soundness.{Compilation as _, Language as _, Token as _, *}

import dysasymptotics.{linearAccess, linearSize}
import scala.jdk.CollectionConverters.*
import pyrocosm.{Block, Inline, Language, TerminalRenderer, Token, Tone}
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
          case _ :: _ :: Block.Code(Language.Scala, line :: _, _, _, _) :: _ => line.tokens.size > 1
          case _                                                             => false

      . assert(_ == true)

      test(m"a fence in an unknown language is plain, under that language"):
        Doc.parse(t"doc/intro.md", intro).blocks match
          case _ :: _ :: _ :: Block.Code(language, lines, _, _, _) :: _ =>
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
          case _ :: Block.Code(_, _, _, start, _) :: _ => start
          case _                                       => Unset

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
            case Block.Code(_, _, note :: _, _, _) :: Block.Diagnostic(_, _, _, _, _) :: _ =>
              (note.line, note.style)

            case _ =>
              (-1, Block.Note.Style.Param)

      . assert(_ == (1, Block.Note.Style.Erroneous))

    suite(m"Animation"):
      val animatedFence: Text =
        t"```scala fuel\nid steps\nframe\n  code\n      val x: Int = 1\nframe\n  code\n      val x: Int = 1\n" +
          t"      val y: Int = x + 1\nframe\n  code val z: Int = 2\n##\n```\n"

      test(m"a frame's code is its `code` child, indented beneath it or on the line"):
        Preamble.parse(t"frame\n  code\n      val x = 1\n      val y = 2\nframe\n  code val z = 3\nloop\n") match
          case p: Preamble => (p.frames.map(_.code), p.autoplay, p.loop)
          case _           => (Nil: List[Optional[Text]], true, true)

      . assert(_ == (List(t"val x = 1\nval y = 2", t"val z = 3"), false, true))

      test(m"a frame's label, hold, at and language are read"):
        Preamble.parse(t"frame\n  code\n      val x = 1\n  label the *first* step\n  hold 2\n  at 12.5\n  language java\n") match
          case p: Preamble =>
            p.frames.prim.let: (frame: Preamble.Frame) =>
              val emphasised: Boolean = frame.label.exists { case Inline.Emphasis(_) => true; case _ => false }
              (Blocks.plain(frame.label), emphasised, frame.hold, frame.at, frame.language)

          case _ =>
            Unset

      . assert(_ == (t"the first step", true, 2.0, 12.5, t"java"))

      test(m"a frame's marks are its own, refined beneath them by line and occurrence"):
        Preamble.parse(t"frame\n  code\n      val x = x\n  highlight x  the variable\n    line 1\n    occurrence 2\n") match
          case p: Preamble =>
            val own: Optional[List[(Block.Note.Style, Optional[Text], Optional[Int], Int)]] =
              p.frames.prim.let(_.marks.map { (mark: Preamble.Mark) => (mark.style, mark.caption, mark.selection.line, mark.selection.occurrence) })

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
        val text: Text = t"# T\n\n```scala fuel\nframe\n  code\n      val x = 1\nframe\n    val y = 2\n##\n```\n"
        val doc = Doc.parse(t"doc/anim.md", text)

        val problem = doc.problems match
          case Block.Diagnostic(Tone.Failure, location, Block.Paragraph(Inline.Textual(message) :: _) :: _, _, _) :: Nil =>
            (location.let(_.line), message.starts(t"Frame 2"))

          case _ =>
            (Unset, false)

        val frames = doc.blocks match
          case _ :: Block.Code(_, _, _, _, animation) :: _ => animation.let(_.slides.size)
          case _                                           => Unset

        (problem, frames)

      . assert(_ == ((7, true), 1))

      test(m"a frame's details reach the animation, and a block-level mark every frame it matches"):
        val text: Text =
          t"```scala fuel\nhighlight x\nframe\n  code\n      val x: Int = 1\nframe\n  label a *second* value\n  hold 2\n  at 12.5\n" +
            t"  highlight y  the new one\n  code\n      val y: Int = 2\n##\n```\n"

        Doc.parse(t"doc/anim.md", text).blocks match
          case Block.Code(_, _, notes, _, animation) :: Nil =>
            animation.let: (animation: Block.Animation) =>
              val second: Block.Slide = animation.slides.skip(1).prim.or(Block.Slide(-1))
              (notes.size, animation.slides.map(_.notes.size), Blocks.plain(second.label), second.hold, second.at)

          case _ =>
            Unset

      . assert(_ == (1, List(1, 1), t"a second value", 2.0, 12.5))

      test(m"a java frame is highlighted without the checker, in its own language"):
        val text: Text =
          t"```scala fuel\nframe\n  code\n      val x: Int = 1\nframe\n  language java\n  code\n      int x = 1;\n##\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          val doc = Doc.parse(t"doc/anim.md", text, checker)

          doc.blocks match
            case Block.Code(_, _, _, _, animation) :: Nil =>
              animation.let: (animation: Block.Animation) =>
                (doc.problems.size, animation.slides.skip(1).prim.let(_.language), animation.lines(1).prim.let(_.size > 1))

            case _ =>
              Unset

      . assert(_ == (0, Language.Java, true))

      test(m"a java fence with a preamble is read by the preamble embedding"):
        Doc.parse(t"doc/j.md", t"```java fuel\nid j\n##\nint x = 1;\n```\n").blocks match
          case Block.Code(language, line :: _, _, _, _) :: Nil => (language, line.tokens.size > 1)
          case _                                               => Unset

      . assert(_ == (Language.Java, true))

      test(m"a fragment follows the last scala frame of an animation"):
        val first: Text =
          t"```scala fuel\nid setup\nframe\n  code\n      val base: Int = 1\nframe\n  language java\n  code\n      int base = 1;\n##\n```\n"

        val second: Text = t"```scala fuel\nfollows setup\n##\nval n: Int = base + 1\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", first + second, checker).problems

      . assert(_ == Nil)

      test(m"an animated fence becomes one block whose lines are the first frame"):
        Doc.parse(t"doc/anim.md", animatedFence).blocks match
          case Block.Code(_, lines, _, _, animation) :: Nil =>
            (lines.size, animation.let(_.slides.size), animation.let(_.lines(1).size))

          case _ =>
            (0, Unset, Unset)

      . assert(_ == (1, 3, 2))

      test(m"a surviving token keeps its atom across frames"):
        Doc.parse(t"doc/anim.md", animatedFence).blocks match
          case Block.Code(_, _, _, _, animation) :: Nil =>
            animation.let: (animation: Block.Animation) =>
              val first: List[Text] = animation.placements.filter(_.slide == 0).map(_.atom)
              val second: List[Text] = animation.placements.filter(_.slide == 1).map(_.atom)
              first.all(second.has(_))

          case _ =>
            Unset

      . assert(_ == true)

      test(m"a frame that does not compile is a problem naming the frame"):
        val fence: Text =
          t"```scala fuel\nframe\n  code\n      val x: Int = 1\nframe\n  code\n      val x: Int = \"oops\"\n##\n```\n"

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
        problems(t"```scala\nval a: Int = 1\n```\n```scala fuel\nrestart\n##\nval b: Int = a\n```\n").map(line(_))
      . assert(_ == List(7))

      test(m"a name defined again hides the earlier definition"):
        problems(t"```scala\nval a: Int = 1\n```\n```scala\nval a: Int = 2\n```\n```scala\nval b: Int = a + 1\n```\n")
      . assert(_ == Nil)

      test(m"an earlier fragment's imports are in scope for a later one"):
        problems(t"```scala\nimport scala.collection.mutable.ArrayBuffer\nval a: Int = 1\n```\n```scala\nval buffer: ArrayBuffer[Int] = ArrayBuffer(a)\n```\n")
      . assert(_ == Nil)

      test(m"a fixture is compiled before the first fragment"):
        problems(t"```scala\nfixed + 1\n```\n", (t"doc/fixtures/check.scala", t"val fixed: Int = 1"))
      . assert(_ == Nil)

      test(m"a fixture which does not compile is a problem at its own path"):
        problems(t"```scala\nval n: Int = 1\n```\n", (t"doc/fixtures/check.scala", t"val fixed: Int = \"x\"")) match
          case Block.Diagnostic(_, location, _, _, _) :: Nil => (location.let(_.path), location.let(_.line))
          case _                                             => Unset
      . assert(_ == (t"doc/fixtures/check.scala", 1))

      test(m"an `errors show` fragment does not join the session"):
        val text: Text =
          t"```scala\nval a: Int = 1\n```\n```scala fuel\nerrors show\n##\nval c: Int = \"x\"\n```\n```scala\nval d: Int = c\n```\n"

        problems(text).map(line(_))
      . assert(_ == List(10))

      test(m"a parsed fragment passes a type error and fails a syntax error"):
        ( problems(t"```scala fuel\nparse\n##\nval n: Int = \"x\"\n```\n").size,
          problems(t"```scala fuel\nparse\n##\nval = 1\n```\n").size )
      . assert(_ == (0, 1))

      test(m"a compiled fragment reports a type error, and passes a feature flag"):
        ( problems(t"```scala fuel\ncompile\n##\nval n: Int = \"x\"\n```\n").size,
          problems(t"```scala fuel\ncompile\nfeature experimental.captureChecking\n##\nval n: Int = 1\n```\n").size )
      . assert(_ == (1, 0))

      test(m"a line marked as not compiling must not compile, and is noted"):
        val text: Text = t"```scala\nval n: Int = 1\nval m: Int = \"x\" // does not compile\n```\n"

        val note: Optional[(Int, Block.Note.Style)] = blocks(text) match
          case Block.Code(_, _, note :: _, _, _) :: _ => (note.line, note.style)
          case _                                      => Unset

        (problems(text).size, note)
      . assert(_ == (0, (1, Block.Note.Style.Erroneous)))

      test(m"a line marked as not compiling which compiles is a problem at its line"):
        problems(t"```scala\nval n: Int = 1\nval m: Int = 2 // does not compile\n```\n").map(line(_))
      . assert(_ == List(3))

      test(m"a diagnostic in a session is at its line and column in the file"):
        problems(t"```scala\nval a: Int = 1\n```\n```scala\nval b: Int = \"oops\"\n```\n") match
          case Block.Diagnostic(_, location, _, _, _) :: Nil => (location.let(_.line), location.let(_.column))
          case _                                             => Unset
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
            case Block.Code(_, _, _, _, animation: Block.Animation) => List(animation.slides.size)
            case _                                                  => Nil

        (all.size, animated)

      . assert(_ == (4, List(3)))

      test(m"markdown on a slide is blocks, and code on a slide is not numbered"):
        val all: List[Block.Leaf] = leaves(talk)

        val first: Optional[Text] = all.prim.let(_.content.prim).let:
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

        def check(code: Text, mode: Preamble.Mode, context: Optional[harlequin.Scala.Context]): SourceCode =
          checks += 1
          inner.check(code, mode, context)

        def enter(name: Text, unit: Text): Boolean = inner.enter(name, unit)
        def build(sources: Map[Text, Text], output: Text): List[Notice] = inner.build(sources, output)
        def context: Optional[dotty.tools.dotc.core.Contexts.Context] = inner.context
        def compiles: Boolean = inner.compiles

      def document(b: Text, prose: Text = t""): Text =
        t"# Fragments\n\n${prose}Some prose.\n\n```scala\nval a = 1\n```\n\nMore prose.\n\n```scala\n$b\n```\n\n```scala\nval c = 3\n```\n"

      // The checks a second compile makes, after a first of `first`, and the two compiles'
      // documents.
      def twice(first: Text, second: Text, session: Boolean, path: Text): (Int, Doc, Doc) =
        Checking.session(classpath, Nil): (checker: Checker) =>
          val cache: Cache = Cache.Transient()
          val counting = Counting(checker)
          val one: Doc = Doc.parse(path, first, counting, session, Unset, Linker.none, cache, t"base")
          counting.checks = 0
          val two: Doc = Doc.parse(path, second, counting, session, Unset, Linker.none, cache, t"base")
          (counting.checks, one, two)

      def starts(doc: Doc): List[Optional[Int]] = doc.blocks.bind:
        case code: Block.Code => List(code.start)
        case _                => Nil

      test(m"an unchanged document is compiled from what was kept, to the same blocks"):
        val (checks, one, two) = twice(document(t"val b = a + 1"), document(t"val b = a + 1"), true, t"doc/s1.md")
        (checks, one.blocks == two.blocks, one.problems == two.problems)
      . assert(_ == (0, true, true))

      test(m"a change of prose compiles no fence, and the fences' lines move with them"):
        val (checks, one, two) =
          twice(document(t"val b = a + 1"), document(t"val b = a + 1", t"Two lines\nof prose.\n\n"), true, t"doc/s2.md")

        (checks, starts(two) == starts(one).map(_.let(_ + 3)), Blocks.plain(two.blocks.at(Sec).lay(Nil) { case Block.Paragraph(c) => c; case _ => Nil }))
      . assert(_ == (0, true, t"Two lines of prose."))

      test(m"in a session, a changed fence is compiled with those after it, and not those before"):
        twice(document(t"val b = a + 1"), document(t"val b = a + 2"), true, t"doc/s3.md")(0)
      . assert(_ == 2)

      test(m"outside a session, a changed fence is compiled alone"):
        twice(document(t"val b = 2"), document(t"val b = 3"), false, t"doc/s4.md")(0)
      . assert(_ == 1)

      test(m"a document compiled through a cache has the blocks it has without one"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val text: Text = Text(java.nio.file.Files.readString(java.nio.file.Path.of(s"$here/doc/markdown.md")).nn)

        Checking.session(classpath, Nil): (checker: Checker) =>
          val plain: Doc = Doc.parse(t"markdown.md", text, checker, true)
          val kept: Doc = Doc.parse(t"markdown.md", text, checker, true, Unset, Linker.none, Cache.Transient(), t"base")
          (plain.blocks == kept.blocks, plain.problems.size == kept.problems.size)
      . assert(_ == (true, true))

    suite(m"Output directory"):
      val fragment: Output.Fragment =
        Output.Fragment(List(Block.paragraph(t"code")), Nil, true, List(t"x"), Nil, t"val x = 1", 4)

      val prose: Output.Prose = Output.Prose(List(Block.paragraph(t"prose"), Output.sentinel(0)), Nil)

      def directory(): Text = Text(java.nio.file.Files.createTempDirectory("fuel-output").nn.toString)

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
          (manifest.fuel, manifest.classpath, manifest.documents.map { (d: Output.Manifest.Document) => (d.section, d.path, d.title) })
      . assert(_ == (t"1.0", t"classpath", List((t"doc/modules", t"json.md", t"JSON"))))

    suite(m"Output"):
      test(m"a compiled document's blocks round-trip through BinTEL as a fragment's output"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)

        Compilation.document(t"$here/doc/markdown.md").lay(t"unreadable"): (doc: Doc) =>
          val fragment: Output.Fragment = Output.Fragment(doc.blocks, doc.problems, true, List(t"x"), List(t"import a.*"), t"val x = 1", 7)
          val data: Data = Output.Fragment.encode(fragment)
          Output.Fragment.decode(data).lay(t"undecodable") { (back: Output.Fragment) => if back == fragment then t"same" else t"different" }

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

      val output: Text = Text(java.nio.file.Files.createTempDirectory("fuel-api").nn.toString)
      val entries: List[Text] = classpath().cut(t":").filter(_ != t"")

      lazy val notices: List[Notice] =
        Checking.session(classpath, Nil) { (checker: Checker) => checker.build(Map(t"Sample.scala" -> sample), output) }

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
          case api: Api         => t"${api.entities.size.toString} entities"
          case error: Throwable => t"${error.toString}: ${java.lang.String.join("; ", error.getStackTrace.nn.take(6).map(_.toString)*).nn}"
      . assert(_.ends(t"entities"))

      test(m"a case class and its companion are one entity, with a template and a definition"):
        entity(t"sample.Doc").let { (doc: Api.Entity) => (doc.own, doc.template.let(_.kind), doc.definition.let(_.kind)) }
      . assert(_ == (true, Api.Kind.CaseClass, Api.Kind.Object))

      test(m"a field is a member of the type, and a companion method of the term"):
        (entity(t"sample.Doc#title").let(_.definition.let(_.kind)), entity(t"sample.Doc.parse").let(_.definition.let(_.kind)))
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
        entity(t"sample").let { (p: Api.Entity) => (p.members.has(t"sample.Doc"), p.overloads.size) }
      . assert(_ == (true, 0))

      test(m"a private member is not walked"):
        entity(t"sample.Doc#secret").present
      . assert(_ == false)

      test(m"a method's signature has its clauses and its result, and its overloads are kept"):
        entity(t"sample.Doc.parse").let { (e: Api.Entity) => (e.definition.let(_.clauses.size), e.definition.let(_.result.let(_.qualified)), e.overloads.size) }
      . assert(_ == (1, t"sample.Doc", 1))

      test(m"a class's parents are its declared ones"):
        entity(t"sample.Doc").let(_.template.let(_.parents.map(_.qualified)))
      . assert(_ == List(t"sample.Shape[scala.Int]"))

      test(m"an extension keeps the clause it extends apart from its own"):
        entity(t"sample.Util.twice").let(_.definition.let { (d: Api.Declaration) => (d.kind, d.group.size, d.clauses.size) })
      . assert(_ == (Api.Kind.Extension, 1, 0))

      test(m"a type alias has its parameters and its right-hand side"):
        entity(t"sample.Util.Pair").let(_.template.let { (d: Api.Declaration) => (d.kind, d.clauses.size, d.result.let(_.qualified)) })
      . assert(_ == (Api.Kind.Type, 1, t"(A, A)"))

      test(m"a given and a val are declarations of their kinds"):
        (entity(t"sample.Util.given_Ordering_Doc").let(_.definition.let(_.kind)), entity(t"sample.Util.limit").let(_.definition.let(_.kind)))
      . assert(_ == (Api.Kind.Given, Api.Kind.Val))

      test(m"a dependency jar is walked through the session's symbol table"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val jar: Text = t"$here/out/fuel/launcher/assembly.dest/out.jar"

        val result: Api | Throwable =
          Checking.session(classpath, Nil): (checker: Checker) =>
            checker.context.lay[Api | Throwable](Api.empty) { (context: dotty.tools.dotc.core.Contexts.Context) => Inspection.classpath(List(jar), context) }


        result match
          case api: Api         => (api.entities.size > 1000, api(t"scala.Option").let(_.own), api(t"scala.Option").let(_.template.let(_.kind)), api(t"scala.Option#map").present, api(t"scala.Option#map").let(_.definition.let(_.result.let(_.qualified))))
          case error: Throwable => error.toString
      . assert(_ == (true, false, Api.Kind.Class, true, t"scala.Option[B]"))

      test(m"every type of a dependency renders through stenography, none from its parts and none as `?`"):
        (Inspection.fallbacks.get, Inspection.unrendered.get, Inspection.rendered.get > 10000)
      . assert(_ == (0, 0, true))

      test(m"a dependency jar is walked before a build, in a session on that jar under the project's options"):
        val here: Text = Text(java.lang.System.getProperty("user.dir").nn)
        val jar: Text = t"$here/out/fuel/launcher/assembly.dest/out.jar"
        val out: Text = Text(java.nio.file.Files.createTempDirectory("fuel-probe").nn.toString)

        val config: Text = Text(java.nio.file.Files.readString(java.nio.file.Path.of(s"$here/.pyrocosm/fuel/config.tel")).nn)
        val options: List[Text] = config.cut(t"\n").filter(_.starts(t"scalac-option ")).map(_.skip(14).trim)
        val sizes: (Int, Int) =
          Checking.session(List(jar), options): (checker: Checker) =>
            val before: Int = checker.context.lay(0) { (c: dotty.tools.dotc.core.Contexts.Context) => Inspection.classpath(List(jar), c) match { case api: Api => api.entities.size; case _ => -1 } }
            checker.build(Map(t"Probe.scala" -> sample), out)
            val after: Int = checker.context.lay(0) { (c: dotty.tools.dotc.core.Contexts.Context) => Inspection.classpath(List(jar), c) match { case api: Api => api.entities.size; case _ => -1 } }
            (before, after)

        // After a compile under these options the walk reads next to nothing, which is why
        // the compile step walks the dependencies first.
        (sizes(0) > 10000, sizes(1) < sizes(0))
      . assert(_ == (true, true))

      test(m"a symbolic name is encoded in its path, and decoded from it"):
        Api.unpath(Api.path(stenography.Designator(t"a.B"), t"+:")).name
      . assert(_ == t"+:")

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

      def problems(tone: Tone): List[Text] =
        merged.lay(Nil: List[Block]) { (m: (Api, List[Block])) => m(1) }.bind:
          case Block.Diagnostic(tone0, location, message, _, _) if tone0 == tone =>
            List(t"${location.let(_.path).or(t"?")}:${location.let(_.line).or(0).toString}: ${Blocks.plain(message.bind { case Block.Paragraph(c) => c; case _ => Nil })}")
          case _ => Nil

      test(m"an overlay is read: its base, its package's info and detail, and its imports"):
        read match
          case overlay: Overlay => (overlay.base, overlay.doc.info, overlay.doc.detail.let(_.trim), overlay.doc.imports)
          case other            => other
      . assert(_ == (t"sample", t"a sample package", t"All about _sample_.", List(t"scala")))

      test(m"an entry has its name, kind, info (inline), detail (source), line and inherited imports"):
        read match
          case overlay: Overlay =>
            overlay.entries.prim.let { (e: Overlay.Entry) => (e.name, e.isType, e.doc.info, e.doc.detail.let(_.trim), e.line, e.doc.imports, e.entries.size) }
          case other => other
      . assert(_ == (t"Doc", false, t"a document", t"A `Doc` has a *title*.", 8, List(t"scala", t"scala.util"), 5))

      test(m"a leaf entry's detail is its source atom"):
        read match
          case overlay: Overlay => overlay.entries.at(Sec).let(_.entries.prim.let(_.doc.detail))
          case other            => other
      . assert(_ == Unset)

      test(m"an entry's children are placed by their parent's kind"):
        merged.let(_(0)(t"sample.Doc#title").let(_.doc.let(_.info)))
      . assert(_ == t"its title")

      test(m"an entry for a member nothing declares is a failure at its line"):
        problems(Tone.Failure)
      . assert(_ == List(t"doc/api/sample.tel:22: `.nowhere` is documented here, but no such member of `sample` is declared."))

      test(m"an entry marked `until` need not exist, and has a page which is removed"):
        merged.let(_(0)(t"sample.Doc#whisper").let { (e: Api.Entity) => (e.removed, e.doc.let(_.until.let(_.replacement))) })
      . assert(_ == (true, t"sample.Doc#shout"))

      test(m"an undocumented public member is a caution where it is declared; a hidden entity's members are not"):
        problems(Tone.Warning)
      . assert(_ == List
          ( t"Sample.scala:6: `sample.Doc#size` is not documented: add `entry #size` under `sample.Doc` in doc/api/sample.tel.",
            t"Sample.scala:4: `sample.Shape#area` is not documented: add `entry #area` under `sample.Shape` in doc/api/sample.tel." ))

    suite(m"Resolution"):
      def resolver(scope: Scope): Resolver = Resolver(Tests.this.api0, scope)
      def linked(scope: Scope, reference: Text): Optional[Text] = resolver(scope).link(reference) match
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

      test(m"a member resolves through its enclosing entity, and by Amok's `.`/`#` forms"):
        val doc: Api.Entity = Tests.this.api0(t"sample.Doc").or(Api.Entity(Unset, t"?"))
        val scope: Scope = Scope.of(doc, Nil)
        (linked(scope, t"parse"), linked(scope, t"#title"), linked(scope, t".parse"), linked(scope, t"Doc#shout"))
      . assert(_ == (t"/api/sample.Doc.parse", t"/api/sample.Doc~title", t"/api/sample.Doc.parse", t"/api/sample.Doc~shout"))

      test(m"a fragment's imports are learned"):
        val r: Resolver = resolver(Scope.document(Nil))
        r.learn(List(t"import sample.*"))
        r.link(t"Util") match
          case Inline.Destination.Local(path, _) => path
          case _                                 => Unset
      . assert(_ == t"/api/sample.Util")

      test(m"a typed token links only when it is the name its type names"):
        val r: Resolver = resolver(Scope.document(Nil))
        val doc: stenography.Syntax = stenography.Syntax.Simple(stenography.Designator(t"sample.Doc"))
        (r.link(t"Doc", doc).present, r.link(t"x", doc).present)
      . assert(_ == (true, false))

      test(m"a signature is tokens, its designators linked and elided by the scope"):
        val api: Api = Tests.this.api0
        val parse: Api.Declaration = api(t"sample.Doc.parse").let(_.definition).or(Api.Declaration(Api.Kind.Def))
        val tokens = Signatures.declaration(t"parse", parse, api, Scope.document(List(t"sample")))
        (tokens.map(_.text).join(t""), tokens.seek(_.text == t"Doc").let(_.link))
      . assert(_ == (t"def parse(text: String): Doc", Inline.Destination.Local(t"/api/sample.Doc")))

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

      test(m"a page has its heading, signature and member rows anchored by name"):
        val api: Api = Tests.this.api0
        val doc: Api.Entity = api(t"sample.Doc").or(Api.Entity(Unset, t"?"))
        val page = ApiPages.page(api, doc, resolver(Scope.of(doc, Nil)), Scope.of(doc, Nil), Nil)
        val rows: List[Text] = page.bind { case Block.Table(_, rows, _) => rows.bind { (r: Block.Row) => r.id.let(List(_)).or(Nil) }; case _ => Nil }
        (page.prim, rows.has(t"title"), rows.has(t"parse"))
      . assert(_ == (Block.Paragraph(List(Inline.Link(Inline.Destination.Local(t"/api/sample"), Inline.text(t"sample")), Inline.Toned(Tone.Muted, Inline.text(t".")))), true, true))

      test(m"the tree links each node to its page, open along the way to the current one"):
        val tree = ApiPages.tree(Tests.this.api0, t"sample.Doc")
        tree.roots.prim.let { (n: Block.TreeNode) => (n.destination, n.open, n.children.map(_.label).map(Blocks.plain)) }
      . assert(_ == (Inline.Destination.Local(t"/api/sample"), true, List(t"Doc", t"Shape", t"Util")))

    suite(m"Sections"):
      test(m"a section is named by its directory's last segment"):
        Compilation.name(t"/home/me/project/doc/modules/")

      . assert(_ == t"modules")
