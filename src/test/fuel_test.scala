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
          case _ :: _ :: Block.Code(Language.Scala, line :: _, _, _, _, _, _, _) :: _ =>
            line.tokens.size > 1

          case _ =>
            false

      . assert(_ == true)

      test(m"a fence in an unknown language is plain, under that language"):
        Doc.parse(t"doc/intro.md", intro).blocks match
          case _ :: _ :: _ :: Block.Code(language, lines, _, _, _, _, _, _) :: _ =>
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
          case _ :: Block.Code(_, _, _, start, _, _, _, _) :: _ => start
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

      test(m"a fragment whose typed highlighting fails is checked by the typer, not lost"):
        val messages: List[Text] =
          problems:
            t"```scala\nimport soundness.*\nimport dysasymptotics.linearSize\n\n" +
              t"case class Issues(items: List[Int] = Nil)(using Diagnostics)\n" +
              t"extends Error(m\"$${items.size} problems\"):\n" +
              t"  def +(item: Int): Issues = Issues(items :+ item)\n```\n"

          . bind:
              case Block.Diagnostic(_, _, content, _, _) => content.map:
                case Block.Paragraph(inlines) => Blocks.plain(inlines)
                case _                        => t""

              case _ =>
                Nil

        messages.filter(_.starts(t"The fragment could not be highlighted"))

      . assert(_ == Nil)

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
            case Block.Code(_, _, note :: _, _, _, _, _, _) :: Block.Diagnostic(_, _, _, _, _) :: _ =>
              (note.line, note.style)

            case _ =>
              (-1, Block.Note.Style.Param)

      . assert(_ == (1, Block.Note.Style.Erroneous))

      test(m"an error of several lines is a card after its fragment, not a caption"):
        val fence: Text = t"```scala fuel\nerrors show\n##\nval n: Int = \"oops\"\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", fence, checker).blocks match
            case Block.Code(_, _, note :: _, _, _, _, _, _) :: Block.Diagnostic(_, _, _, _, _) :: _ =>
              note.caption.nil

            case _ =>
              false

      . assert(_ == true)

      test(m"an error of one line is a caption on its code, with no card, and still a problem"):
        val fence: Text = t"```scala fuel\nerrors show\n##\nval n = missing\n```\n"

        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/check.md", fence, checker).blocks match
            case Block.Code(_, _, note :: _, _, _, _, _, _) :: Nil =>
              Blocks.plain(note.caption).contains(t"missing")

            case _ =>
              false

      . assert(_ == true)

      test(m"a failing fragment's error of one line remains a problem of the document"):
        problems(t"```scala\nval n = missing\n```\n") match
          case Block.Diagnostic(Tone.Failure, location, _, excerpt, _) :: Nil =>
            (location.let(_.line), excerpt.size)

          case _ =>
            (Unset, 0)

      . assert(_ == (2, 1))

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
          case _ :: Block.Code(_, _, _, _, animation, _, _, _) :: _ => animation.let(_.slides.size)
          case _                                              => Unset

        (problem, frames)

      . assert(_ == ((7, true), 1))

      test(m"a frame's details reach the animation, and a block-level mark every frame it matches"):
        val text: Text =
          t"```scala fuel\nhighlight x\nframe\n  code\n      val x: Int = 1\n" +
            t"frame\n  label a *second* value\n  hold 2\n  at 12.5\n" +
            t"  highlight y  the new one\n  code\n      val y: Int = 2\n##\n```\n"

        Doc.parse(t"doc/anim.md", text).blocks match
          case Block.Code(_, _, notes, _, animation, _, _, _) :: Nil =>
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
            case Block.Code(_, _, _, _, animation, _, _, _) :: Nil =>
              animation.let: (animation: Block.Animation) =>
                ( doc.problems.size,
                  animation.slides.skip(1).head.let(_.language),
                  animation.lines(1).head.let(_.size > 1) )

            case _ =>
              Unset

      . assert(_ == (0, Language.Java, true))

      test(m"a java fence with a preamble is read by the preamble embedding"):
        Doc.parse(t"doc/j.md", t"```java fuel\nid j\n##\nint x = 1;\n```\n").blocks match
          case Block.Code(language, line :: _, _, _, _, _, _, _) :: Nil =>
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
          case Block.Code(_, lines, _, _, animation, _, _, _) :: Nil =>
            (lines.size, animation.let(_.slides.size), animation.let(_.lines(1).size))

          case _ =>
            (0, Unset, Unset)

      . assert(_ == (1, 3, 2))

      test(m"a surviving token keeps its atom across frames"):
        Doc.parse(t"doc/anim.md", animatedFence).blocks match
          case Block.Code(_, _, _, _, animation, _, _, _) :: Nil =>
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

    suite(m"Code rendering"):
      def doc(markdown: Text): Doc = Doc.parse(t"doc/code.md", markdown)
      def fence(language: Text, preamble: Text, code: Text): Text =
        t"```$language fuel\n$preamble\n##\n$code\n```\n"

      def code(doc: Doc): Optional[Block.Code] = doc.blocks.seek { case _: Block.Code => true; case _ => false }.let:
        case code: Block.Code => code
        case _                => Unset

      def accents(line: Block.Line): List[(Text, Token.Accent)] =
        line.tokens.map { token => (token.text, token.accent) }.filter(_(0).trim != t"")

      test(m"the preamble reads a path, substitutes, a focus and alternatives"):
        val text: Text =
          t"path data/config.json\nsubstitute 2..3  ...\nsubstitute 4\n  text  etc\nfocus x..y\n" +
            t"alternative Bash\n  code\n      export X=1\nalternative Fish\n  language fish\n  frame\n    code set X 1\n"

        Preamble.parse(text) match
          case p: Preamble =>
            ( p.path,
              p.substitutions.map { s => (s.selection.start, s.text) },
              p.marks.map(_.style),
              p.alternatives.map { a => (a.label, a.code, a.language, a.frames.map(_.code)) } )

          case _ => Unset

      . assert(_ == ( t"data/config.json",
                      List((t"2", t"..."), (t"4", t"etc")),
                      List(Block.Note.Style.Focus),
                      List( (t"Bash", t"export X=1", Unset, Nil),
                            (t"Fish", Unset, t"fish", List(t"set X 1")) ) ))

      test(m"a comment, to the end of its line or to the `*/` which closes it, is a comment"):
        val source: Text = t"val x = 1 // one\n/* two\n   lines */ val y = 2\nval z = \"// not\""
        code(doc(fence(t"scala", t"", source))).let: (block: Block.Code) =>
          block.lines.map(accents(_).filter(_(1) == Token.Accent.Comment).map(_(0)))

      . assert(_ == List(List(t"// one"), List(t"/* two"), List(t"   lines */"), Nil))

      test(m"a substitute shows its text in place of the range, in its own accent; the code compiles"):
        val compiled: Doc = doc(fence(t"scala", t"substitute 2..3  ...", t"val total: Int = 1 + 2 + 3"))
        code(compiled).let: (block: Block.Code) =>
          ( block.lines.map(_.tokens.map(_.text).join),
            block.lines.head.let(accents(_).filter(_(1) == Token.Accent.Substitute)).or(Nil),
            compiled.problems )

      . assert(_ == (List(t"val total: Int = 1 + ..."), List((t"...", Token.Accent.Substitute)), Nil))

      test(m"a fence with a path is the content of that file, named on its block"):
        code(doc(fence(t"json", t"path config.json", t"{ \"a\": 1 }"))).let(_.path)

      . assert(_ == t"config.json")

      test(m"a path leading out of the document's directory is a problem"):
        doc(fence(t"json", t"path ../secret", t"x")).problems.size

      . assert(_ == 1)

      test(m"alternatives are the block's, each with its lines; the first is its own"):
        val preamble: Text =
          t"alternative Braces\n  code\n      object X { def y = 1 }\n" +
            t"alternative Whitespace\n  code\n      object X:\n        def y = 1\n"

        val compiled: Doc = doc(fence(t"scala", preamble, t""))
        code(compiled).let: (block: Block.Code) =>
          ( block.alternatives.map { a => Inline.plain(a.label) },
            block.alternatives.map(_.lines.size),
            block.lines.size,
            compiled.problems.size )

      . assert(_ == (List(t"Braces", t"Whitespace"), List(1, 2), 1, 0))

      test(m"with alternatives, code after ## is a problem"):
        val preamble: Text = t"alternative A\n  code val a = 1\n"
        doc(fence(t"scala", preamble, t"val b = 2")).problems.size

      . assert(_ == 1)

      test(m"an animated alternative carries its animation, and a focus mark its style"):
        val preamble: Text =
          t"alternative One\n  frame\n    code val a = 1\n    focus a\n  frame\n    code val a = 2\n" +
            t"alternative Two\n  code val b = 1\n"

        code(doc(fence(t"scala", preamble, t""))).let: (block: Block.Code) =>
          ( block.alternatives.map(_.animation.let(_.slides.size)),
            block.animation.let(_.slides.head.let(_.notes.map(_.style))) )

      . assert(_ == (List(2, Unset), List(Block.Note.Style.Focus)))

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

      test(m"two documents of one name, in different sections, keep apart in one session"):
        Checking.session(classpath, Nil): (checker: Checker) =>
          val first: Text =
            t"```scala\ncase class Event(n: Int)\n```\n\n```scala\nval e: Int = Event(1).n\n```\n"

          val second: Text =
            t"```scala\ncase class Event(s: String)\n```\n\n" +
              t"```scala\nval e: String = Event(\"x\").s\n```\n"

          val a: List[Block] =
            Doc.parse(t"logging.md", first, checker, true, section = t"modules").problems

          val b: List[Block] =
            Doc.parse(t"logging.md", second, checker, true, section = t"standards").problems

          a + b

      . assert(_ == Nil)

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
          case Block.Code(_, _, note :: _, _, _, _, _, _) :: _ => (note.line, note.style)
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

    suite(m"Running"):
      def parsed(markdown: Text, session: Boolean = true): Doc =
        Checking.session(classpath, Nil): (checker: Checker) =>
          Doc.parse(t"doc/run.md", markdown, checker, session)

      def outputs(doc: Doc): List[Block.Output] = doc.blocks.bind:
        case output: Block.Output => List(output)
        case _                    => Nil

      def thrown(doc: Doc): List[Text] = doc.blocks.bind:
        case Block.Diagnostic(Tone.Failure, _, Block.Paragraph(content) :: _, _, _) =>
          List(Blocks.plain(content))

        case _ =>
          Nil

      def fence(preamble: Text, code: Text): Text = t"```scala fuel\n$preamble\n##\n$code\n```\n"

      test(m"a fragment run shows what it printed beneath its code"):
        val doc: Doc = parsed(fence(t"run", t"println(\"hi\")"))
        (outputs(doc), doc.problems)

      . assert(_ == (List(Block.Output(t"hi\n")), Nil))

      test(m"the standard streams are shown in the order written"):
        val code: Text = t"println(\"a\")\nSystem.err.println(\"b\")\nprintln(\"c\")"
        outputs(parsed(fence(t"run", code))).map(_.text)

      . assert(_ == List(t"a\nb\nc\n"))

      test(m"styled output keeps its colour and weight; a colour left to the terminal is unset"):
        val esc: Text = t"27.toChar.toString"
        val code: Text = t"println($esc + \"[1;31mred\" + $esc + \"[0m plain\")"
        val found: List[Block.Output] = outputs(parsed(fence(t"run", code)))

        val runs: List[(Int, Boolean, Boolean)] =
          found.head.let(_.runs).or(Nil).map: (run: Block.Run) =>
            (run.length, run.foreground.present, run.bold)

        (found.map(_.text), runs)

      . assert(_ == (List(t"red plain\n"), List((3, true, true), (7, false, false))))

      test(m"a thrown exception is a failure at the fence, shown beneath the output"):
        val doc: Doc = parsed(fence(t"run", t"println(\"before\")\nthrow Exception(\"boom\")"))
        (outputs(doc).map(_.text), doc.problems.size, thrown(doc).exists(_.contains(t"boom")))

      . assert(_ == (List(t"before\n"), 1, true))

      test(m"with errors show, the exception is shown and is no problem"):
        val doc: Doc = parsed(fence(t"run\nerrors show", t"throw Exception(\"boom\")"))
        (doc.problems, thrown(doc).exists(_.contains(t"boom")))

      . assert(_ == (Nil, true))

      test(m"a fragment which is only parsed cannot be run"):
        val doc: Doc = parsed(fence(t"parse\nrun", t"val x = 1"))
        (outputs(doc), doc.problems.size)

      . assert(_ == (Nil, 1))

      test(m"a term fragment is run as an expression"):
        outputs(parsed(fence(t"run\ncontext term", t"{ println(\"x\"); 1 + 1 }"))).map(_.text)

      . assert(_ == List(t"x\n"))

      test(m"a fragment run reads a sample file by its relative path"):
        val sample: Text = t"```json fuel\npath data/config.json\n##\n{ \"name\": \"fuel\" }\n```\n\n"

        val reader: Text =
          t"val dir = summon[ambience.WorkingDirectory].directory()\n" +
            t"println(java.nio.file.Files.readString(java.nio.file.Path.of(dir.toString, \"data/config.json\")))"

        // Soundness is experimental, as a project which uses it says in its options.
        val doc: Doc =
          Checking.session(classpath, List(t"-experimental")): (checker: Checker) =>
            Doc.parse(t"doc/run.md", sample + fence(t"run", reader), checker, true)

        (outputs(doc).map(_.text), doc.problems)

      . assert(_ == (List(t"{ \"name\": \"fuel\" }\n"), Nil))

      test(m"a fragment run in a session sees what came before it"):
        val first: Text = t"```scala\nval base = 41\n```\n\n"
        val markdown: Text = first + fence(t"run", t"println(base + 1)")
        outputs(parsed(markdown, true)).map(_.text)

      . assert(_ == List(t"42\n"))

      test(m"a fragment's output is kept with it, and not run again from the cache"):
        Checking.session(classpath, Nil): (checker: Checker) =>
          val cache: Cache = Cache.Transient()
          val text: Text = fence(t"run", t"println(System.nanoTime)")

          def parse(): Doc =
            Doc.parse(t"doc/run.md", text, checker, true, Unset, Linker.none, cache, t"base")

          val one: List[Block.Output] = outputs(parse())
          val two: List[Block.Output] = outputs(parse())
          (one == two, one.size)

      . assert(_ == (true, 1))

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
  syntax scala
      val x = 1

slide
  syntax scala
      val x = 1
      val y = x + 1

slide
  syntax scala
      val x = 1
      val y = x + 1
      val z = y * 2

slide
  syntax java
      int x = 1;

slide
  markdown
      The end.
  transcript
      That is all there is to say.

      Thank you.
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
            case Block.Deck(leaves, _) => leaves
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
            case Block.Code(_, _, _, _, animation: Block.Animation, _, _, _) =>
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

      . assert(_ == List(2, 4))

      test(m"the schema reads, and a presentation which follows it has no faults"):
        (Presentation.schema.name, read(talk).lay(Nil)(_(1)).size)

      . assert(_ == (t"presentation", 0))

      test(m"a slide's `code` is reported as now being `syntax`, at its line"):
        read(t"slide\n  code scala\n      val x = 1\n").lay(Nil)(_(1)).map:
          case Block.Diagnostic(_, location, content, _, _) =>
            val said: Text = content.head match
              case Block.Paragraph(inlines) => Blocks.plain(inlines)
              case _                        => t""

            (location.let(_.line), said.contains(t"syntax"))
          case _ =>
            (Unset, false)

      . assert(_ == List((2, true)))

      test(m"what a slide's syntax does not say, and a `syntax` without a language, are faults"):
        val text: Text = t"slide\n  syntax\n    highlite x\n    code\n        val x = 1\n"
        read(text).lay(Nil)(_(1)).map:
          case Block.Diagnostic(_, location, _, _, _) => location.let(_.line)
          case _                                      => Unset

      . assert(_ == List(3, 2))

      // An alignment of a reading of three slides' transcripts — "one two three", "four five"
      // and "six" — in MFA's three formats, with a pause, a word MFA did not know, and the
      // phones tier a reader must pass over.
      val grid: Text = t"""File type = "ooTextFile"
Object class = "TextGrid"

xmin = 0
xmax = 6.5
tiers? <exists>
size = 2
item []:
    item [1]:
        class = "IntervalTier"
        name = "words"
        xmin = 0
        xmax = 6.5
        intervals: size = 8
        intervals [1]:
            xmin = 0
            xmax = 0.5
            text = ""
        intervals [2]:
            xmin = 0.5
            xmax = 1.0
            text = "one"
        intervals [3]:
            xmin = 1.0
            xmax = 1.5
            text = "two"
        intervals [4]:
            xmin = 1.5
            xmax = 2.0
            text = "three"
        intervals [5]:
            xmin = 2.0
            xmax = 3.0
            text = ""
        intervals [6]:
            xmin = 3.0
            xmax = 3.5
            text = "<unk>"
        intervals [7]:
            xmin = 3.5
            xmax = 4.0
            text = "five"
        intervals [8]:
            xmin = 4.0
            xmax = 6.5
            text = "six"
    item [2]:
        class = "IntervalTier"
        name = "phones"
        xmin = 0
        xmax = 6.5
        intervals: size = 1
        intervals [1]:
            xmin = 0
            xmax = 6.5
            text = "sil"
"""

      val json: Text =
        t"""{"start": 0, "end": 6.5, "tiers": {"words": {"type": "interval", "entries": [[0.5, 1.0, "one"], [1.0, 1.5, "two"], [1.5, 2.0, "three"], [2.0, 3.0, ""], [3.0, 3.5, "<unk>"], [3.5, 4.0, "five"], [4.0, 6.5, "six"]]}, "phones": {"type": "interval", "entries": [[0, 6.5, "sil"]]}}}"""

      val csv: Text =
        t"Begin,End,Label,Type,Speaker\n0.5,1.0,one,words,x\n1.0,1.5,two,words,x\n1.5,2.0,three,words,x\n" +
          t"2.0,3.0,,words,x\n3.0,3.5,<unk>,words,x\n3.5,4.0,five,words,x\n4.0,6.5,six,words,x\n0,6.5,sil,phones,x\n"

      def spoken(text: Text): List[(Text, Double)] = ForcedAlignment.read(text) match
        case words: List[ForcedAlignment.Word] @unchecked => words.map { word => (word.label, word.start) }
        case problem: Text                                => List((problem, -1.0))
        case _                                            => Nil

      test(m"an alignment's spoken words are read alike from a TextGrid, JSON and CSV"):
        val expected: List[(Text, Double)] =
          List((t"one", 0.5), (t"two", 1.0), (t"three", 1.5), (t"<unk>", 3.0), (t"five", 3.5), (t"six", 4.0))

        (spoken(grid) == expected, spoken(json) == expected, spoken(csv) == expected)

      . assert(_ == (true, true, true))

      test(m"text which is no alignment, or a TextGrid without a words tier, says why"):
        (spoken(t"hello"), spoken(grid.sub(t"name = \"words\"", t"name = \"mots\"")).map(_(0)))

      . assert(_ == (List((t"not a TextGrid, JSON or CSV alignment", -1.0)), List(t"the TextGrid has no tier named `words`")))

      test(m"a transcript's words are normalised as the aligner's are"):
        ForcedAlignment.words(t"  Hello, World! It's  well-known —\nthat's 3.\n")

      . assert(_ == List(t"hello", t"world", t"it's", t"well-known", t"that's", t"3"))

      def aligned: List[ForcedAlignment.Word] = ForcedAlignment.read(grid) match
        case words: List[ForcedAlignment.Word] @unchecked => words
        case _                                            => Nil

      def slides(transcripts: List[Optional[Text]]): Text =
        transcripts.map: (transcript: Optional[Text]) =>
          t"slide\n  markdown\n      a\n" + transcript.lay(t"") { text => t"  transcript\n      $text\n" }
        . join

      def timed(transcripts: List[Optional[Text]]): List[Double] | List[Text] =
        read(slides(transcripts)).lay(List[Text]()): (presentation: Presentation, _) =>
          ForcedAlignment.schedule(presentation, aligned) match
            case ForcedAlignment.Outcome.Timed(schedule, _) => schedule
            case ForcedAlignment.Outcome.Failed(messages)   => messages

      test(m"the transcript's words are compared with the alignment's, a word the aligner did not know standing for any"):
        val comparison = ForcedAlignment.compare(List(t"one", t"two", t"three", t"four", t"five", t"six"), aligned)
        (comparison.differing, comparison.close, comparison.places.get(3), comparison.places.get(5))

      . assert(_ == (0, true, Some(3), Some(5)))

      test(m"the schedule is each later slide's first word's time"):
        timed(List(t"One, two three.", t"Four five.", t"Six!"))

      . assert(_ == List(3.0, 4.0))

      test(m"a slide whose first word the aligner missed is timed by its next word"):
        timed(List(t"One, two.", t"Tree three four.", t"Five six!"))

      . assert(_ == List(1.5, 3.5))

      test(m"a slide without a transcript cannot be timed by an alignment"):
        timed(List(t"One, two three.", Unset, t"Four five six.")) match
          case messages: List[Text] @unchecked => messages.map(_.keep(16))

      . assert(_ == List(t"slide 2 (line 6)"))

      test(m"a transcript which differs from the alignment in more than a word in ten is refused, with the differences shown"):
        timed(List(t"One, two, three.", t"Nine ten eleven twelve thirteen."))

      . assert(_ == List
          ( t"the transcript and the alignment differ in 6 of 8 words, which is too many to time the presentation by:",
            t"  … two three nine -ten -eleven -twelve -thirteen +five +six" ))

      test(m"a presentation's transcript is its slides' transcripts, in order, as paragraphs"):
        read(talk).lay(Nil): (presentation: Presentation, _) =>
          Presentation.transcript(presentation).map:
            case Block.Paragraph(content) => Blocks.plain(content)
            case _                        => t"?"

      . assert(_ == List(t"That is all there is to say.", t"Thank you."))

      def codes(text: Text): List[Block.Code] =
        leaves(text).bind(_.content).bind:
          case code: Block.Code => List(code)
          case _                => Nil

      test(m"a slide's code may say what a fence's preamble says, its code beneath `code`"):
        val slide: Text =
          t"slide\n  syntax scala\n    highlight y  a new name\n    code\n" +
            t"        val x = 1\n        val y = x + 1\n"

        codes(slide).bind(_.notes).map: (note: Block.Note) =>
          (note.style, Blocks.plain(note.caption), note.line)

      . assert(_ == List((Block.Note.Style.Highlight, t"a new name", 1)))

      test(m"a slide's code with frames of its own is an animation alone, not joined to others"):
        def frame(code: Text): Text = t"    frame\n      code\n$code"
        val second: Text = t"          val a = 1\n          val b = 2\n"
        val first: Text = t"          val a = 1\n"
        val framed: Text = t"slide\n  syntax scala\n" + frame(first) + frame(second)
        val plain: Text = t"slide\n  syntax scala\n      val c = 3\n"

        codes(framed + plain).map(_.animation.let(_.slides.size))

      . assert(_ == List(2, Unset))

      test(m"each slide of a run is a frame, with the slide's label"):
        def step(label: Text, code: Text): Text =
          t"slide\n  syntax scala\n    label  $label\n    code\n        $code\n"

        val run: Text = step(t"first", t"val x = 1") + step(t"second", t"val y = 2")

        def labels(animation: Block.Animation): List[Text] =
          animation.slides.map(_.label).map(Blocks.plain)

        codes(run).bind(_.animation.let(labels).or(Nil))

      . assert(_ == List(t"first", t"second"))

      test(m"a presentation is shown on a stage"):
        Presenter.interface(t"Talk", Block.Deck(Nil)).hints.has[pyrocosm.hints.web.Stage.type]

      . assert(_ == true)

      test(m"a deck's steps are its animations' later frames and the moves between its slides"):
        val deck: Block.Deck = Block.Deck(leaves(talk))
        (Presentation.steps(deck), Presentation.steps(Block.Deck(Nil)))

      . assert(_ == (3 + 2, 0))

      test(m"a regular schedule lands a step every interval, and a scheduled deck carries it"):
        val deck: Block.Deck = Block.Deck(leaves(talk))
        val schedule: List[Double] = Presentation.regular(Presentation.steps(deck), 4.0)
        (schedule, Presentation.scheduled(deck, schedule).schedule == schedule)

      . assert(_ == (List(4.0, 8.0, 12.0, 16.0, 20.0), true))

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
        def run(name: Text): Unit = inner.run(name)

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
        (second.fragment(t"k1") == fragment, second.prose(t"k2") == prose, second.hits())

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

      test(m"the JDK's public classes are walked, and named as Scala names them"):
        val result: Api | Throwable =
          Checking.session(classpath, Nil): (checker: Checker) =>
            checker.context.lay[Api | Throwable](Api.empty)(Inspection.jdk(_))

        result match
          case api: Api =>
            val scope: Scope = Scope.document(Nil)

            def linked(reference: Text): Optional[Text] =
              Resolver(api, scope).link(reference) match
                case Inline.Destination.Local(path, _) => path
                case _                                 => Unset

            def marks(key: Text): List[Text] =
              api(key).let(_.template.let(_.modifiers).or(Nil)).or(Nil)

            val string: List[Text] = marks(t"java.lang.String")
            val deprecated: List[Text] = marks(t"java.lang.Deprecated")

            ( api(t"java.lang.String#getBytes").present,
              api(t"java.lang.String.valueOf").present,
              api(t"java.lang.StringLatin1").present,
              linked(t"#String#getBytes"),
              linked(t".java.lang.String.getBytes"),
              string.has(t"java") && !string.has(t"annotation"),
              deprecated.has(t"java") && deprecated.has(t"annotation") )

          case error: Throwable =>
            error.toString

      . assert(_ == (true, true, false, t"/api/java.lang.String~getBytes", Unset, true, true))

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

      // A package of exports, compiled on its own so that the other fixtures' counts stay as
      // they are.
      val exporting: Text = t"""package exporting

trait Titled:
  def heading: String = ""

infix type ~>[-A, +B] = PartialFunction[A, B]
infix type flip[A, B] = Either[B, A]
infix trait xor[A, B]

object Converting:
  def aliased: Int flip String = Left("")
  def other: Int xor String = ???

case class Point(x: Int) derives CanEqual

trait Show[T]

object Show:
  def derived[T]: Show[T] = new Show[T] {}

case class Shown(x: Int) derives Show

into trait Convertible

class Marked extends scala.annotation.StaticAnnotation

object Marks extends scala.annotation.StaticAnnotation

case class Doc(title: String) extends Titled

object Doc:
  def parse(text: String): Doc = Doc(text)
  def parse(text: String, size: Int): Doc = Doc(text)
  extension (doc: Doc) def size: Int = 0

case class Note(text: String)

object Note:
  extension (note: Note) def size: Int = 0

object Coding:
  opaque type Code = Int
  extension (code: Code) def digits: Int = code

object Exports:
  export exporting.Doc
  export exporting.Doc.parse
"""

      lazy val exported: Api =
        val output3: Text =
          Text(java.nio.file.Files.createTempDirectory("fuel-api3").nn.toString)

        Checking.session(classpath, Nil): (checker: Checker) =>
          checker.build(Map(t"Exporting.scala" -> exporting), output3)

        Inspection.walk(List(output3), Nil, entries) match
          case api: Api => api
          case _        => Api.empty

      test(m"into and annotation classes and objects are recorded as modifiers"):
        def modifiers(key: Text): List[Text] =
          exported(key).let: (e: Api.Entity) =>
            e.template.or(e.definition).let(_.modifiers).or(Nil)

          . or(Nil)

        ( modifiers(t"exporting.Convertible"),
          modifiers(t"exporting.Marked"),
          modifiers(t"exporting.Marks"),
          modifiers(t"exporting.Note"),
          modifiers(t"exporting.Coding") )

      . assert(_ == (List(t"into"), List(t"annotation"), List(t"annotation"), Nil, Nil))

      test(m"a type declared infix is written infix, in scope or qualified, and alias or not"):
        def result(name: Text, scope: Scope): Text =
          exported(t"exporting.Converting.$name").let(_.definition).let(_.result).lay(t"?"):
            (syntax: Syntax) => Signatures.tokens(syntax, exported, scope).map(_.text).join

        val outside: Scope = Scope.document(List(t"exporting"))
        val within: Scope = Scope(List(Designator(t"exporting")), Nil)

        (result(t"other", outside), result(t"other", within), result(t"aliased", within))

      . assert(_ == (t"Int exporting.xor String", t"Int xor String", t"Int flip String"))

      test(m"an infix type is declared infix on its own page"):
        val scope: Scope = Scope(List(Designator(t"exporting")), Nil)

        def head(key: Text, name: Text): Text =
          exported(key).let(_.template).let(Signatures.block(name, _, exported, scope)).lay(t"?"):
            (block: Block.Code) => block.lines.bind(_.tokens.map(_.text)).join

        (head(t"exporting.~>", t"~>"), head(t"exporting.xor", t"xor"))

      . assert(_ == (t"infix type A ~> B = PartialFunction[A, B]", t"infix trait A xor B"))

      test(m"a derives clause is recorded and written"):
        val point: Optional[Api.Entity] = exported(t"exporting.Point")

        val derives: List[Text] =
          point.let(_.template.let(_.derives.map(_.qualified)).or(Nil)).or(Nil)

        val scope: Scope = Scope(List(Designator(t"exporting")), Nil)

        val code: Text =
          point.let(_.template).let(Signatures.block(t"Point", _, exported, scope)).lay(t""):
            (block: Block.Code) =>
              block.lines.bind(_.tokens.map(_.text)).join

        ( derives.size,
          derives.head.or(t"").contains(t"CanEqual"),
          code.contains(t"derives CanEqual") )

      . assert(_ == (1, true, true))

      test(m"a derived instance is no member of its class, but is its type class's instance"):
        val api: Api = Applicables.decorate(exported)
        val scope: Scope = Scope(List(Designator(t"exporting")), Nil)

        def names(key: Text): List[Text] =
          api(key).lay(Nil: List[Text]): (entity: Api.Entity) =>
            ApiPages.page(api, entity, Resolver(api, scope), scope, Nil).bind:
              case Block.Table(_, rows, _) =>
                rows.map { row => Blocks.plain(row.cells.stdlib(1).content) }

              case _ =>
                Nil

        ( api(t"exporting.Shown.derived$$Show").let(_.derived).or(false),
          names(t"exporting.Shown").exists(_.starts(t"derived$$")),
          names(t"exporting.Show").exists(_.starts(t"derived$$")) )

      . assert(_ == (true, false, true))

      val capturing: Text = t"""package capturing

import language.experimental.captureChecking

class Sink extends caps.SharedCapability

object Io extends caps.SharedCapability:
  val out: Sink = Sink()

class Logger() uses Io.out initially, Io.out:
  Io.out.toString
  def log(): Unit = Io.out.toString
"""

      test(m"a uses clause is recorded and written, marking what is used initially"):
        val output4: Text =
          Text(java.nio.file.Files.createTempDirectory("fuel-api4").nn.toString)

        Checking.session(classpath, Nil): (checker: Checker) =>
          checker.build(Map(t"Capturing.scala" -> capturing), output4)

        Inspection.walk(List(output4), Nil, entries) match
          case api: Api =>
            val logger: Optional[Api.Declaration] = api(t"capturing.Logger").let(_.template)
            val uses: List[Text] = logger.let(_.uses.map(_.qualified)).or(Nil)
            val scope: Scope = Scope(List(Designator(t"capturing")), Nil)

            val code: Text =
              logger.let(Signatures.block(t"Logger", _, api, scope)).lay(t""):
                (block: Block.Code) => block.lines.bind(_.tokens.map(_.text)).join

            (uses.to[Set].size, uses.exists(_.contains(t"initially")), code.contains(t"uses "))

          case _ =>
            (0, false, false)

      . assert(_ == (2, true, true))

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

      test(m"an extension's glyph says what it is, beneath the clause it extends"):
        api0(t"sample.Util").let: (util: Api.Entity) =>
          val scope: Scope = Scope.of(util, Nil)
          val page: List[Block] = ApiPages.page(api0, util, Resolver(api0, scope), scope, Nil)

          def cell(row: Block.Row, index: Int): Text = Blocks.plain(row.cells.stdlib(index).content)

          page.bind:
            case Block.Table(_, rows, _) =>
              rows.filter { (row: Block.Row) => cell(row, 1) == t"twice" }.map(cell(_, 0))

            case _ =>
              Nil

        . or(Nil)

      . assert(_ == List(t"extension def"))

      test(m"an export is one row, whatever forwarders it made, and its page one signature"):
        val keys: List[Text] = List(t"exporting.Exports.Doc", t"exporting.Exports.parse")

        val found: List[List[Text]] = keys.map: (key: Text) =>
          exported(key).lay(Nil: List[Text]): (entity: Api.Entity) =>
            val scope: Scope = Scope.of(entity, Nil)
            val resolver: Resolver = Resolver(exported, scope)

            val kinds: List[Text] =
              ApiPages.rows(entity, true, exported, scope, resolver).map: (row: Block.Row) =>
                Blocks.plain(row.cells.stdlib.head.content)

            val signatures: List[Text] =
              ApiPages.page(exported, entity, resolver, scope, Nil).bind:
                case code: Block.Code => code.lines.map(_.tokens.map(_.text).join)
                case _                => Nil

            kinds + signatures

        found

      . assert: (found: List[List[Text]]) =>
          // `Doc` is both a type and a term, so it has two glyphs, each described as an export.
          found == List(List(t"export export", t"export Doc"), List(t"export", t"export Doc.parse"))

      def destination(api: Api, imports: List[Text], reference: Text): Optional[Text] =
        Resolver(api, Scope.document(imports)).link(reference) match
          case Inline.Destination.Local(path, _) => path
          case _                                 => Unset

      test(m"a member is named through the export of what declares it"):
        destination(exported, Nil, t".exporting.Exports.Doc.parse")

      . assert(_ == t"/api/exporting.Doc.parse")

      test(m"a member of a type is named through the type which inherits it"):
        ( destination(exported, Nil, t"#exporting.Doc#heading"),
          exported(t"exporting.Titled#heading").let(_.url) )

      . assert { (found, expected) => found.present && found == expected }

      test(m"an extension in a base's companion is named as a member of the type it extends"):
        ( destination(decorated, Nil, t"#extended.Doc#describe"),
          destination(decorated, Nil, t".extended.Doc.describe") )

      . assert(_ == (t"/api/extended.Shape.describe", Unset))

      test(m"an extension which needs an import is named as a member only where it is imported"):
        ( destination(decorated, Nil, t"#extended.Doc#titled"),
          destination(decorated, List(t"extended"), t"#extended.Doc#titled") )

      . assert(_ == (Unset, t"/api/extended.titled"))

      test(m"an extension is named by its leaf, and is ambiguous on several receivers"):
        val api: Api = Applicables.decorate(exported)
        val resolver: Resolver = Resolver(api, Scope.document(Nil))
        val ambiguous: Optional[Inline.Destination] = resolver.link(t".size")
        (destination(decorated, Nil, t".describe"), ambiguous, resolver.ambiguities)

      . assert(_ == (t"/api/extended.Shape.describe", Unset, List(t".size")))

      test(m"an extension in an object enclosing its receiver needs no import"):
        val api: Api = Applicables.decorate(exported)
        (destination(api, Nil, t"#exporting.Coding.Code#digits"), destination(api, Nil, t".digits"))

      . assert(_ == (t"/api/exporting.Coding.digits", t"/api/exporting.Coding.digits"))

      test(m"a name without a sigil is not sought among the extensions"):
        val resolver: Resolver = Resolver(Applicables.decorate(exported), Scope.document(Nil))
        (resolver.link(t"size"), resolver.ambiguities)

      . assert(_ == (Unset, Nil))

      test(m"a later standard import shadows an earlier one"):
        def named(parent: Text, name: Text): Api.Entity =
          Api.Entity
            ( Designator(parent), name, template = Api.Declaration(Api.Kind.Class) )

        val scala: Api.Entity = named(t"scala", t"Double")
        val java: Api.Entity = named(t"java.lang", t"Double")
        val api: Api = Api(sci.Map(scala.key -> scala, java.key -> java))
        destination(api, Nil, t"#Double")

      . assert(_ == t"/api/scala.Double")

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

        test(m"the note of how many more is no box, but ends its parent's line on the right"):
          val layout: Hierarchy.Layout = sibling.let(Hierarchy.layout(_)).or(laid(chain))
          val right: Int = layout.boxes.stdlib.map { b => b.x + b.w }.max

          ( layout.boxes.stdlib.exists(_.node.role == Role.More),
            layout.remainders.map(_.text),
            layout.remainders.stdlib.forall(_.to > right),
            layout.width > right )

        . assert(_ == (false, List(t"…and 1 more"), true, true))

        test(m"every joining line is drawn alike, and a parent's written form is no label"):
          val svg: Text = sibling.let(Hierarchy.layout(_)).let(Hierarchy.svg(_)).or(t"")
          (svg.contains(t"stroke: var(--pyro-muted)"), svg.contains(t"paint-order"))

        . assert(_ == (false, false))

        test(m"a package in scope is not written"):
          val scoped: Optional[Graph] = family(t"p.C1").let: (e: Api.Entity) =>
            Hierarchy.of(family, e, Scope.of(e, Nil))

          scoped.let(_.nodes.filter(_.role == Role.Peer).map(_.pkg)).or(Nil).to[Set]

        . assert(_ == Set(t""))

        test(m"a package which may be imported is drawn both ways, for the purview to choose"):
          val current: Node = node(t"x.A", Role.Current).copy(pkgPath = t"/api/x")
          val nodes: List[Node] = List(current, node(t"y.B", Role.Ancestor))
          val graph: Graph = Graph(nodes, List(Edge(t"x.A", t"y.B")), t"x.A")
          val svg: Text = Hierarchy.layout(graph).let(Hierarchy.svg(_)).or(t"")

          ( svg.contains(t"""<g data-not-imported="/api/x">"""),
            svg.contains(t"""<g data-imported="/api/x">""") )

        . assert(_ == (true, true))

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

        // `type Compass[points <: 4 | 16] = points match { case 4 => A; case 16 => B }`
        val compass: Api.Declaration =
          def item(pattern: Text, result: Text): Syntax =
            Syntax.Prefix(t"case", Syntax.Infix(Syntax.Primitive(pattern), t"=>", ret(result)))

          val bound: Syntax = Syntax.Infix(Syntax.Primitive(t"4"), t"|", Syntax.Primitive(t"16"))
          val param: Syntax = Syntax.Infix(Syntax.Symbolic(t"points"), t"<:", bound)
          val matched: Syntax = Syntax.Match(Syntax.Symbolic(t"points"), List(item(t"4", t"A"), item(t"16", t"B")))
          Api.Declaration(Api.Kind.Type, clauses = List(Syntax.Sequence('[', List(param))), result = matched)

        test(m"a match type's cases are a line each, without braces, their arrows aligned"):
          shown(t"Compass", compass, 88)

        . assert(_ == List( t"type Compass[points <: 4 | 16] = points match",
                            t"  case 4  => A",
                            t"  case 16 => B" ))

        test(m"a table cell keeps a declaration's type parameters, and a match type's cases"):
          val generic: Api.Declaration =
            Api.Declaration
              ( Api.Kind.Def,
                clauses = List(Syntax.Sequence('[', List(Syntax.Symbolic(t"A"))), clause(named(t"x", t"A"))),
                result = ret(t"A") )

          ( Signatures.cell(generic, Api.empty, scope, 40).tokens.map(_.text).join,
            Signatures.cell(compass, Api.empty, scope, 40).tokens.map(_.text).join )

        . assert(_ == ( t"[A](x: A): A",
                        t"[points <: 4 | 16] = points match\n  case 4  => A\n  case 16 => B" ))

        test(m"a match type within another type is written on one line"):
          val nested: Syntax = Syntax.Application(ret(t"List"), List(compass.result.or(ret(t"X"))), false)
          Signatures.tokens(nested, Api.empty, scope).map(_.text).join

        . assert(_ == t"List[points match { case 4 => A; case 16 => B }]")

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
                overloads = List(Api.Declaration(Api.Kind.Def, exported = true)),
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

        . assert(_ == (List(t"export export"), List(t"case class val")))

        test(m"a name both a type and a term has both glyphs, each described by its keywords"):
          val template: Api.Declaration = Api.Declaration(Api.Kind.Class, List(t"final", t"java"))
          val term: Api.Declaration = Api.Declaration(Api.Kind.Object)

          val entity: Api.Entity =
            Api.Entity(Designator.Top(t"p"), t"Doc", true, term, template)

          val api: Api = Api(sci.Map(entity.key -> entity))
          val resolver: Resolver = Resolver(api, scope)

          ApiPages.rows(entity, true, api, scope, resolver).bind: row =>
            row.cells.stdlib.head.content.bind:
              case Inline.Icon(_, alt) => List(alt)
              case _                   => Nil

        . assert(_ == List(t"final class", t"object"))

        test(m"a glyph of the keyword stands in a column before a member's name"):
          val declaration: Api.Declaration = Api.Declaration(Api.Kind.CaseClass)

          val entity: Api.Entity =
            Api.Entity(Designator.Top(t"p"), t"Doc", true, template = declaration)

          val api: Api = Api(sci.Map(entity.key -> entity))
          val resolver: Resolver = Resolver(api, scope)
          val row: Block.Row = ApiPages.row(entity, declaration, true, api, scope, resolver)

          val first: Optional[Inline] = row.cells.stdlib.head.content.head

          first match
            case Inline.Icon(source, alt) => (source.starts(t"<svg"), alt)
            case _                        => (false, t"?")

        . assert(_ == (true, t"case class"))

        test(m"a glyph is drawn on the grid, and its modifiers transform it"):
          val plain: Text = Icons.markup(Api.Kind.Def, Nil)
          val finished: Text = Icons.markup(Api.Kind.Def, List(t"final"))
          val inlined: Text = Icons.markup(Api.Kind.Def, List(t"inline"))
          val translucent: Text = Icons.markup(Api.Kind.Def, List(t"transparent"))
          val hollow: Text = Icons.markup(Api.Kind.Def, List(t"abstract"))
          val slashed: Text = Icons.markup(Api.Kind.Def, List(t"erased"))
          val overridden: Text = Icons.markup(Api.Kind.Def, List(t"override"))
          val placed: Text = Icons.markup(Api.Kind.Def, List(t"infix"))

          ( plain.contains(t"""viewBox="0 0 14 14""""),
            plain.contains(t"<rect"),
            finished.contains(t"""viewBox="0 0 14 14"""") && finished.contains(t"<circle"),
            inlined.contains(t"skewX"),
            translucent.contains(t"<mask"),
            hollow.contains(t"evenodd"),
            slashed.contains(t"rotate(45 7 7)"),
            overridden.contains(t"""y="12.6""""),
            placed.contains(t"""x="10"""") && placed.contains(t"""x="2.5"""") )

        . assert(_ == (true, true, true, true, true, true, true, true, true))

        test(m"a stop and infix squares stand close to each shape"):
          def stop(kind: Api.Kind): Boolean =
            Icons.markup(kind, List(t"final")).contains(t"""viewBox="0 0 14 14"""")

          ( List(Api.Kind.Def, Api.Kind.Val, Api.Kind.Case, Api.Kind.Class).all(stop),
            Icons.markup(Api.Kind.Trait, List(t"infix")).contains(t"""x="4"""") )

        . assert(_ == (true, true))

        test(m"a package is a cube, and a case class's outer ring is closed"):
          val cube: Text = Icons.markup(Api.Kind.Package, Nil)
          val cased: Text = Icons.markup(Api.Kind.CaseClass, Nil)
          (cube.contains(t"stroke"), cube.contains(t"<rect"), cased.contains(t"evenodd"))

        . assert(_ == (true, false, true))

        test(m"into, annotation and java each decorate the glyph"):
          val plain: Text = Icons.markup(Api.Kind.Class, Nil)
          val into: Text = Icons.markup(Api.Kind.Class, List(t"into"))
          val annotation: Text = Icons.markup(Api.Kind.Class, List(t"annotation"))
          val java: Text = Icons.markup(Api.Kind.Class, List(t"java"))

          ( into.contains(t"<polygon") && into.contains(t"""viewBox="-0.5 """),
            annotation.contains(t"""viewBox="0 0 15.5 14""""),
            java != plain && java.contains(t"""viewBox="0 0 14 14"""") )

        . assert(_ == (true, true, true))

        test(m"java and annotation are said by the glyph, not written in a signature"):
          val declaration: Api.Declaration =
            Api.Declaration(Api.Kind.Class, List(t"final", t"java", t"annotation", t"into"))

          declaration.written

        . assert(_ == List(t"final", t"into"))

        test(m"member tables share fixed column widths, the keyword's minor and at the end"):
          ApiPages.table(t"Name", t"Signature", List(Block.Row(Nil))).bind:
            case Block.Table(columns, _, _) =>
              columns.map: (column: Block.Column) =>
                (column.sizing, column.alignment, column.minor)

            case _ =>
              Nil

        . assert: (columns: List[(Block.Sizing, Block.Alignment, Boolean)]) =>
            val expected: List[(Block.Sizing, Block.Alignment, Boolean)] =
              List
                ( (Block.Sizing.Fixed(10), Block.Alignment.End, false),
                  (Block.Sizing.Fixed(26), Block.Alignment.Start, false),
                  (Block.Sizing.Fixed(64), Block.Alignment.Start, false) )

            columns == expected

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
  keyword outline
  keyword two dimensional
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

      test(m"an entry's keywords find it in a search, exactly, fuzzily and in any case"):
        def found(text: Text): List[Text] =
          merged.lay(Nil: List[Text]): (m: (Api, List[Block])) =>
            Query.read(text) match
              case query: Query => m(0).search(query).hits.map(_.entity.key).filter(_.starts(t"sample.")).distinct
              case _            => Nil

        (found(t"outline"), found(t"outlin"), found(t"`Two Dimensional`"), found(t"shape"))

      . assert(_ == (List(t"sample.Shape"), List(t"sample.Shape"), List(t"sample.Shape"), List(t"sample.Shape")))

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
        t"doc/api/sample.tel:25: `.nowhere` is documented here, " +
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
            t"`info`, `detail`, `refer`, `hidden`, `until`, `entry`, `keyword`.",
            t"4: `info` is given more than once here.",
            t"6: This entry's info is already given on its line.",
            t"7: `detial` is not part of an overlay: An entry may hold `import`, `info`, " +
              t"`detail`, `refer`, `hidden`, `until`, `entry`, `keyword`.",
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
              List(Designator(t"java.lang"), Designator(t"scala")),
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
      val sampleToken: Token = Token.plain(t"sample").copy(link = samplePage)
      val dot: Token = Token(t".", Token.Accent.Symbol).copy(chained = true)

      val heading: Block =
        Block.Paragraph(List(Inline.Snippet(Language.Scala, List(sampleToken, dot))))

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

    suite(m"Issues"):
      import codepages.utf8Codepage

      def issue(number: Int, title: Text, open: Boolean = true, names: List[Text] = Nil, updated: Text = t"2026-11-08T10:00:00Z"): Issue =
        Issue(number, title, t"https://github.com/o/r/issues/${number.show}", open, updated, names)

      test(m"the names after `affects`, in backticks, separated by commas, `and`, `or` or lines"):
        Issue.affected(t"This affects `a.B`, `c.D` and `e.F#g`\nor `h.I`; nothing else.")

      . assert(_ == List(t"a.B", t"c.D", t"e.F#g", t"h.I"))

      test(m"every `affects` counts, in any case, once per name, and a list ends at other text"):
        Issue.affected(t"AFFECTS `x.Y` the type. Also affects: `x.Y` and `z.W` which `q.R` uses.")

      . assert(_ == List(t"x.Y", t"z.W"))

      test(m"a fenced block or an HTML comment holding the keyword is not a statement"):
        ( Issue.affected(t"```\nthis affects `x.Y`\n```\n"),
          Issue.affected(t"<!-- affects `x.Y` -->\n"),
          Issue.affected(t"no keyword, but `x.Y` in code") )

      . assert(_ == (Nil, Nil, Nil))

      test(m"a name resolves to its entity's key through the scope, or is unresolved"):
        ( Issue.resolve(List(t"sample.Doc", t"Nope", t"sample.Doc#title"), api0, Scope.document(Nil)),
          Issue.resolve(List(t"Doc"), api0, Scope.document(List(t"sample"))) )

      . assert(_ == ( (List(t"sample.Doc", t"sample.Doc#title"), List(t"Nope")),
                      (List(t"sample.Doc"), Nil) ))

      test(m"the index has each entity's open issues by number, and the names nothing resolves"):
        val issues: List[Issue] =
          List
            ( issue(14, t"later", names = List(t"sample.Doc")),
              issue(12, t"earlier", names = List(t"sample.Doc", t"sample.Gone")),
              issue(13, t"closed", open = false, names = List(t"sample.Doc")) )

        val (index, unresolved) = Issue.index(issues, api0, Scope.document(Nil))
        (index.get(t"sample.Doc").map(_.map(_.number)), unresolved)

      . assert(_ == (Some(List(12, 14)), List((12, t"sample.Gone"))))

      test(m"a GitHub payload decodes to issues, pull requests left out"):
        val json: Text = t"""[
          {"number": 12, "title": "First", "state": "open", "html_url": "https://github.com/o/r/issues/12",
           "updated_at": "2026-11-08T10:00:00Z", "body": "This affects `sample.Doc`."},
          {"number": 13, "title": "A PR", "state": "open", "html_url": "https://github.com/o/r/pull/13",
           "updated_at": "2026-11-08T11:00:00Z", "body": null, "pull_request": {"url": "x"}},
          {"number": 14, "title": "Closed", "state": "closed", "html_url": "https://github.com/o/r/issues/14",
           "updated_at": "2026-11-08T12:00:00Z", "body": null}
        ]"""

        import strategies.throwUnsafely
        val payloads: List[GitHubIssues.Payload] = json.read[Json].as[List[GitHubIssues.Payload]]
        payloads.bind { p => GitHubIssues.issue(p).let(List(_)).or(Nil) }.map: issue =>
          (issue.number, issue.open, issue.url, issue.affects)

      . assert(_ == List( (12, true, t"https://github.com/o/r/issues/12", List(t"sample.Doc")),
                          (14, false, t"https://github.com/o/r/issues/14", Nil) ))

      test(m"a fetch merges into the state: closed dropped, changed replaced, new added, since advanced"):
        val before: Sync.State =
          Sync.State(t"github o/r", t"W/\"1\"", t"2026-11-08T09:00:00Z", List(issue(12, t"old"), issue(13, t"keep")))

        val fetched: List[Issue] =
          List( issue(12, t"renamed", updated = t"2026-11-08T12:00:00Z"),
                issue(13, t"keep", open = false, updated = t"2026-11-08T11:00:00Z"),
                issue(15, t"new", updated = t"2026-11-08T10:30:00Z") )

        val after: Sync.State = Sync.merge(before, fetched, t"W/\"2\"")
        val same: Sync.State = Sync.merge(before, Nil, Unset)
        (after.issues.map { i => (i.number, i.title) }, after.since, after.etag, same == before)

      . assert(_ == (List((12, t"renamed"), (15, t"new")), t"2026-11-08T12:00:00Z", t"W/\"2\"", true))

      test(m"a sync's state round-trips through its TEL file"):
        val plain: Sync.State = Sync.State(t"github o/r", Unset, t"2026-11-08T09:00:00Z", List(issue(12, t"An issue, with: punctuation", names = List(t"sample.Doc", t"x.Y#z"))))
        val etagged: Sync.State = plain.copy(etag = t"W/\"abc\"")
        ( Sync.State.decode(Sync.State.encode(plain)) == plain,
          Sync.State.decode(Sync.State.encode(etagged)) == etagged,
          Sync.State.encode(plain).contains(t"since 2026-11-08T09:00:00Z") )

      . assert(_ == (true, true, true))

      test(m"a target is the words of a sync line, and names its file"):
        ( Sync.Target.parse(List(t"github", t"propensive/fuel")),
          Sync.Target.parse(List(t"github", t"fuel")),
          Sync.Target.parse(List(t"gitlab", t"a/b")),
          Sync.Target.parse(List(t"github", t"propensive/fuel")).let(_.slug) )

      . assert(_ == (Sync.Target.Github(t"propensive", t"fuel"), Unset, Unset, t"github-propensive-fuel"))

      test(m"a page with issues has their numbers after its title and a list of them at its foot"):
        api0(t"sample.Doc").let: (doc: Api.Entity) =>
          val scope: Scope = Scope.of(doc, Nil)
          val page: List[Block] = ApiPages.page(api0, doc, Resolver(api0, scope), scope, Nil)
          val shown: List[Block] = ApiPages.withIssues(page, List(issue(12, t"First"), issue(15, t"Second")))

          def external(inline: Inline): Boolean = inline match
            case Inline.Link(Inline.Destination.External(_), _) => true
            case _                                               => false

          val top: Optional[Text] = shown.stdlib.lift(2).collect { case Block.Paragraph(content) => Blocks.plain(content) }.getOrElse(Unset)
          val links: Int = shown.stdlib.lift(2).collect { case Block.Paragraph(content) => content.stdlib.count(external) }.getOrElse(0)
          val foot: List[Text] = shown.reverse.keep(2).reverse.map:
            case Block.Heading(3, content, _) => Blocks.plain(content)
            case Block.Listing(_, items)      => t"listing of ${items.size.show}"
            case _                            => t"?"

          (shown.head == page.head, shown.stdlib.lift(1) == page.stdlib.lift(1), top, links, foot, ApiPages.withIssues(page, Nil) == page)

      . assert(_ == (true, true, t"Open issues: #12 #15", 2, List(t"Issues", t"listing of 2"), true))

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
          case Block.Code(_, _, _, _, _, id, _, _) :: _ => id
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

      test(m"a relative link leaving its section leads to the section holding its file"):
        val sections: List[(Text, Text)] =
          List((t"modules", t"/r/doc/modules"), (t"philosophy", t"/r/doc/philosophy"))

        def placed(section: Text, path: Text, text: Text): Doc =
          Doc.parse
            ( path, text, Checker.tokenized, false, Unset, Linker.none, section = section,
              root = t"/r/doc/$section", sections = sections )

        val text: Text = t"[s](../../philosophy/safety.md#why)\n\n[n](../../nope/x.md)\n"
        val a: Doc = placed(t"modules", t"guide/a.md", text)

        val s: Doc = placed(t"philosophy", t"safety.md", t"# Safety\n\n## Why\n")

        val corpus: Corpus =
          Corpus
            ( List
                ( Corpus.Section(t"modules", t"/r/doc/modules", List(a)),
                  Corpus.Section(t"philosophy", t"/r/doc/philosophy", List(s)) ) )

        (links(a), lines(Links.check(corpus, Api.empty, Glossary.none)))

      . assert:
          _ == ( List
                  ( Inline.Destination.Local(t"/doc/philosophy/safety.md", t"why"),
                    Inline.Destination.External(t"../../nope/x.md") ),
                 List(3) )

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
  keyword  typeclass
  keyword  interface
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

      test(m"a term's keywords find it in a search, with its name, in any case"):
        def found(text: Text): List[Text] = Query.read(text) match
          case query: Query => api0.search(query, glossary).terms.map(_.name)
          case _            => Nil

        (found(t"typeclass"), found(t"Interface"), found(t"monad"), found(t"nothing"))

      . assert(_ == (List(t"type class"), List(t"type class"), List(t"monad"), Nil))

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

    suite(m"Latest"):
      import java.util.concurrent as juc
      import scala.jdk.CollectionConverters.*

      // The requests done, in order, with a latch for each request a test waits on; the
      // first request is held, once begun, until `release` opens.
      final class Recorder(held: Text):
        val done: juc.ConcurrentLinkedQueue[Text] = juc.ConcurrentLinkedQueue()
        val begun: juc.CountDownLatch = juc.CountDownLatch(1)
        val release: juc.CountDownLatch = juc.CountDownLatch(1)
        val latches: juc.ConcurrentHashMap[Text, juc.CountDownLatch] = juc.ConcurrentHashMap()

        def latch(request: Text): juc.CountDownLatch =
          latches.computeIfAbsent(request, _ => juc.CountDownLatch(1)).nn

        val latest: Latest[Text] = Latest: (request: Text) =>
          if request == held then
            begun.countDown()
            release.await(10, juc.TimeUnit.SECONDS)

          // A request which fails, as a search might: the number is not one.
          if request == t"fail" then java.lang.Integer.parseInt("fail")
          done.add(request)
          latch(request).countDown()

        def await(request: Text): Boolean = latch(request).await(10, juc.TimeUnit.SECONDS)
        def list: List[Text] = List.from(done.asScala)

      test(m"keystrokes made during a search are searched for once, as the last of them"):
        val recorder = Recorder(t"a")
        recorder.latest(t"a")
        recorder.begun.await(10, juc.TimeUnit.SECONDS)
        List(t"ab", t"abc", t"abcd", t"abcde").each(recorder.latest(_))
        recorder.release.countDown()
        recorder.await(t"abcde")
        recorder.list

      . assert(_ == List(t"a", t"abcde"))

      test(m"a request made once the worker is idle is done, as is one after a failure"):
        val recorder = Recorder(t"")
        recorder.latest(t"x")
        recorder.await(t"x")
        recorder.latest(t"fail")
        recorder.latest(t"y")
        recorder.await(t"y")
        recorder.latest(t"z")
        recorder.await(t"z")
        recorder.list.filter(_ != t"x")

      . assert(_ == List(t"y", t"z"))

    suite(m"Tables"):
      // A document of a heading, a line of prose and blank lines, then the fence, which opens on
      // the file's fifth line: its first line of content is the sixth.
      def doc(fence: Text): Doc = Doc.parse(t"a.md", t"# Title\n\nProse.\n\n```tabular\n$fence```\n")

      def table(doc: Doc): Optional[Block.Table] =
        doc.blocks.seek { case _: Block.Table => true; case _ => false }.let:
          case table: Block.Table => table
          case _                  => Unset

      def text(content: List[Inline]): Text = Inline.plain(content)

      // Each problem, as its line and column in the file and its message.
      def problems(doc: Doc): List[(Optional[Int], Optional[Int], Text)] = doc.problems.map:
        case Block.Diagnostic(_, location, message, _, _) =>
          val said: Text = message.map { case Block.Paragraph(content) => text(content); case _ => t"" }.join
          (location.let(_.line), location.let(_.column), said)

        case _ =>
          (Unset, Unset, t"")

      def lines(doc: Doc): List[Optional[Int]] = problems(doc).map(_(0))

      val planets: Text = t"""column  Planet
column  Distance
column  Moons
column  Notes
row  Mercury  0.39  0  Closest to the Sun
row  Venus de Milo
  cell
  cell  0
  cell  *Turns* backwards
row  Earth  1.00  1  See [the Moon](https://example.com/moon)
"""

      test(m"a tabular fence is a table: its columns' titles, and its rows' cells"):
        table(doc(planets)).let: (table: Block.Table) =>
          ( table.columns.map { column => text(column.title) },
            table.rows.map(_.cells.map { cell => text(cell.content) }) )

      . assert(_ == ( List(t"Planet", t"Distance", t"Moons", t"Notes"),
                      List
                        ( List(t"Mercury", t"0.39", t"0", t"Closest to the Sun"),
                          List(t"Venus de Milo", t"", t"0", t"Turns backwards"),
                          List(t"Earth", t"1.00", t"1", t"See the Moon") ) ))

      test(m"a field is the same table whether it is written as an atom or as a child"):
        val atoms: Text = t"column  Name  end\ncolumn  Other\nrow  a b  c\n"

        val children: Text =
          t"column\n  title  Name\n  align end\ncolumn\n  title  Other\nrow\n  cell  a b\n  cell  c\n"

        val (first, second) = (table(doc(atoms)), table(doc(children)))
        (first.present, first == second)

      . assert(_ == (true, true))

      test(m"a cell is Markdown phrasing"):
        table(doc(planets)).let: (table: Block.Table) =>
          table.rows.map(_.cells.reverse.head.let(_.content.head).or(Unset)).map:
            case Inline.Emphasis(_) => t"emphasis"
            case Inline.Textual(_)  => t"text"
            case _                  => t"other"

      . assert(_ == List(t"text", t"emphasis", t"text"))

      test(m"a row with fewer cells than columns is filled out with empty ones"):
        table(doc(t"column  A\ncolumn  B\ncolumn  C\nrow  x\n")).let: (table: Block.Table) =>
          table.rows.map(_.cells.map { cell => text(cell.content) })

      . assert(_ == List(List(t"x", t"", t"")))

      test(m"a numeric column is set at the end, unless the column says how it is aligned"):
        val aligned: Text = planets
          . sub(t"column  Moons\n", t"column  Moons  start\n")
          . sub(t"column  Notes\n", t"column\n  title  Notes\n  align end\n")

        table(doc(aligned)).let: (table: Block.Table) =>
          table.columns.map { column => (column.numeric, column.alignment) }

      . assert(_ == List( (false, Block.Alignment.Start),
                          (true, Block.Alignment.End),
                          (false, Block.Alignment.Start),
                          (false, Block.Alignment.End) ))

      test(m"a caption, anywhere, is Markdown phrasing beneath the table"):
        table(doc(t"caption  The *inner* planets\n" + planets)).let: (table: Block.Table) =>
          table.caption.let(text(_))

      . assert(_ == t"The inner planets")

      test(m"a table with a caption of several inlines round-trips as a fragment's output"):
        val compiled: Doc = doc(planets + t"caption  The *inner* planets\n")
        val fragment = Output.Fragment(compiled.blocks, Nil, true, Nil, Nil, t"", 0)
        Output.Fragment.decode(Output.Fragment.encode(fragment)).let(_ == fragment).or(false)

      . assert(_ == true)

      test(m"a well-formed table has no problems"):
        problems(doc(t"caption  Planets\n" + planets))

      . assert(_ == Nil)

      test(m"a keyword the schema does not have is a fault at its line and column"):
        val compiled: Doc = doc(t"column  A\nrow  x\nheading  B\n")
        val shown: Boolean = compiled.blocks.exists { case _: Block.Code => true; case _ => false }
        (table(compiled).absent, shown, problems(compiled).map { (line, column, _) => (line, column) })

      . assert(_ == (true, true, List((8, 1))))

      test(m"every fault in a table is reported, each at its place"):
        lines(doc(t"column  A\nrow  x\nheading  B\ncell  y\n"))

      . assert(_ == List(8, 9))

      test(m"a value with spaces in it after one space has more values than it takes"):
        problems(doc(t"column  A\nrow\n  cell Closest to the Sun\n")).map { (line, _, said) => (line, said) }

      . assert(_ == List((8, t"This has more values than it takes: a value with spaces in it follows two spaces, as `title  Known for`.")))

      test(m"a column without a title is a fault at the column"):
        problems(doc(t"column  A\ncolumn\n  align end\nrow  x\n")).map { (line, _, said) => (line, said) }

      . assert(_ == List((7, t"This column has no `title`.")))

      test(m"columns come together, and rows together"):
        lines(doc(t"column  A\nrow  x\ncolumn  B\nrow  y\n")).size > 0

      . assert(_ == true)

      test(m"a table needs a column and a row, at the fence's line"):
        (problems(doc(t"caption  Nothing\n")), lines(doc(t"column  A\n")))

      . assert(_ == ( List((5, Unset, t"A table needs a `column`."), (5, Unset, t"A table needs a `row`.")),
                      List(5) ))

      test(m"a column is aligned start or end, and a row has no more cells than columns"):
        ( problems(doc(t"column  A  right\nrow  x\n")).map { (line, _, said) => (line, said) },
          problems(doc(t"column  A\nrow  x  y\n")).map { (line, _, said) => (line, said) } )

      . assert(_ == ( List((6, t"A column is aligned `start` or `end`, not `right`.")),
                      List((7, t"This row has 2 cells, but the table has 1 columns.")) ))

    suite(m"Sections"):
      test(m"a fixture is found by the document's section and path before its file name"):
        val directory = java.nio.file.Files.createTempDirectory("fuel-fixtures").nn
        val nested = java.nio.file.Files.createDirectories(directory.resolve("modules")).nn
        java.nio.file.Files.writeString(directory.resolve("logging.scala"), "val shared = 1")
        java.nio.file.Files.writeString(nested.resolve("logging.scala"), "val own = 1")
        val regime = Compilation.Regime(fixtures = directory.toString.nn.tt)

        val own: Optional[Text] =
          Compilation.fixture(regime, t"modules", t"logging.md").let(_(1))

        val shared: Optional[Text] =
          Compilation.fixture(regime, t"standards", t"logging.md").let(_(1))

        (own, shared)

      . assert(_ == (t"val own = 1", t"val shared = 1"))

      test(m"a section is named by its directory's last segment"):
        Compilation.name(t"/home/me/project/doc/modules/")

      . assert(_ == t"modules")
