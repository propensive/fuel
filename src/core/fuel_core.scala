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

// Excluded from the umbrella: `Language` and `Token`, which are Pyrocosm's here (the model's own,
// which `Block.Code` carries), not cosmopolite's and harlequin's.
import soundness.{Language as _, Token as _, *}

import dysasymptotics.{linearAccess, linearSize}
import pyrocosm.{Block, Inline, Language, Token, Tone}

object Embedding:
  // A Scala fence, highlighted by the checker — typechecked, in a compile with a classpath —
  // with the preamble a ` ```scala fuel ` fence carries honoured: its context, its marks, the
  // fragments it follows (whose text precedes it in the compile), what its errors mean, how
  // far the compiler takes it, and its language when it is not Scala's (a ` ```java fuel `
  // fence comes here for its preamble, and is highlighted, not compiled). One instance per
  // document: the fragments seen so far are what `follows` names, and `problems` is what the
  // compile reports — a diagnostic each, at its place in the file.
  //
  // In a *session*, every fragment follows the ones before it, as a REPL's do: each is
  // compiled once, as an object of its own, and a later one's object imports the objects of
  // those which joined before it (the document's fixture first), whose names the warm
  // session's symbol table retains; so a bare expression is a statement, a definition is in
  // scope from then on, and a fragment costs its own size rather than the whole document's.
  // `restart` begins again. An object opens at column zero, so a diagnostic's column is the
  // fragment's own and only its line shifts.
  final class ScalaCode
    ( checker: Checker,
      path:    Text,
      session: Boolean                = false,
      fixture: Optional[(Text, Text)] = Unset )
  extends Embedding:

    // What a fragment left to the session: its object, with the names it defines (so that a
    // later definition of the same name excludes it from the import) and its own top-level
    // imports, which an import of the object does not carry; or, when it failed, its imports
    // alone. A fragment's imports are replayed at file scope before every later object, where
    // the objects' members, imported inside it, outrank them.
    private enum Joined:
      case Object(name: Text, defined: List[Text], imports: List[Text])
      case Imports(lines: List[Text])

    private var fragments: List[(Text, Text)] = Nil
    private var history: List[Joined] = Nil
    private var features: List[Text] = Nil
    private var prepared: Boolean = false
    private var count: Int = 0
    private var problems0: List[Block] = Nil

    def problems: List[Block] = problems0.reverse

    private def fragment(name: Text): Text =
      fragments.seek(_(0) == name).lay(t""): (entry: (Text, Text)) => entry(1)

    private def report(location: Block.Location, text: Text): Unit =
      problems0 = Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text))) :: problems0

    // What is compiled around a fragment: the text before it, whose length shifts every
    // diagnostic and whose lines are dropped from the rendering; the text after it; and the
    // context harlequin wraps it in, when the framing does not do so itself.
    private case class Framing
      ( head: Text, foot: Text, wrapper: Int, context: Optional[harlequin.Scala.Context] ):

      def shift: Int = head.length + wrapper
      def unit(code: Text): Text = head + code + foot

      // The fragment's own lines, out of the unit's: all of them when nothing framed it (the
      // tokenized path, or harlequin having dropped its wrapper), else those after the head.
      def own(all: List[Block.Line], count: Int): List[Block.Line] =
        if all.size == count then all
        else if foot == t"" then all.skip(all.size - count)
        else all.skip(head.cut(t"\n").size - 1).keep(count)

    private def objectName(index: Int): Text = t"fuel_fragment_${index.show}"

    // The names a fragment defines at its top level, by their declarations, so that a later
    // fragment defining one of them again hides the earlier from its import — as a REPL
    // lets a name be redefined — rather than leaving the two ambiguous.
    private val declaration: java.util.regex.Pattern =
      java.util.regex.Pattern.compile("""^(?:(?:final|lazy|private|protected|inline|transparent|opaque|infix|open|case|abstract|sealed|override|implicit)\s+)*(?:val|var|def|object|class|trait|type|enum|given)\s+(?:\[[^\]]*\]\s*)?([A-Za-z_][\w$]*|`[^`]+`)""").nn

    private def defined(code: Text): List[Text] =
      code.cut(t"\n").bind: (line: Text) =>
        val matcher = declaration.matcher(line.s).nn
        if matcher.find() then List(matcher.group(1).nn.tt) else Nil

    // The top-level imports of a fragment, which its object would not carry.
    private def imported(code: Text): List[Text] = code.cut(t"\n").filter(_.starts(t"import "))

    // The imports of what has joined the session so far, as those to replay at file scope (every
    // fragment's own) and those inside the object (each joined object's members, less any name
    // a later object defines again, and its givens).
    private def replayed: (List[Text], List[Text]) =
      // Walking newest first, a name defined by a newer object hides the older's.
      def walk(rest: List[Joined], seen: List[Text]): (List[Text], List[Text]) = rest match
        case Joined.Object(name, defined, imports) :: tail =>
          val hidden: List[Text] = defined.filter(seen.has(_)).map { (name: Text) => t"$name as _" }
          val selectors: Text = (hidden + List(t"*", t"given")).join(t", ")
          val (file, inner) = walk(tail, seen + defined)
          (imports + file, List(t"import $name.{$selectors}") + inner)

        case Joined.Imports(lines) :: tail =>
          val (file, inner) = walk(tail, seen)
          (lines + file, inner)

        case _ =>
          (Nil: List[Text], Nil: List[Text])

      val (file, inner) = walk(history, Nil)
      (file.reverse, inner.reverse)

    // A session's framing: the language imports, this fragment's object with the session's
    // imports inside it, then the fragment, wrapped as a term or a type when its context says,
    // and the closing brace; outside a session, the fragments it follows, with harlequin's own
    // wrapping.
    private def framing
      ( preamble: Preamble, context: Optional[harlequin.Scala.Context], wrapper: Int )
    :   Framing =

      if !session then Framing(preamble.follows.map(fragment(_)).join(t"", t"\n", t"\n"), t"", wrapper, context)
      else
        val followed: List[Text] = preamble.follows.map(fragment(_))
        val (fileScope, inner) = replayed
        val imports: Text = (features.map { (feature: Text) => t"import scala.language.$feature" } + fileScope).map { (line: Text) => t"$line\n" }.join(t"")
        val opening: Text = t"object ${objectName(count)} {\n"
        val body: Text = (inner + followed).map { (text: Text) => t"$text\n" }.join(t"")

        val (open, close): (Text, Text) = context match
          case harlequin.Scala.Context.Term => (t"val x${count.show} = {\n", t"\n}")
          case harlequin.Scala.Context.Type => (t"type X${count.show} =\n", t"")
          case _                            => (t"", t"")

        Framing(imports + opening + body + open, close + t"\n}", 0, Unset)

    // The fixture, compiled before the document's first fragment: it opens the session when it
    // compiles, and is a problem at its own place, and left out, when it does not.
    private def prepare(): Unit =
      if session && !prepared then
        prepared = true

        fixture.let: (entry: (Text, Text)) =>
          val (fixturePath, text) = entry
          val framed: Framing = Framing(t"object ${objectName(0)} {\n", t"\n}", 0, Unset)

          highlight(t"scala", text, framed, Preamble.Mode.Typecheck, checker.compiles) match
            case source: SourceCode =>
              val errors: List[harlequin.Diagnostic] =
                source.diagnostics.filter(_.importance == harlequin.Diagnostic.Importance.Error)

              if errors.nil && checker.enter(t"${objectName(0)}.scala", framed.unit(text))
              then history = List(Joined.Object(objectName(0), defined(text), imported(text)))
              else if errors.nil then report(Block.Location(fixturePath), t"The fixture could not be entered into the session.")
              else
                errors.each: (diagnostic: harlequin.Diagnostic) =>
                  val from: Int = (diagnostic.span.offset.let(_.n0).or(0) - framed.shift).max(0)
                  val note: Block.Note = Notes.of(text, from, from, Block.Note.Style.Erroneous)
                  report(Block.Location(fixturePath, note.line + 1, note.start + 1), diagnostic.message)

            case error: Throwable =>
              report(Block.Location(fixturePath), t"The fixture could not be compiled: ${error.toString.tt}")

            case _ =>
              ()

    // A fragment joins the session when it compiled as its preamble asked, at full depth, and
    // stands alone; one which failed leaves only its imports, so the names they bring stay in
    // scope for what follows, as a checker which runs fragments does.
    private def join(code: Text, framing: Framing, failed: Boolean, preamble: Preamble, mode: Preamble.Mode): Unit =
      if session && mode != Preamble.Mode.Parse && preamble.errors == Preamble.Errors.Fail && preamble.scalaContext.absent then
        if !failed && (!checker.compiles || checker.enter(t"${objectName(count)}.scala", framing.unit(code)))
        then history = Joined.Object(objectName(count), defined(code), imported(code)) :: history
        else
          val imports: List[Text] = code.cut(t"\n").filter(_.trim.starts(t"import "))
          if !imports.nil then history = Joined.Imports(imports) :: history

    // The lines of a fragment marked as not compiling, which are compiled alone, each in its
    // place, and must fail.
    private val negative: java.util.regex.Pattern =
      java.util.regex.Pattern.compile("""//\s*(?:does\s+not|doesn't|will\s+not|won't|cannot|can't)\s+compile\b""", java.util.regex.Pattern.CASE_INSENSITIVE).nn

    private def negatives(code: Text): List[Int] =
      code.cut(t"\n").indexed.filter { (line: Text, index: Ordinal) => negative.matcher(line.s).nn.find() }.map(_(1).n0)

    // The code with the marked lines blanked — their numbering kept — but for `except`.
    private def blank(code: Text, marked: List[Int], except: Optional[Int]): Text =
      code.cut(t"\n").indexed.map: (line: Text, index: Ordinal) =>
        if marked.has(index.n0) && !except.let(_ == index.n0).or(false) then t"" else line
      . join(t"\n")

    def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
      if info.prim != t"scala" && !info.has(t"fuel") then Unset else
        prepare()

        val (header, code0) =
          if info.has(t"fuel") then Preamble.split(content) else (Unset, content)

        val code: Text = trimmed(code0)
        count += 1

        // The file's line, counted from one, on which the code begins: after the fence and
        // the preamble with its `##`.
        val preambleLines: Int = header.lay(0) { (text: Text) => text.cut(t"\n").size + 1 }
        val start: Int = fence + 2 + preambleLines

        // A preamble that is not TEL is reported at its place, and the fence read as if it
        // had none.
        val preamble: Preamble = header.lay(Preamble.empty): (text: Text) =>
          Preamble.parse(text) match
            case preamble: Preamble => preamble

            case error: Tel.Error =>
              problems0 = Preamble.diagnostic(error, path, text, fence + 1) :: problems0
              Preamble.empty

        if preamble.restart then history = Nil
        features = features + preamble.features.filter { (feature: Text) => !features.has(feature) }

        val id: Text = preamble.id.or(t"fragment-${count.show}")
        val context: Optional[harlequin.Scala.Context] = preamble.scalaContext
        val language: Text = preamble.language.or(info.prim.or(t"scala"))
        val mode: Preamble.Mode = preamble.mode.or(Preamble.Mode.Typecheck)

        val wrapper: Int = context match
          case harlequin.Scala.Context.Term => 10  // `val x = {\n`
          case harlequin.Scala.Context.Type => 9   // `type X =\n`
          case _                            => 0

        val checked: Boolean = checker.compiles && preamble.errors != Preamble.Errors.Ignore
        val framed: Framing = framing(preamble, context, wrapper)

        // The preamble's frames which have code: one without its `code` child is a problem
        // at its line in the file — the nth `frame` line of the preamble, since a TEL node
        // does not know its line — and is dropped.
        val frameLines: List[Int] =
          header.lay(Nil) { (text: Text) => text.cut(t"\n").indexed.filter(_(0).starts(t"frame")).map(_(1).n0) }

        val described: List[Preamble.Frame] =
          preamble.frames.indexed.bind: (frame: Preamble.Frame, index: Ordinal) =>
            if frame.code.present then List(frame) else
              val location = Block.Location(path, fence + 2 + frameLines.at(index).or(0))
              report(location, t"Frame ${(index.n0 + 1).toString.tt} has no `code`.")
              Nil

        // An animated fence: its frames, then the code after `##` as the last if there is any.
        val frames: List[Preamble.Frame] =
          if preamble.frames.nil then Nil
          else if code.trim == t"" then described
          else described + List(Preamble.Frame(code))

        if !frames.nil then animated(frames, preamble, language, framed, mode, checked, id, start)
        else if language == t"scala" then embedPlain(code, preamble, framed, mode, checked, id, start)
        else List(Block.Code(Language(language), highlight(language, code, Framing(t"", t"", 0, Unset), mode, false) match
          case source: SourceCode => modelLines(source)
          case _                  => lines(code), notes(code, preamble.marks), start))

    // One fragment highlighted in a language: Scala through the checker (in its framing, as
    // far as its mode asks, when checking), Java through harlequin alone, another language
    // not at all; or the failure, which is shown plain with a diagnostic.
    private def highlight
      ( language: Text,
        code:     Text,
        framing:  Framing,
        mode:     Preamble.Mode,
        checked:  Boolean )
    :   Optional[SourceCode | Throwable] =

      language match
        case t"scala" =>
          try
            if checked then checker.check(framing.unit(code), mode, framing.context)
            else Checker.tokenized.check(code, mode, framing.context)
          catch case error: Throwable => error

        case t"java" =>
          try harlequin.Java.highlight(code) catch case error: Throwable => error

        case _ =>
          Unset

    // The notes for the marks which find their range in a fragment's text; a mark which does
    // not is nothing.
    private def notes(code: Text, marks: List[Preamble.Mark]): List[Block.Note] =
      marks.bind[List[Block.Note], Block.Note, List[Block.Note]]:
        (mark: Preamble.Mark) =>
          mark.selection.findIn(code).lay(Nil: List[Block.Note]): (range: (Int, Int)) =>
            val caption: List[Inline] = mark.caption.lay(Nil: List[Inline])(Inline.text(_))
            List(Notes.of(code, range(0), range(1), mark.style, caption))

    // Each frame highlighted (and compiled, when checking and Scala) on its own, in its own
    // language or the block's, then aligned into one animation with what the preamble says
    // of each slide: its label, hold, time, language when it differs from the block's, and
    // notes — the preamble's marks, which apply in every frame they match, and the frame's
    // own. The block's lines and notes are the first frame's. A frame that does not compile
    // is a problem naming it, at the fragment's place; the fragment a later one may follow,
    // and which joins a session, is the last Scala frame's code.
    private def animated
      ( frames:   List[Preamble.Frame],
        preamble: Preamble,
        language: Text,
        framing:  Framing,
        mode:     Preamble.Mode,
        checked:  Boolean,
        id:       Text,
        start:    Int )
    :   List[Block] =

      val built: List[(List[Block.Line], Block.Slide.Detail)] =
        frames.indexed.map: (frame: Preamble.Frame, index: Ordinal) =>
          val code: Text = frame.code.or(t"")
          val own: Text = frame.language.or(language)
          val scala: Boolean = own == t"scala"
          val number: Text = (index.n0 + 1).toString.tt

          val frameLines: List[Block.Line] = highlight(own, code, framing, mode, checked && scala) match
            case error: Throwable =>
              report(Block.Location(path, start), t"Frame $number could not be highlighted: ${error.toString.tt}")
              lines(code)

            case source: SourceCode =>
              if checked && scala && preamble.errors == Preamble.Errors.Fail then
                source.diagnostics.each: (diagnostic: harlequin.Diagnostic) =>
                  if diagnostic.importance == harlequin.Diagnostic.Importance.Error then
                    report(Block.Location(path, start), t"Frame $number: ${diagnostic.message}")

              framing.own(modelLines(source), code.cut(t"\n").size)

            case _ =>
              lines(code)

          val detail: Block.Slide.Detail =
            Block.Slide.Detail
              ( frame.label,
                frame.hold,
                frame.at,
                if own == language then Unset else Language(own),
                notes(code, preamble.marks + frame.marks) )

          (frameLines, detail)

      val followed: Text =
        frames.reverse.seek { (frame: Preamble.Frame) => frame.language.or(language) == t"scala" }.let(_.code).or(t"")

      fragments = (id, followed) :: fragments
      join(followed, framing, false, preamble, mode)

      val frameLines: List[List[Block.Line]] = built.map(_(0))
      val details: List[Block.Slide.Detail] = built.map(_(1))

      val animation: Block.Animation =
        Block.Animation.of(frameLines, details).copy(autoplay = preamble.autoplay, loop = preamble.loop)

      List(Block.Code(Language(language), frameLines.prim.or(Nil), details.prim.lay(Nil)(_.notes), start, animation))

    private def embedPlain
      ( code:     Text,
        preamble: Preamble,
        framing:  Framing,
        mode:     Preamble.Mode,
        checked:  Boolean,
        id:       Text,
        start:    Int )
    :   List[Block] =

        // The lines marked as not compiling are blanked out of the fragment proper, so that
        // it compiles without them, and compiled alone, each in its place, after it; a
        // fragment which is only parsed cannot judge them, so they stay as they are.
        val marked: List[Int] = if checked && mode != Preamble.Mode.Parse then negatives(code) else Nil
        val positive: Text = if marked.nil then code else blank(code, marked, Unset)

        // A fragment the highlighter cannot handle (harlequin overflows on an expression
        // nested ten deep, propensive/soundness#2284) is shown plain, with a diagnostic, and
        // the rest of the document goes on.
        val attempt: Optional[SourceCode | Throwable] = highlight(t"scala", positive, framing, mode, checked)

        fragments = (id, positive) :: fragments

        attempt match
          case error: Throwable =>
            report(Block.Location(path, start), t"The fragment could not be highlighted: ${error.toString.tt}")
            List(Block.Code(Language.Scala, lines(code), Nil, start))

          case source: SourceCode =>
            val (block, reports, failed) = embedSource(source, positive, framing, checked, preamble, start)
            join(positive, framing, failed, preamble, mode)

            val restored: Block.Code = marked.fold(block): (block: Block.Code, index: Int) =>
              negativeLine(block, code, marked, index, framing, mode, start)

            restored :: reports

          case _ =>
            List(Block.Code(Language.Scala, lines(code), Nil, start))

    // One line marked as not compiling, compiled in its place: it must fail, and its first
    // error is a note on it, in the block whose line is now taken from that run, so that its
    // tokens are typed too; a line which compiles is a problem.
    private def negativeLine
      ( block:   Block.Code,
        code:    Text,
        marked:  List[Int],
        index:   Int,
        framing: Framing,
        mode:    Preamble.Mode,
        start:   Int )
    :   Block.Code =

      val restored: Text = blank(code, marked, index)
      val text: Text = code.cut(t"\n").skip(index).prim.or(t"")
      val count: Int = code.cut(t"\n").size

      def replaced(line: Block.Line): List[Block.Line] =
        block.lines.indexed.map { (old: Block.Line, position: Ordinal) => if position.n0 == index then line else old }

      highlight(t"scala", restored, framing, mode, true) match
        case source: SourceCode =>
          val line: Block.Line = framing.own(modelLines(source), count).skip(index).prim.or(Block.Line(List(Token.plain(text))))
          val error: Optional[harlequin.Diagnostic] = source.diagnostics.seek(_.importance == harlequin.Diagnostic.Importance.Error)

          error.lay(block.copy(lines = replaced(line))): (diagnostic: harlequin.Diagnostic) =>
            val from: Int = diagnostic.span.offset.let(_.n0).or(0) - framing.shift
            val length: Int = diagnostic.span.length.or(0)
            val caption: List[Inline] = Inline.text(diagnostic.message)
            val placed: Block.Note = Notes.of(restored, from.max(0), from.max(0) + length, Block.Note.Style.Erroneous, caption)

            val note: Block.Note =
              if from >= 0 && placed.line == index then placed
              else Block.Note(index, 0, text.length, Block.Note.Style.Erroneous, caption)

            block.copy(lines = replaced(line), notes = block.notes + List(note))

          . also:
            if error.absent then
              report(Block.Location(path, start + index), t"This line is marked `// does not compile`, but it compiled.")

        case _ =>
          block

    private def embedSource
      ( source:   SourceCode,
        code:     Text,
        framing:  Framing,
        checked:  Boolean,
        preamble: Preamble,
        start:    Int )
    :   (Block.Code, List[Block], Boolean) =

      // The lines of this fragment alone, out of the unit's.
      val lines: List[Block.Line] = framing.own(modelLines(source), code.cut(t"\n").size)

      // Each diagnostic as a note on the fragment, and as a diagnostic of its own after it,
      // located in the file, with the lines around it.
      val reported: List[(Block.Note, Block, Boolean)] =
        if !checked then Nil else
          source.diagnostics.bind[List[(Block.Note, Block, Boolean)], (Block.Note, Block, Boolean), List[(Block.Note, Block, Boolean)]]:
            (diagnostic: harlequin.Diagnostic) =>
              val from: Int = diagnostic.span.offset.let(_.n0).or(0) - framing.shift
              val length: Int = diagnostic.span.length.or(0)

              if from < 0 then Nil else
                val style: Block.Note.Style = diagnostic.importance match
                  case harlequin.Diagnostic.Importance.Error => Block.Note.Style.Erroneous
                  case _                                     => Block.Note.Style.Caution

                val tone: Tone = diagnostic.importance match
                  case harlequin.Diagnostic.Importance.Error   => Tone.Failure
                  case harlequin.Diagnostic.Importance.Warning => Tone.Warning
                  case _                                       => Tone.Info

                val note: Block.Note = Notes.of(code, from, from + length, style)
                val location = Block.Location(path, start + note.line, note.start + 1)
                val excerpt: Block.Code = Block.Code.excerpt(Language.Scala, lines, List(note))
                val message: List[Block] = List(Block.paragraph(diagnostic.message))
                val numbered: Block.Code = excerpt.copy(start = excerpt.start.let(_ + start - 1))
                val shown: Block.Note = note.copy(caption = Inline.text(diagnostic.message))
                val report: Block = Block.Diagnostic(tone, location, message, List(numbered))
                List((shown, report, tone == Tone.Failure))

      val errors: List[Block] = reported.filter(_(2)).map(_(1))

      preamble.errors match
        case Preamble.Errors.Fail =>
          errors.each { (error: Block) => problems0 = error :: problems0 }

        case Preamble.Errors.Show =>
          if checked && errors.nil then
            report(Block.Location(path, start), t"This fragment is marked `errors show`, but it compiled.")

        case _ =>
          ()

      val block: Block.Code = Block.Code(Language.Scala, lines, reported.map(_(0)) + notes(code, preamble.marks), start)
      (block, reported.map(_(1)), !errors.nil)

  object JavaCode extends Embedding:
    def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
      if info.prim == t"java"
      then List(Block.Code(Language.Java, modelLines(harlequin.Java.highlight(trimmed(content))), Nil, fence + 2))
      else Unset

  def standard(checker: Checker, path: Text): List[Embedding] =
    List(ScalaCode(checker, path), JavaCode)

  // The fallback: the content line by line, as unparsed tokens, numbered from the file.
  def plain(info: List[Text], content: Text, fence: Int): Block =
    Block.Code(info.prim.lay(Language.Plain) { (name: Text) => Language(name) }, lines(content), Nil, fence + 2)

  def lines(content: Text): List[Block.Line] =
    trimmed(content).cut(t"\n").map: (line: Text) => Block.Line(List(Token.plain(line)))

  // A fence's content ends with the newline that closed its last line, which is not a line.
  def trimmed(content: Text): Text =
    if content.ends(t"\n") then Text(content.s.substring(0, content.s.length - 1).nn) else content

  private def modelLines(source: SourceCode): List[Block.Line] =
    import pyrocosm.modelLines
    source.modelLines

// A fenced code block's rendering, chosen by the fence's info string: the first embedding in
// scope that answers wins, and a fence nobody answers is shown plain under its own language
// name. Amok's `Formattable` (punctuation's, yielding HTML) with blocks as the result, so that
// the same embedding serves the web, the terminal and a fragment. `fence` is the line of the
// file, counted from zero, on which the fence opens, so a block can number its lines.
trait Embedding:
  def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]]

// A range of a fragment's text, by character offsets, as one note: on one line, across lines,
// or a caret where it has no width.
object Notes:
  def of(code: Text, from: Int, to: Int, style: Block.Note.Style, caption: List[Inline] = Nil)
  :   Block.Note =

    val lines: List[Text] = code.cut(t"\n")

    // The line and column of an offset, clamped to the text.
    def place(offset: Int): (Int, Int) =
      def recur(rest: List[Text], index: Int, consumed: Int): (Int, Int) = rest match
        case line :: tail =>
          val end: Int = consumed + line.length
          if offset <= end || tail.nil then (index, (offset - consumed).max(0).min(line.length))
          else recur(tail, index + 1, end + 1)

        case _ =>
          (0, 0)

      recur(lines, 0, 0)

    val (line, start) = place(from)
    val (endLine, end) = place(to.max(from))
    val last: Optional[Int] = if endLine > line then endLine else Unset
    Block.Note(line, start, end, style, caption, last)
