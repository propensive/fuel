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
  // fragments it follows (whose text precedes it in the compile), what its errors mean, and
  // its language when it is not Scala's (a ` ```java fuel ` fence comes here for its preamble,
  // and is highlighted, not compiled). One instance per document: the fragments seen so far
  // are what `follows` names, and `problems` is what the compile reports — a diagnostic each,
  // at its place in the file.
  final class ScalaCode(checker: Checker, path: Text) extends Embedding:
    private var fragments: List[(Text, Text)] = Nil
    private var count: Int = 0
    private var problems0: List[Block] = Nil

    def problems: List[Block] = problems0.reverse

    private def fragment(name: Text): Text =
      fragments.seek(_(0) == name).lay(t""): (entry: (Text, Text)) => entry(1)

    def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
      if info.prim != t"scala" && !info.has(t"fuel") then Unset else
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

        val id: Text = preamble.id.or(t"fragment-${count.show}")
        val context: Optional[harlequin.Scala.Context] = preamble.scalaContext
        val language: Text = preamble.language.or(info.prim.or(t"scala"))

        // The compile unit: the followed fragments, then this one. Diagnostics are reported
        // against the whole unit (and harlequin's wrapper for a term or type context), so they
        // are shifted back by the prefix.
        val prefix: Text =
          preamble.follows.map(fragment(_)).join(t"", t"\n", t"\n")

        val wrapper: Int = context match
          case harlequin.Scala.Context.Term => 10  // `val x = {\n`
          case harlequin.Scala.Context.Type => 9   // `type X =\n`
          case _                            => 0

        val checked: Boolean = checker.compiles && preamble.errors != Preamble.Errors.Ignore

        // The preamble's frames which have code: one without its `code` child is a problem
        // at its line in the file — the nth `frame` line of the preamble, since a TEL node
        // does not know its line — and is dropped.
        val frameLines: List[Int] =
          header.lay(Nil) { (text: Text) => text.cut(t"\n").indexed.filter(_(0).starts(t"frame")).map(_(1).n0) }

        val described: List[Preamble.Frame] =
          preamble.frames.indexed.bind: (frame: Preamble.Frame, index: Ordinal) =>
            if frame.code.present then List(frame) else
              val location = Block.Location(path, fence + 2 + frameLines.at(index).or(0))
              val text = t"Frame ${(index.n0 + 1).toString.tt} has no `code`."
              problems0 = Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text))) :: problems0
              Nil

        // An animated fence: its frames, then the code after `##` as the last if there is any.
        val frames: List[Preamble.Frame] =
          if preamble.frames.nil then Nil
          else if code.trim == t"" then described
          else described + List(Preamble.Frame(code))

        if !frames.nil then animated(frames, preamble, language, prefix, wrapper, context, checked, id, start)
        else if language == t"scala" then embedPlain(code, preamble, prefix, wrapper, context, checked, id, start)
        else List(Block.Code(Language(language), highlight(language, code, t"", Unset, false) match
          case source: SourceCode => modelLines(source)
          case _                  => lines(code), notes(code, preamble.marks), start))

    // One fragment highlighted in a language: Scala through the checker (compiled, after the
    // prefix, when checking), Java through harlequin alone, another language not at all; or
    // the failure, which is shown plain with a diagnostic.
    private def highlight
      ( language: Text,
        code:     Text,
        prefix:   Text,
        context:  Optional[harlequin.Scala.Context],
        checked:  Boolean )
    :   Optional[SourceCode | Throwable] =

      language match
        case t"scala" =>
          try
            if checked then checker.check(prefix + code, context)
            else Checker.tokenized.check(code, context)
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
    // of each slide: its label, hold, time, language when it differs from the block's, and notes —
    // the preamble's marks, which apply in every frame they match, and the frame's own. The
    // block's lines and notes are the first frame's. A frame that does not compile is a
    // problem naming it, at the fragment's place; the fragment a later one may follow is the
    // last Scala frame's code.
    private def animated
      ( frames:   List[Preamble.Frame],
        preamble: Preamble,
        language: Text,
        prefix:   Text,
        wrapper:  Int,
        context:  Optional[harlequin.Scala.Context],
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

          val frameLines: List[Block.Line] = highlight(own, code, prefix, context, checked && scala) match
            case error: Throwable =>
              val location = Block.Location(path, start)
              val text = t"Frame $number could not be highlighted: ${error.toString.tt}"
              problems0 = Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text))) :: problems0
              lines(code)

            case source: SourceCode =>
              if checked && scala && preamble.errors == Preamble.Errors.Fail then
                source.diagnostics.each: (diagnostic: harlequin.Diagnostic) =>
                  if diagnostic.importance == harlequin.Diagnostic.Importance.Error then
                    val location = Block.Location(path, start)
                    val text = t"Frame $number: ${diagnostic.message}"
                    problems0 = Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(text))) :: problems0

              // The lines of the frame alone: the prefix's lines dropped from the front.
              val all: List[Block.Line] = modelLines(source)
              all.skip(all.size - code.cut(t"\n").size)

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

      val frameLines: List[List[Block.Line]] = built.map(_(0))
      val details: List[Block.Slide.Detail] = built.map(_(1))

      val animation: Block.Animation =
        Block.Animation.of(frameLines, details).copy(autoplay = preamble.autoplay, loop = preamble.loop)

      List(Block.Code(Language(language), frameLines.prim.or(Nil), details.prim.lay(Nil)(_.notes), start, animation))

    private def embedPlain
      ( code:     Text,
        preamble: Preamble,
        prefix:   Text,
        wrapper:  Int,
        context:  Optional[harlequin.Scala.Context],
        checked:  Boolean,
        id:       Text,
        start:    Int )
    :   List[Block] =

        // A fragment the highlighter cannot handle (harlequin overflows on an expression
        // nested ten deep, propensive/soundness#2284) is shown plain, with a diagnostic, and
        // the rest of the document goes on.
        val attempt: SourceCode | Throwable =
          try
            if checked then checker.check(prefix + code, context)
            else Checker.tokenized.check(code, context)
          catch case error: Throwable => error

        fragments = (id, code) :: fragments

        attempt match
          case error: Throwable =>
            val location = Block.Location(path, start)
            val message = List(Block.paragraph(t"The fragment could not be highlighted: ${error.toString.tt}"))
            problems0 = Block.Diagnostic(Tone.Failure, location, message) :: problems0
            List(Block.Code(Language.Scala, lines(code), Nil, start))

          case source: SourceCode =>
            embedSource(source, code, prefix, wrapper, checked, preamble, id, start)

    private def embedSource
      ( source:   SourceCode,
        code:     Text,
        prefix:   Text,
        wrapper:  Int,
        checked:  Boolean,
        preamble: Preamble,
        id:       Text,
        start:    Int )
    :   List[Block] =


      // The lines of this fragment alone: the prefix's lines are dropped from the front.
      val own: Int = code.cut(t"\n").size
      val allLines: List[Block.Line] = modelLines(source)
      val lines: List[Block.Line] = allLines.skip(allLines.size - own)

      // Each diagnostic as a note on the fragment, and as a diagnostic of its own after it,
      // located in the file, with the lines around it.
      val reported: List[(Block.Note, Block, Boolean)] =
        if !checked then Nil else
          source.diagnostics.bind[List[(Block.Note, Block, Boolean)], (Block.Note, Block, Boolean), List[(Block.Note, Block, Boolean)]]:
            (diagnostic: harlequin.Diagnostic) =>
              val from: Int = diagnostic.span.offset.let(_.n0).or(0) - wrapper - prefix.length
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
            val location = Block.Location(path, start)
            val message = List(Block.paragraph(t"This fragment is marked `errors show`, but it compiled."))
            problems0 = Block.Diagnostic(Tone.Failure, location, message) :: problems0

        case _ =>
          ()

      val block: Block = Block.Code(Language.Scala, lines, reported.map(_(0)) + notes(code, preamble.marks), start)
      block :: reported.map(_(1))

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
