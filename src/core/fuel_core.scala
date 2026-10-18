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

import dysasymptotics.linearSize
import pyrocosm.{Block, Inline, Language, Token}

object Embedding:
  // A Scala fence, highlighted by the checker — typechecked, in a compile with a classpath —
  // with the preamble a ` ```scala fuel ` fence carries honoured: its context, its marks, the
  // fragments it follows (whose text precedes it in the compile), and what its errors mean.
  // One instance per document: the fragments seen so far are what `follows` names, and
  // `problems` is what the compile reports.
  final class ScalaCode(checker: Checker, path: Text) extends Embedding:
    private var fragments: List[(Text, Text)] = Nil
    private var count: Int = 0
    private var problems0: List[Text] = Nil

    def problems: List[Text] = problems0.reverse

    private def fragment(name: Text): Text =
      fragments.seek(_(0) == name).lay(t""): (entry: (Text, Text)) => entry(1)

    def embed(info: List[Text], content: Text): Optional[List[Block]] =
      if info.prim != t"scala" then Unset else
        val (header, code0) =
          if info.has(t"fuel") then Preamble.split(content) else (Unset, content)

        val code: Text = trimmed(code0)
        val preamble: Preamble = header.let(Preamble.parse(_)).or(Preamble.empty)
        count += 1
        val id: Text = preamble.id.or(t"fragment-${count.show}")
        val context: Optional[harlequin.Scala.Context] = preamble.scalaContext

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

        val source: SourceCode =
          if checked then checker.check(prefix + code, context)
          else Checker.tokenized.check(code, context)

        fragments = (id, code) :: fragments

        // The lines of this fragment alone: the prefix's lines are dropped from the front.
        val own: Int = code.cut(t"\n").size
        val allLines: List[Block.Line] = modelLines(source)
        val lines: List[Block.Line] = allLines.skip(allLines.size - own)

        val diagnostics: List[Block.Note] =
          if !checked then Nil else
            source.diagnostics.bind[List[Block.Note], Block.Note, List[Block.Note]]:
              (diagnostic: harlequin.Diagnostic) =>
                val style: Block.Note.Style = diagnostic.importance match
                  case harlequin.Diagnostic.Importance.Error => Block.Note.Style.Erroneous
                  case _                                     => Block.Note.Style.Caution

                val start: Int = diagnostic.span.offset.let(_.n0).or(0) - wrapper - prefix.length
                val length: Int = diagnostic.span.length.or(1)

                if start < 0 then Nil
                else Notes.of(code, start, start + length, style, diagnostic.message)

        val errors: List[harlequin.Diagnostic] =
          if !checked then Nil
          else source.diagnostics.filter(_.importance == harlequin.Diagnostic.Importance.Error)

        preamble.errors match
          case Preamble.Errors.Fail =>
            errors.each: (error: harlequin.Diagnostic) =>
              problems0 = t"$path: $id: ${error.message}" :: problems0

          case Preamble.Errors.Show =>
            if checked && errors.nil
            then problems0 = t"$path: $id: expected an error, but it compiled" :: problems0

          case _ =>
            ()

        val marks: List[Block.Note] =
          preamble.marks.bind[List[Block.Note], Block.Note, List[Block.Note]]:
            (mark: Preamble.Mark) =>
              mark.selection.findIn(code).lay(Nil: List[Block.Note]): (range: (Int, Int)) =>
                Notes.of(code, range(0), range(1), mark.style, mark.caption)

        List(Block.Code(Language.Scala, lines, diagnostics + marks))

  object JavaCode extends Embedding:
    def embed(info: List[Text], content: Text): Optional[List[Block]] =
      if info.prim == t"java"
      then List(Block.Code(Language.Java, modelLines(harlequin.Java.highlight(trimmed(content)))))
      else Unset

  def standard(checker: Checker, path: Text): List[Embedding] =
    List(ScalaCode(checker, path), JavaCode)

  // The fallback: the content line by line, as unparsed tokens.
  def plain(info: List[Text], content: Text): Block =
    Block.Code(info.prim.lay(Language.Plain) { (name: Text) => Language(name) }, lines(content))

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
// the same embedding serves the web, the terminal and a fragment.
trait Embedding:
  def embed(info: List[Text], content: Text): Optional[List[Block]]

// Notes are per line, so a range of a fragment's text becomes one note per line it touches.
object Notes:
  def of(code: Text, from: Int, to: Int, style: Block.Note.Style, caption: Optional[Text])
  :   List[Block.Note] =

    val lines: List[Text] = code.cut(t"\n")

    def recur(rest: List[Text], index: Int, offset: Int, acc: List[Block.Note]): List[Block.Note] =
      rest match
        case line :: tail =>
          val end: Int = offset + line.length
          val start: Int = from.max(offset)
          val stop: Int = to.min(end)

          val acc2: List[Block.Note] =
            if start < stop || (from == to && from >= offset && from <= end) then
              Block.Note(index, start - offset, stop.max(start + 1) - offset, style, caption) :: acc
            else
              acc

          recur(tail, index + 1, end + 1, acc2)

        case _ =>
          acc.reverse

    recur(lines, 0, 0, Nil)
