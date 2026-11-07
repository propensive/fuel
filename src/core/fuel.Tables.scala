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

import scala.collection.immutable as sci
import scala.collection.immutable.ArraySeq

import codepages.utf8Codepage
import pyrocosm.Tels
import pyrocosm.{Block, Inline, Tone}

// A table, written in a fence marked ` ```tabular `, as TEL: its rows are a TEL tabulated block
// — a heading line, of a `#` before each column's title, with each row beneath it, its cells
// aligned under the `#`s — and around them, before the heading line or after the rows (a blank
// line ends them), what a grid cannot say:
//
//   caption The inner *planets*      # beneath the table, as Markdown phrasing
//   column Distance                  # a column, by its heading, and how it is set:
//     align end                      # start or end
//
//   # Planet    # Distance  # Moons  # Notes
//   Mercury     0.39 AU     0        Closest to the Sun
//   Venus       0.72 AU     0        Turns *backwards*
//
// A cell is Markdown phrasing, its references resolved as prose's are (though a link in it is
// not yet among those the compile checks); it may hold single spaces, but not two
// together, which is what separates the columns; and it may be empty, though a row's first may
// not, and the last column runs to the end of its line. A column whose every cell that is not
// empty is a number is numeric, set at the end unless `align` says otherwise. TEL checks the
// alignment: a cell which starts away from its column's `#`, or runs into the next, is an
// error at its place, as is anything else this cannot read; and a table which fails is shown as
// its fence's text.
object Tables:
  // The fence's info string which marks a table.
  val language: Text = t"tabular"

  // What the settings say: the caption, and the alignment of each column which has one.
  private case class Settings
    ( caption: Optional[Text] = Unset, aligned: sci.Map[Int, Block.Alignment] = sci.Map() )

  // Why a fence's content is not a table: at a line of the content (from 1), or of the fence as
  // a whole.
  case class Problem(message: Text, line: Optional[Int] = Unset)

  // A cell which is a number: digits, perhaps signed, grouped, with a fraction, an exponent or
  // a percentage.
  private val number: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("[-+−]?[0-9][0-9,_]*(\\.[0-9]+)?([eE][-+]?[0-9]+)?%?").nn

  private def numeric(cell: Text): Boolean = number.matcher(cell.s).nn.matches

  // The leading spaces of a line, which TEL's indentation and the markers' offsets count from.
  private def indent(line: String): Int = line.length - line.stripLeading.nn.length

  // A comment line: the sigil, then a space or nothing, at the start of the line's text.
  private def comment(line: String): Boolean =
    val text: String = line.trim.nn
    text == "#" || text.startsWith("# ")

  // The offsets of a line's markers, when it is a tabulation line (TEL §16.1): its first
  // character a `#`, and at least one more `#` immediately after two or more spaces.
  private def markers(line: String): Optional[List[Int]] =
    val start: Int = indent(line)

    if start >= line.length || line.charAt(start) != '#' then Unset else
      val later: scala.List[Int] =
        (start + 3 until line.length).filter: index =>
          line.charAt(index) == '#' && line.charAt(index - 1) == ' ' && line.charAt(index - 2) == ' '

        . toList

      if later.isEmpty then Unset else List.from(start :: later)

  // The text of a line from one offset to the next (or its end), trimmed.
  private def slice(line: String, from: Int, to: Optional[Int]): Text =
    val end: Int = to.or(line.length).min(line.length)
    if from >= end then t"" else line.substring(from, end).nn.trim.nn.tt

  // A fence's content as a table, with its cells and caption read as Markdown phrasing through
  // `linker`; or the TEL error, or the problem, which stops it being one.
  def read(content: Text, linker: Linker = Linker.none): Block.Table | Tel.Error | Problem =
    import strategies.throwUnsafely

    val parsed: Tel | Tel.Error =
      try Tels.headed(content)(0).read[Tel] catch case error: Tel.Error => error

    parsed match
      case error: Tel.Error => error
      case tel: Tel         => table(content, tel, linker)

  private def table(content: Text, tel: Tel, linker: Linker): Block.Table | Problem =
    val split: scala.Array[String | Null] = content.s.split("\n", -1).nn
    val lines: scala.List[String] = ArraySeq.unsafeWrapArray(split).toList.map(_.nn)
    def blank(index: Int): Boolean = lines(index).trim.nn.isEmpty

    val headings: scala.List[Int] =
      lines.indices.filter { index => markers(lines(index)).present }.toList

    headings match
      case scala.Nil =>
        Problem(t"A table needs a heading line, of a `#` before each column's title, as `# Name  # Born`.")

      case scala.::(_, scala.::(second, _)) =>
        Problem(t"A `tabular` fence holds one table, but this is a second heading line.", second + 1)

      case scala.::(heading, _) =>
        val offsets: scala.List[Int] = markers(lines(heading)).or(List()).stdlib.toList
        val base: Int = indent(lines(heading))

        def bound(index: Int): Optional[Int] = offsets.lift(index + 1).fold(Unset)(_ - 2)

        // A heading's text lies between its `#`, and the space after it, and the next `#`.
        val titles: scala.List[Text] = offsets.indices.toList.map: index =>
          slice(lines(heading), offsets(index) + 1, offsets.lift(index + 1).fold(Unset)(identity))

        val rows: scala.List[Int] =
          (heading + 1 until lines.size).takeWhile(!blank(_)).toList

        // The lines of the compounds which are not rows: before the heading line and after the
        // rows, at the table's indentation, which is where `tel`'s compounds come from.
        def settingLines(range: sci.Range): scala.List[Int] =
          range.filter: index =>
            !blank(index) && indent(lines(index)) == base && !comment(lines(index))

          . toList

        val before: scala.List[Int] = settingLines(0 until heading)
        val after: scala.List[Int] = settingLines((heading + 1 + rows.size) until lines.size)
        val compounds: scala.List[Tel.Compound] = tel.childCompounds.to[List].stdlib.toList

        val settings: scala.List[(Tel.Compound, Optional[Int])] =
          val outside: scala.List[Tel.Compound] =
            compounds.take(before.size) ++ compounds.drop(before.size + rows.size)

          outside.zipAll(before ++ after, null, -1).collect:
            case (compound: Tel.Compound, line: Int) =>
              (compound, if line < 0 then Unset else line + 1)

        val cells: scala.List[scala.List[Text]] = rows.map: (row: Int) =>
          offsets.indices.toList.map: index =>
            slice(lines(row), offsets(index), bound(index))

        if rows.isEmpty then
          Problem
            ( t"This table has no rows: they follow its heading line, with no blank line between.",
              heading + 1 )
        else
          configure(settings, titles, cells, linker)

  // The settings applied to the rows: the caption, and each column's alignment.
  private def configure
    ( settings: scala.List[(Tel.Compound, Optional[Int])],
      titles:   scala.List[Text],
      cells:    scala.List[scala.List[Text]],
      linker:   Linker )
  :   Block.Table | Problem =

    def text(compound: Tel.Compound): Text =
      compound.atoms.to[List].stdlib.toList.collect { case atom: Tel.Atom.Inline => atom.text.s }
      . mkString(" ").tt

    def children(compound: Tel.Compound): scala.List[Tel.Compound] =
      compound.children.to[List].stdlib.toList.flatMap(_.compounds.to[List].stdlib.toList)

    def alignment(word: Text): Optional[Block.Alignment] = word match
      case t"start"  => Block.Alignment.Start
      case t"end"    => Block.Alignment.End
      case _         => Unset

    // Each column's alignment, by its index, as the settings say; or the first problem.
    def recur(rest: scala.List[(Tel.Compound, Optional[Int])], said: Settings)
    :   Settings | Problem =

      rest match
        case scala.Nil =>
          said

        case scala.::((compound, line), tail) => compound.keyword match
          case t"caption" =>
            if said.caption.present then Problem(t"A table has one caption.", line)
            else recur(tail, said.copy(caption = text(compound)))

          case t"column" =>
            val title: Text = text(compound)
            val index: Int = titles.indexOf(title)

            if index < 0 then
              val known: Text = titles.map { title => t"`$title`" }.mkString(", ").tt
              Problem(t"There is no column headed `$title`; the headings are $known.", line)
            else
              children(compound) match
                case scala.Nil =>
                  Problem(t"Say how the column `$title` is set, with `align` beneath it.", line)

                case settings =>
                  val answers: scala.List[Optional[Block.Alignment] | Problem] = settings.map:
                    (setting: Tel.Compound) => setting.keyword match
                      case t"align" =>
                        val word: Text = text(setting)
                        alignment(word).or(Problem(t"A column is aligned `start` or `end`, not `$word`.", line))

                      case other =>
                        Problem(t"A column's setting is `align`, not `$other`.", line)

                  answers.collectFirst { case problem: Problem => problem } match
                    case Some(problem) => problem
                    case None =>
                      val last: Optional[Block.Alignment] =
                        answers.collect { case alignment: Block.Alignment => alignment }.lastOption
                        . fold(Unset)(identity)

                      val aligned: sci.Map[Int, Block.Alignment] =
                        last.lay(said.aligned) { alignment => said.aligned.updated(index, alignment) }

                      recur(tail, said.copy(aligned = aligned))

          case other =>
            Problem
              ( t"A table's settings are `caption` and `column`, not `$other`; its rows follow its heading line, with no blank line between.",
                line )

    recur(settings, Settings()) match
      case problem: Problem => problem

      case Settings(caption, aligned) =>
        def phrasing(text: Text): List[Inline] =
          if text == t"" then Nil else Blocks.inline(text, linker)

        val columns: List[Block.Column] = List.from:
          titles.indices.toList.map: index =>
            // A column set at the start is not numeric, which a renderer may take to mean set at
            // the end, whatever its alignment.
            val column: scala.List[Text] = cells.map(_.lift(index).getOrElse(t"")).filter(_ != t"")
            val number: Boolean = column.nonEmpty && column.forall(numeric)

            aligned.get(index) match
              case Some(Block.Alignment.Start) =>
                Block.Column(phrasing(titles(index)), Block.Alignment.Start)

              case Some(alignment) =>
                Block.Column(phrasing(titles(index)), alignment, numeric = number)

              case None =>
                val alignment: Block.Alignment =
                  if number then Block.Alignment.End else Block.Alignment.Start

                Block.Column(phrasing(titles(index)), alignment, numeric = number)

        val rows: List[Block.Row] = List.from:
          cells.map: (row: scala.List[Text]) =>
            Block.Row(List.from(row.map { cell => Block.Cell(phrasing(cell)) }))

        // The caption as one phrase: a caption of several inlines does not survive the model's
        // TEL encoding, which is how a compiled document is kept.
        val captioned: Optional[List[Inline]] =
          caption.let { caption => List(Inline.Phrase(phrasing(caption))) }

        Block.Table(columns, rows, captioned)

// The embedding of `tabular` fences, for one document (or page, or deck): each as its table, or,
// when it is not one, as its text, with the problem kept, as a diagnostic at its place in the
// file, in `problems`.
final class Tables(path: Text, linker: Linker = Linker.none) extends Embedding:
  private var problems0: List[Block] = Nil

  def problems: List[Block] = problems0.reverse

  def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
    if info.head != Tables.language then Unset else
      def failed(problem: Block): List[Block] =
        problems0 = problem :: problems0
        List(Embedding.plain(info, content, fence))

      Tables.read(content, linker) match
        case table: Block.Table =>
          List(table)

        case error: Tel.Error =>
          failed(Preamble.diagnostic(error, path, content, fence + 1))

        // The content's line `n` is the file's `fence + 1 + n`, counting from 1.
        case Tables.Problem(message, line) =>
          val location: Block.Location = Block.Location(path, line.lay(fence + 1)(_ + fence + 1))

          val excerpt: List[Block] = line.lay(Nil): (line: Int) =>
            Excerpts.plain(content, line, Block.Note.Style.Erroneous).map:
              case code: Block.Code => code.copy(start = code.start.let(_ + fence + 1))
              case other            => other

          failed(Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(message)), excerpt))
