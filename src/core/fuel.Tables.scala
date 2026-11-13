                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                ╭────────╮                   ╭───╮                                ┃
┃                                │   ╭────╯                   │   │                                ┃
┃                                │   │                        │   │                                ┃
┃                                │   ╰──╮╭───╮ ╭───╮╭────────╮│   │                                ┃
┃                                │   ╭──╯│   │ │   ││   ╭─╮  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╰─╯  ││   │                                ┃
┃                                │   │   │   │ │   ││   ╭────╯│   │                                ┃
┃                                │   │   │   ╰─╯   ││   ╰────╮│   │                                ┃
┃                                ╰───╯   ╰────╌╰───╯╰────────╯╰───╯                                ┃
┃                                                                                                  ┃
┃    Fuel, version 0.6.0.                                                                          ┃
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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Inline, Tone}

// A table, written in a fence marked ` ```tabular `, as a TEL document of the schema `tabular`,
// derived from `Tables.Table`:
//
//   record Column
//     field title String               # Markdown phrasing
//     field align String optional      # start or end
//   record Row
//     field cell String repeatable     # Markdown phrasing, one per column
//   document
//     field caption String optional    # Markdown phrasing, beneath the table
//     field column Column repeatable
//     field row Row repeatable
//
// A field may be written as a child compound or, in its member order, as an atom on its
// parent's line, so a column's title follows `column`, and a row's cells follow `row`; after
// two spaces, a value may hold single spaces, since only two or more then separate values:
//
//   column  Planet
//   column  Distance  end
//   column
//     title  Notes
//   row  Mercury  0.39  Closest to the Sun
//   row  Venus
//     cell
//     cell  Turns *backwards*
//
// A `cell` with no value is an empty cell. Columns come together, and rows together, as TEL
// requires of a repeatable field's compounds. A row with fewer cells than there are columns is
// filled out with empty ones. A column whose cells are all numbers, where they are not empty,
// is numeric and set at the end, unless it says how it is aligned. Every fault in a table is
// reported at its place: TEL's, as the schema finds them, and these, which it cannot say — a
// table with no column or no row, an alignment other than `start` or `end`, a row with more
// cells than columns. A table with any fault is shown as its fence's text.
object Tables:
  // The fence's info string which marks a table.
  val language: Text = t"tabular"

  case class Column(title: Text, align: Optional[Text] = Unset)
  case class Row(cell: List[Text])
  case class Table(caption: Optional[Text] = Unset, column: List[Column], row: List[Row])

  // The schema, as the case classes derive it.
  lazy val schema: Tels =
    import strategies.throwUnsafely
    Tels.tels[Table](t"tabular")

  // A fault in a fence's content: its message, and where it is, by line and column of the
  // content (from 1), with the length of the text it concerns, where those are known.
  case class Fault
    ( message: Text,
      line:    Optional[Int] = Unset,
      column:  Optional[Int] = Unset,
      length:  Optional[Int] = Unset )

  // The faults the schema finds, as they are accrued: each TEL error, with the path of what it
  // concerns and where that is.
  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)(using Diagnostics)
  extends Error(m"the table has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // A cell which is a number: digits, perhaps signed, grouped, with a fraction, an exponent or
  // a percentage.
  private val number: java.util.regex.Pattern =
    java.util.regex.Pattern.compile("[-+−]?[0-9][0-9,_]*(\\.[0-9]+)?([eE][-+]?[0-9]+)?%?").nn

  private def numeric(cell: Text): Boolean = number.matcher(cell.s).nn.matches

  private def alignment(word: Text): Optional[Block.Alignment] = word match
    case t"start" => Block.Alignment.Start
    case t"end"   => Block.Alignment.End
    case _        => Unset

  // A path's keywords, without the indices of repeated fields: `/column/1/title` is
  // `column`, `title`.
  private def keywords(pointer: Text): List[Text] =
    pointer.cut(t"/").filter { segment => segment != t"" && safely(segment.as[Int]).absent }

  // What a fault TEL finds means in a table.
  private def explain(error: Tel.Error, pointer: Text): Text =
    import Tel.Error.Reason
    val path: List[Text] = keywords(pointer)
    val last: Text = path.reverse.head.or(t"")

    error.reason match
      case Reason.UnknownKeyword => path.size match
        case 1 =>
          t"A table has a `caption`, `column`s and `row`s, but not `$last`."

        case _ =>
          if path.head.or(t"") == t"column"
          then t"A column has a `title` and an `align`, but not `$last`."
          else t"A row has `cell`s, but not `$last`."

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        t"This has more values than it takes: a value with spaces in it follows two spaces, as `title  Known for`."

      case Reason.RequiredMemberAbsent =>
        if last == t"title" then t"This column has no `title`." else t"This needs a `$last`."

      case Reason.NonRepeatableTooMany =>
        t"This says `$last` more than once."

      case Reason.MembersNonContiguous =>
        t"A table's `column`s come together, and its `row`s together, without one among the others."

      case _ =>
        error.message.text

  // A fence's content as a table, with its titles, cells and caption read as Markdown phrasing
  // through `linker`; or the TEL error which stops it being TEL; or every fault which stops it
  // being a table.
  def read(content: Text, linker: Linker = Linker.none): Block.Table | Tel.Error | List[Fault] =
    import strategies.throwUnsafely
    import parsing.trackPositions

    val (text, added) = pyrocosm.Tels.headed(content)

    val parsed: Tel | Tel.Error = try text.read[Tel] catch case error: Tel.Error => error

    parsed match
      case error: Tel.Error =>
        error

      case tel: Tel =>
        // A position in the headed text, as a line and column of the content.
        def place(line: Int, column: Int, length: Optional[Int], message: Text): Fault =
          Fault(message, line - added, column, length)

        // At the compound a path names, or, where that is an atom rather than a compound, at
        // the nearest compound above it.
        def at(pointer: List[Text], message: Text): Fault =
          if pointer.nil then Fault(message)
          else tel.locateKey(Telp(pointer)).lay(at(pointer.reverse.skip(1).reverse, message)):
            (position: Tel.Error.Position) =>
              place(position.line, position.column, Unset, message)

        val faults: Faults =
          validate[Tel.Focus](Faults()):
            case error: Tel.Error =>
              val pointer: Text = prior.let(_.pointer.encode).or(t"/")
              accrual + (error, pointer, prior.lay(Span.empty)(_.span))

          . protect(Tel.Type.assign(tel, schema))

        // A fault without a place of its own — a missing member — is put at its parent's.
        val found: List[Fault] = faults.items.map: (error, pointer, span) =>
          val message: Text = explain(error, pointer)

          if span.exists then
            val line: Int = span.startLine.lay(1)(_.n1)
            val column: Int = span.startColumn.lay(1)(_.n1)
            place(line, column, span.length, message)
          else
            at(pointer.cut(t"/").filter(_ != t"").reverse.skip(1).reverse, message)

        if !found.nil then found else
          val table: Table = tel.as[Table]
          val width: Int = table.column.size

          val misaligned: List[Fault] = table.column.indexed.bind: (column: Column, index: Ordinal) =>
            column.align.lay(Nil): (word: Text) =>
              if alignment(word).present then Nil
              else
                val message: Text = t"A column is aligned `start` or `end`, not `$word`."
                List(at(List(t"column", index.n0.show, t"align"), message))

          val wide: List[Fault] = table.row.indexed.bind: (row: Row, index: Ordinal) =>
            if row.cell.size <= width then Nil
            else
              val message: Text =
                t"This row has ${row.cell.size.show} cells, but the table has ${width.show} columns."

              List(at(List(t"row", index.n0.show), message))

          val empty: List[Fault] =
            (if table.column.nil then List(Fault(t"A table needs a `column`.")) else Nil) +
              (if table.row.nil then List(Fault(t"A table needs a `row`.")) else Nil)

          val all: List[Fault] = empty + misaligned + wide
          if !all.nil then all else render(table, linker)

  // The table, read: each row filled out to the columns, and its text read as Markdown
  // phrasing.
  private def render(table: Table, linker: Linker): Block.Table =
    def phrasing(text: Text): List[Inline] =
      if text == t"" then Nil else Blocks.inline(text, linker)

    val width: Int = table.column.size

    val cells: List[List[Text]] = table.row.map: (row: Row) =>
      row.cell + List.fill(width - row.cell.size)(t"")

    val columns: List[Block.Column] = table.column.indexed.map: (column: Column, index: Ordinal) =>
      val values: List[Text] = cells.map(_.at(index).or(t"")).filter(_ != t"")
      val number: Boolean = !values.nil && values.all(numeric)
      val title: List[Inline] = phrasing(column.title)

      // A column set at the start is not numeric, which a renderer may take to mean set at the
      // end, whatever its alignment.
      column.align.let(alignment(_)) match
        case Block.Alignment.Start =>
          Block.Column(title, Block.Alignment.Start)

        case alignment: Block.Alignment =>
          Block.Column(title, alignment, numeric = number)

        case _ =>
          val alignment: Block.Alignment =
            if number then Block.Alignment.End else Block.Alignment.Start

          Block.Column(title, alignment, numeric = number)

    val rows: List[Block.Row] = cells.map: (row: List[Text]) =>
      Block.Row(row.map { cell => Block.Cell(phrasing(cell)) })

    // The caption as one phrase: a caption of several inlines does not survive the model's
    // TEL encoding, which is how a compiled document is kept.
    val caption: Optional[List[Inline]] =
      table.caption.let { caption => List(Inline.Phrase(phrasing(caption))) }

    Block.Table(columns, rows, caption)

// The embedding of `tabular` fences, for one document (or page, or deck): each as its table, or,
// when it is not one, as its text, with each of its faults kept, as a diagnostic at its place in
// the file, in `problems`.
final class Tables(path: Text, linker: Linker = Linker.none) extends Embedding:
  private var problems0: List[Block] = Nil

  def problems: List[Block] = problems0.reverse

  def embed(info: List[Text], content: Text, fence: Int): Optional[List[Block]] =
    if info.head != Tables.language then Unset else
      def failed(problems: List[Block]): List[Block] =
        problems0 = problems.reverse + problems0
        List(Embedding.plain(info, content, fence))

      Tables.read(content, linker) match
        case table: Block.Table =>
          List(table)

        case error: Tel.Error =>
          failed(List(Preamble.diagnostic(error, path, content, fence + 1)))

        // The content's line `n` is the file's `fence + 1 + n`, counting from 1; a fault with no
        // line is the fence's, on the line which opens it.
        case faults: List[Tables.Fault] @unchecked =>
          failed:
            faults.map: (fault: Tables.Fault) =>
              val line: Int = fault.line.lay(fence + 1)(_ + fence + 1)
              val location: Block.Location = Block.Location(path, line, fault.column)

              val excerpt: List[Block] = fault.line.lay(Nil): (line: Int) =>
                Excerpts.plain(content, line, Block.Note.Style.Erroneous, fault.column, fault.length)
                . map:
                    case code: Block.Code => code.copy(start = code.start.let(_ + fence + 1))
                    case other            => other

              val message: List[Block] = List(Block.paragraph(fault.message))
              Block.Diagnostic(Tone.Failure, location, message, excerpt)
