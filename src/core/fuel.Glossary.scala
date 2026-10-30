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

import codepages.utf8Codepage
import pyrocosm.{Block, Inline, Tone}

// The glossary: terms defined in TEL files, each `define <term>` holding a Markdown
// `definition` and, when the term is explained in full elsewhere, a `link` to that page. A
// `[term]` in prose is annotated with the definition and leads to the link, or to the term's
// entry on the glossary page. The files named by the `glossary` setting are combined; a term
// defined twice is a failure at the second.
//
//     define interface
//       link  /doc/guide/interfaces.md
//       definition
//           A shared boundary where two systems meet and exchange information.
object Glossary:
  case class Term(name: Text, definition: Text, link: Optional[Text], file: Text, line: Int):
    // The term's anchor on the glossary page, and the key a document's `[term]` leads to.
    def slug: Text = Blocks.slug(name)

  val none: Glossary = Glossary(Nil)

  // The scheme of the link reference definitions the glossary adds to a document, by which a
  // `[term]` the parser linked is known as the glossary's.
  val scheme: Text = t"glossary:"

  // The lines `headed` adds before the file's own.
  private def headed(text: Text): Text =
    if text.trim.starts(t"tel ") then text else t"tel 1.0\n\n$text"

  private def added(text: Text): Int = if text.trim.starts(t"tel ") then 0 else 2

  // The text of a node's child by keyword: its atoms, inline then the indented source, joined.
  private def child(node: Tel, keyword: Text): Optional[Text] =
    node.fields(keyword).to[List].head.let(_.atomTexts.to[List].filter(_ != t"").join(t" ")).let:
      (text: Text) => if text == t"" then Unset else text

  // A glossary read from a file's text, or the diagnostic which says why it could not be.
  def read(path: Text, text: Text): Glossary | Block.Diagnostic =
    import strategies.throwUnsafely

    try
      val tel: Tel = headed(text).read[Tel]
      val lines: List[Text] = text.cut(t"\n")

      // A node, and the compound it wraps (whose atoms are the name), paired by order: `fields`
      // and `childCompounds` walk the same children.
      val nodes: List[Tel] = tel.fields(t"define").to[List]
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List].filter(_.keyword == t"define")

      val terms: List[Term] = nodes.zip(compounds).map: (node: Tel, compound: Tel.Compound) =>
        val inline: List[Text] = List.from:
          compound.atoms.stdlib.collect { case Tel.Atom.Inline(text, _) => text }.toList

        val name: Text = inline.join(t" ").trim

        // The term's line, by its text: a TEL node does not know its own.
        val line: Int =
          lines.indexed.seek: (line: Text, index: Ordinal) =>
            line.trim.starts(t"define ") && line.trim.skip(7).trim == name

          . let(_(1).n0 + 1).or(1)

        Term(name, child(node, t"definition").or(t""), child(node, t"link"), path, line)

      Glossary(terms)

    catch case error: Tel.Error =>
      val shift: Int = -added(text)
      val raw: Block.Diagnostic = Block.Diagnostic.of(error, path, headed(text))

      val location = raw.location.let: (location: Block.Location) =>
        location.copy(line = location.line.let(_ + shift))

      // The excerpt's lines are numbered as the file numbers them too.
      val excerpt: List[Block] = raw.excerpt.map:
        case code: Block.Code => code.copy(start = code.start.let(_ + shift))
        case other            => other

      raw.copy(location = location, excerpt = excerpt)

  // Every glossary as one, with the problems: a term defined twice (by its slug, so that two
  // spellings of one anchor are one term too), a name which cannot be a reference label (empty,
  // or holding a bracket), and a caution for a name of one character, which a `[1]` or an
  // `[x]` in prose would match, since labels are matched case-insensitively.
  def merge(glossaries: List[Glossary]): (Glossary, List[Block]) =
    def report(tone: Tone, term: Term, message: Text): Block =
      Block.Diagnostic(tone, Block.Location(term.file, term.line), List(Block.paragraph(message)))

    def recur(rest: List[Term], kept: List[Term], problems: List[Block]): (Glossary, List[Block]) =
      rest match
        case term :: tail =>
          if term.name == t"" || term.name.s.contains("[") || term.name.s.contains("]") then
            val message: Text =
              t"`${term.name}` cannot be a glossary term: a term is written in brackets, so it " +
                t"cannot hold one."

            recur(tail, kept, problems + List(report(Tone.Failure, term, message)))
          else
            kept.seek(_.slug == term.slug) match
              case earlier: Term =>
                val message: Text =
                  t"`${term.name}` is defined already, at ${earlier.file}:${earlier.line.show}."

                recur(tail, kept, problems + List(report(Tone.Failure, term, message)))

              case _ =>
                val message: Text =
                  t"`${term.name}` is one character, which any `[${term.name}]` in prose " +
                    t"matches, in either case."

                val short: List[Block] =
                  if term.name.length == 1 then List(report(Tone.Warning, term, message)) else Nil

                recur(tail, kept + List(term), problems + short)

        case _ =>
          (Glossary(kept), problems)

    recur(glossaries.bind(_.terms), Nil, Nil)

case class Glossary(terms: List[Glossary.Term]):
  def apply(slug: Text): Optional[Glossary.Term] = terms.seek(_.slug == slug)

  // The link reference definitions added after a document's text before it is parsed, so that
  // `[term]` is a link the parser makes, not a pattern searched for; a definition the author
  // writes first wins, as CommonMark has it.
  def definitions: Text =
    val lines: List[Text] = terms.map: (term: Glossary.Term) =>
      t"[${term.name}]: ${Glossary.scheme}${term.slug}"

    lines.join(t"\n")

  // The glossary page: each term under a heading anchored by its slug, with its definition and
  // where it is explained in full.
  def page(linker: Linker): List[Block] =
    val entries: List[Block] = terms.bind: (term: Glossary.Term) =>
      val detail: List[Block] = Blocks.of(Parser.parse(term.definition), Nil, linker)

      val more: List[Block] = term.link.lay(Nil: List[Block]): (url: Text) =>
        val destination: Inline.Destination =
          if url.starts(t"/") then Inline.Destination.Local(url)
          else Inline.Destination.External(url)

        val link: Inline = Inline.Link(destination, Inline.text(url))
        List(Block.Paragraph(List(Inline.Textual(t"See "), link, Inline.Textual(t"."))))

      Block.Heading(2, Inline.text(term.name), term.slug) :: detail + more

    Block.Heading(1, Inline.text(t"Glossary")) :: entries
