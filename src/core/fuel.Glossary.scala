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

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Inline, Tone}
import soundness.collationComparable
import soundness.sortingAlgorithms.timsort

// The glossary: terms defined in TEL files, each `define <term>` holding a Markdown
// `definition` and, when the term is explained in full elsewhere, a `link` to that page. A
// `[term]` in prose is annotated with the definition and leads to the link, or to the term's
// entry on the glossary page. The files named by the `glossary` setting are combined; a term
// defined twice is a failure at the second. The format is the schema `glossary`
// (`fuel/glossary.schema.tel`), in which a value with spaces follows two spaces.
//
//     define  user interface
//       synonym UI
//       link  /doc/guide/interfaces.md
//       definition
//           A shared boundary where a person and a system meet and exchange information.
object Glossary:
  // `keywords` are what a search finds the term by, besides its name; they are shown nowhere.
  // `synonyms` are other names for it: a search finds it by them, `[synonym]` in prose is the
  // term, and the glossary page lists each as leading to it. An `unlisted` term is on no page
  // and found by no search, but `[term]` in prose still shows its definition. `origin` is the
  // term this one translates, as the glossary which defines it names it: a term of a
  // glossary's translation has one, and a term as it is defined has none.
  case class Term
    ( name:       Text,
      definition: Text,
      link:       Optional[Text],
      file:       Text,
      line:       Int,
      keywords:   List[Text]     = Nil,
      origin:     Optional[Text] = Unset,
      synonyms:   List[Text]     = Nil,
      unlisted:   Boolean        = false ):

    // The names `[name]` in prose means the term by: its own and its synonyms.
    def names: List[Text] = name :: synonyms

    // Whether a word of a query names the term: its name, a synonym, the name of the term it
    // translates, or a keyword, in any case.
    def matches(word: Text): Boolean =
      val sought: Text = word.lower

      names.exists(_.lower == sought) || origin.lay(false)(_.lower == sought) ||
        keywords.exists(_.lower == sought)

    // The term's anchor on the glossary page, and the key a document's `[term]` leads to: that
    // of the term it translates, so that a link to a term means the same in every language.
    def slug: Text = Blocks.slug(origin.or(name))

  // Whether a name can be a term's: a term is written in brackets, so it cannot hold one.
  def nameable(name: Text): Boolean =
    name != t"" && !name.s.contains("[") && !name.s.contains("]")

  val none: Glossary = Glossary(Nil)

  // The scheme of the link reference definitions the glossary adds to a document, by which a
  // `[term]` the parser linked is known as the glossary's.
  val scheme: Text = t"glossary:"



  val schemaResource: Text = t"fuel/glossary.schema.tel"

  // The schema's document: read through the thread-context classloader, since under Burdock
  // the system one sees only the launcher's own jar.
  def schemaSource: Text =
    Classloader.threadContext.resource(schemaResource).let(_.utf8).or:
      panic(m"the resource $schemaResource is missing")

  lazy val schema: stratiform.Tels =
    import strategies.throwUnsafely
    stratiform.Tels.Validation.validate(schemaSource.read[Tel].as[stratiform.Tels])

  // A value of a glossary, after two spaces where it has one, so that it is one value, as the
  // schema reads it.
  def atom(value: Text): Tel.Atom = Tel.Atom.Inline(value, if value.contains(t" ") then 2 else 1)

  // A compound of a glossary: a keyword and its value.
  def compound(keyword: Text, value: Text): Tel.Compound =
    Tel.Compound(keyword, Array(atom(value)), Unset, Array.empty)

  private case class Faults(items: List[(Tel.Error, Text, Span)] = Nil)
  extends Error(m"the glossary has ${items.size} faults"):
    def +(error: Tel.Error, pointer: Text, span: Span): Faults =
      Faults(items :+ (error, pointer, span))

  // What a fault the schema finds means in a glossary.
  private def explain(error: Tel.Error, pointer: Text): Text =
    import Tel.Error.Reason
    val last: Text = pointer.cut(t"/").filter(_ != t"").reverse.head.or(t"")

    error.reason match
      case Reason.UnknownKeyword =>
        t"A glossary's term has a `definition`, and perhaps a `term`, `unlisted`, `keyword`s, " +
          t"`synonym`s and a `link`, but not `$last`."

      case Reason.TooManyAtoms | Reason.AtomAtNonAssignablePos =>
        t"This has more values than it takes: a value with spaces in it follows two spaces, " +
          t"as `keyword  policy import`."

      case Reason.NonRepeatableTooMany =>
        t"This says `$last` more than once."

      case _ =>
        error.message.text

  // What the schema finds wrong with a glossary, each at its line of the file; and, in a
  // glossary which is not a translation, a `term`, which only a translation says — most often
  // the second word of a name written after one space, which the schema reads as the term.
  def faults(path: Text, text: Text, translation: Boolean = false): List[Block] =
    import strategies.throwUnsafely
    import parsing.trackPositions

    safely(text.read[Tel]).lay(Nil): (tel: Tel) =>
      val faults: Faults =
        validate[Tel.Focus](Faults()):
          case error: Tel.Error =>
            val pointer: Text = prior.let(_.pointer.encode).or(t"/")
            accrual + (error, pointer, prior.lay(Span.empty)(_.span))

        . protect(Tel.Type.assign(tel, schema))

      val schemed: List[Block] = faults.items.map: (error, pointer, span) =>
        val line: Int = if span.exists then span.startLine.lay(1)(_.n1) else 1
        val location: Block.Location = Block.Location(path, line.max(1))
        Block.Diagnostic(Tone.Failure, location, List(Block.paragraph(explain(error, pointer))))

      val named: List[Block] =
        if translation then Nil else entries(text).filter(_(2).present).map: (name, line, term) =>
          val message: Text =
            t"`$name` has a `term`, which only a glossary's translation says: a name of " +
              t"several words follows two spaces, as `define  $name ${term.or(t"")}`."

          Block.Diagnostic
            ( Tone.Failure, Block.Location(path, line), List(Block.paragraph(message)) )

      schemed + named

  // Each term's name, line and `term`, as the schema reads them: the `term` is the line's
  // second value, or a child.
  private def entries(text: Text): List[(Text, Int, Optional[Text])] =
    import parsing.trackPositions

    safely(text.read[Tel]).lay(Nil): (tel: Tel) =>
      val compounds: List[Tel.Compound] = tel.childCompounds.to[List].filter(_.keyword == t"define")
      val nodes: List[Tel] = tel.fields(t"define").to[List]

      nodes.zip(compounds).indexed.map: (pair: (Tel, Tel.Compound), index: Ordinal) =>
        val atoms: List[Text] = Compounds.inline(pair(1))
        val line: Int = Compounds.line(tel, List(t"define", index.n0.show))
        (atoms.head.or(t""), line, second(atoms).or(first(pair(0), t"term")))

  // A list's second element, if it has one.
  def second(values: List[Text]): Optional[Text] = values match
    case _ :: value :: _ => value
    case _               => Unset

  // The first value of each of a node's children by keyword, as the schema reads it.
  private def values(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].map(_.atomTexts.to[List].filter(_ != t"").head.or(t""))
    . filter(_ != t"")

  private def first(node: Tel, keyword: Text): Optional[Text] = values(node, keyword).head

  // The text of a node's child by keyword: its atoms, inline then the indented source, joined.
  private def child(node: Tel, keyword: Text): Optional[Text] =
    node.fields(keyword).to[List].head.let(_.atomTexts.to[List].filter(_ != t"").join(t" ")).let:
      (text: Text) => if text == t"" then Unset else text

  // A glossary read from a file's text, or the diagnostic which says why it could not be.
  def read(path: Text, text: Text): Glossary | Block.Diagnostic =
    import parsing.trackPositions

    recover:
      case error: Tel.Error => Block.Diagnostic.of(error, path, text)

    . protect[Glossary | Block.Diagnostic]:
        val tel: Tel = text.read[Tel]

        // A node, and the compound it wraps (whose atoms are the name), paired by order:
        // `fields` and `childCompounds` walk the same children.
        val nodes: List[Tel] = tel.fields(t"define").to[List]

        val compounds: List[Tel.Compound] =
          tel.childCompounds.to[List].filter(_.keyword == t"define")

        val terms: List[Term] = nodes.zip(compounds).indexed.map:
          (pair: (Tel, Tel.Compound), index: Ordinal) =>
            val node: Tel = pair(0)
            val name: Text = Compounds.inline(pair(1)).head.or(t"").trim
            val unlisted: Boolean = !node.fields(t"unlisted").to[List].nil
            val line: Int = Compounds.line(tel, List(t"define", index.n0.show))

            Term
              ( name, child(node, t"definition").or(t""), first(node, t"link"), path, line,
                values(node, t"keyword"), Unset, values(node, t"synonym"), unlisted )

        Glossary(terms)

  // Every glossary as one, with the problems: a term defined twice, or a name — a term's or a
  // synonym's — which another term has already (by its slug, so that two spellings of one
  // anchor are one name too); a name which cannot be a reference label (empty, or holding a
  // bracket); and a caution for a name of one character, which a `[1]` or an `[x]` in prose
  // would match, since labels are matched case-insensitively.
  def merge(glossaries: List[Glossary]): (Glossary, List[Block]) =
    def report(tone: Tone, term: Term, message: Text): Block =
      Block.Diagnostic(tone, Block.Location(term.file, term.line), List(Block.paragraph(message)))

    def recur(rest: List[Term], kept: List[Term], problems: List[Block]): (Glossary, List[Block]) =
      rest match
        case term :: tail =>
          if !nameable(term.name) then
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

                // A synonym which cannot be a name, or which is a name already: the term's own,
                // another of its synonyms, or another term's name or synonym.
                val taken: List[(Text, Term)] =
                  kept.bind { (other: Term) => other.names.map((_, other)) } +
                    List((term.name, term))

                def synonyms(rest: List[Text], seen: List[(Text, Term)]): List[Block] = rest match
                  case synonym :: more =>
                    if !nameable(synonym) then
                      val message: Text =
                        t"`$synonym` cannot be a synonym: a name is written in brackets, so it " +
                          t"cannot hold one."

                      report(Tone.Failure, term, message) :: synonyms(more, seen)
                    else
                      seen.seek(entry => Blocks.slug(entry(0)) == Blocks.slug(synonym)) match
                        case (_, other: Term) =>
                          val whose: Text =
                            if other eq term then t"of `${term.name}` already"
                            else t"`${other.name}`'s already, at ${other.file}:${other.line.show}"

                          val message: Text = t"`$synonym` is a name $whose."
                          report(Tone.Failure, term, message) :: synonyms(more, seen)

                        case _ =>
                          synonyms(more, seen + List((synonym, term)))

                  case _ =>
                    Nil

                val clashes: List[Block] = synonyms(term.synonyms, taken)
                val clean: Term = if clashes.nil then term else term.copy(synonyms = Nil)

                recur(tail, kept + List(clean), problems + short + clashes)

        case _ =>
          (Glossary(kept), problems)

    recur(glossaries.bind(_.terms), Nil, Nil)

case class Glossary(terms: List[Glossary.Term]):
  def apply(slug: Text): Optional[Glossary.Term] = terms.seek(_.slug == slug)

  // The terms a word names, by their names, synonyms or keywords: those listed, since a
  // search leads to a term's entry on the glossary page.
  def search(word: Text): List[Glossary.Term] =
    terms.filter { (term: Glossary.Term) => !term.unlisted && term.matches(word) }

  // The link reference definitions added after a document's text before it is parsed, so that
  // `[term]` is a link the parser makes, not a pattern searched for; a definition the author
  // writes first wins, as CommonMark has it.
  //
  // A term is named by its synonyms too. A translated term is named by the name it translates
  // too, since a passage of a translation nobody has translated reads as the text it
  // translates; its own names come first, so that a name which is one term's here and
  // another's there is this language's.
  def definitions: Text =
    def line(name: Text, term: Glossary.Term): Text =
      t"[$name]: ${Glossary.scheme}${term.slug}"

    val own: List[Text] = terms.bind { term => term.names.map(line(_, term)) }

    val origins: List[Text] = terms.bind: (term: Glossary.Term) =>
      term.origin.lay(List[Text]()): (origin: Text) =>
        if origin.lower == term.name.lower then Nil else List(line(origin, term))

    (own + origins).join(t"\n")

  // The terms in the order of a language's dictionary.
  def sorted(locale: Locale[Phrases.Spoken]): List[Glossary.Term] =
    given Collation = Phrases.collation(locale)
    terms.order(_.name)

  // The glossary page: each listed term under a heading anchored by its slug, with its
  // definition and where it is explained in full, and each of its synonyms under its own,
  // leading to it; in the order of the dictionary of the language it is read in, whose words
  // Fuel's own are.
  def page(linker: Linker, locale: Locale[Phrases.Spoken] = Phrases.english): List[Block] =
    given Locale[Phrases.Spoken] = locale

    given Collation = Phrases.collation(locale)
    val listed: List[Glossary.Term] = terms.filter(!_.unlisted)

    def see(destination: Inline.Destination, text: Text): Block =
      val link: Inline = Inline.Link(destination, Inline.text(text))
      val see: Inline = Inline.Textual(t"${Phrases.see()} ")
      Block.Paragraph(List(see, link, Inline.Textual(t".")))

    val defined: List[(Text, List[Block])] = listed.map: (term: Glossary.Term) =>
      val detail: List[Block] = Blocks.of(Parser.parse(term.definition), Nil, linker)

      val more: List[Block] = term.link.lay(Nil: List[Block]): (url: Text) =>
        val destination: Inline.Destination =
          if url.starts(t"/") then Inline.Destination.Local(url)
          else Inline.Destination.External(url)

        List(see(destination, url))

      (term.name, Block.Heading(2, Inline.text(term.name), term.slug) :: detail + more)

    val synonyms: List[(Text, List[Block])] = listed.bind: (term: Glossary.Term) =>
      term.synonyms.map: (synonym: Text) =>
        val destination: Inline.Destination = Inline.Destination.Local(t"/glossary", term.slug)
        val heading: Block = Block.Heading(2, Inline.text(synonym), Blocks.slug(synonym))
        (synonym, List(heading, see(destination, term.name)))

    val entries: List[Block] = (defined + synonyms).order(_(0)).bind(_(1))

    Block.Heading(1, Inline.text(Phrases.glossary())) :: entries
