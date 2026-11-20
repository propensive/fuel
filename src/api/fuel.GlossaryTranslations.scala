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
import dysasymptotics.linearSize
import pyrocosm.{Block, Tone}

// A glossary as another language reads it: a glossary of the same terms, each named as the
// glossary it was written in names it, which says beneath the name what the term is in its own
// language, and defines it in that language:
//
//   define library
//     term bibliothèque
//     keyword lib
//     synonym  bibliothèque logicielle
//     definition
//         Un ensemble de code que d'autres programmes utilisent.
//
// So a term is tied to the one it translates by the name its entry keeps, whatever the
// language. An entry with no `term` is called what the original calls it; its `keyword`s and
// `synonym`s are its own language's, in place of the original term's; whether it is `unlisted`
// is the original term's, the same in every language; and a `link` is written only where the
// page which explains the term is another in this language, which a page of the site never
// is. It is the text an Orthant store holds for the glossary in the language: `foreign` is
// the glossary a reader of the language has, and `check` what is wrong with it as one. That
// it says all the original says, and of the text as it now is, is Orthant's to hold it to.
object GlossaryTranslations:
  // The code of what is wrong with a glossary in another language, `↯/f.26.1.<variant>`: an
  // entry for no term of the original (2), or for one which has an entry already (3); a `term`
  // which cannot be one (7), which names another term (8), or which is another entry's (9); a
  // synonym which cannot be a name (10), or which is one already (11); and a `link` in place
  // of the page of the site which explains the term (12). The others — 1, and 4 to 6 — were
  // of a translation Fuel held to its text itself, and are no longer raised.
  private def bolt(variant: Int): fulminate.Bolt =
    FuelBolts(FuelBolts.api, FuelBolts.glossaryTranslation, variant)

  // An entry of a glossary or of a translation of one, as the schema reads it: the name on its
  // `define` line; its `term` (the line's second value, or a child), `keyword`s, `link` and
  // `synonym`s; its definition's text; the line (from 1) of the entry; and the line its
  // definition's text begins on.
  case class Entry
    ( name:       Text,
      term:       Optional[Text],
      keywords:   List[Text],
      link:       Optional[Text],
      definition: Optional[Text],
      line:       Int,
      site:       Int,
      synonyms:   List[Text] = Nil )

  private def joined(atoms: List[Text]): Text = atoms.filter(_ != t"").join(t" ").trim

  // Every entry of a glossary, in the order of the file; none if it is not TEL.
  def entries(text: Text): List[Entry] =
    import parsing.trackPositions

    safely(text.read[Tel]).lay(List[Entry]()): (tel: Tel) =>
      val defined: List[Tel.Compound] = tel.childCompounds.to[List].filter(_.keyword == t"define")

      defined.indexed.map: (compound: Tel.Compound, index: Ordinal) =>
        val at: List[Text] = List(t"define", index.n0.show)
        val children: List[Tel.Compound] = Compounds.children(compound)

        // Each child's first value, by keyword: what the schema reads a value of one as.
        def all(keyword: Text): List[Text] =
          children.filter(_.keyword == keyword).map: (child: Tel.Compound) =>
            Compounds.inline(child).filter(_ != t"").head.or(t"")

          . filter(_ != t"")

        def child(keyword: Text): Optional[Text] = all(keyword).head
        val atoms: List[Text] = Compounds.inline(compound).filter(_ != t"")

        val definition: Optional[(Text, Int)] =
          children.seek(_.keyword == t"definition").let: (body: Tel.Compound) =>
            val brief: Text = joined(Compounds.inline(body))
            val line: Int = Compounds.line(tel, at + List(t"definition"))

            Compounds.source(body).let { (text: Text) => (text, line + 1) }.or:
              if brief == t"" then Unset else (brief, line)

        val line: Int = Compounds.line(tel, at)

        Entry
          ( atoms.head.or(t""), Glossary.second(atoms).or(child(t"term")), all(t"keyword"),
            child(t"link"), definition.let(_(0)), line, definition.let(_(1)).or(line),
            all(t"synonym") )

  // The glossary a reader of a translation's language has, of the glossary `definitive` read
  // from the file it translates: each of its terms as the translation says it — its `term`,
  // its definition as its translation reads, its own keywords and synonyms, and its `link` if
  // it has one — and as it is defined where the translation says nothing of it. Whether a
  // term is unlisted is the definitive term's. Where a term's name is not the name it
  // translates, `original` says that name, and is the definition's last paragraph, so that a
  // reader knows the word the original text uses.
  def foreign
    ( path: Text, text: Text, definitive: Glossary, original: Optional[Text => Text] = Unset )
  :   Glossary =
    val theirs: List[Entry] = entries(text)

    Glossary:
      definitive.terms.map: (term: Glossary.Term) =>
        theirs.seek(_.name == term.name).lay(term): (entry: Entry) =>
          val name: Text = entry.term.lay(term.name): (name: Text) =>
            if Glossary.nameable(name) then name else term.name

          val read: Text = entry.definition.or(term.definition)

          val definition: Text =
            if name.lower == term.name.lower then read
            else original.lay(read) { (note: Text => Text) => t"$read\n\n${note(term.name)}" }

          Glossary.Term
            ( name, definition, entry.link.or(term.link), path, entry.line, entry.keywords,
              term.name, entry.synonyms, term.unlisted )

  // What is wrong with a glossary as another language reads it, read from `text` at `path`,
  // of the glossary `definitive` it was written as: what is wrong with it as TEL, and to the
  // schema; an entry for a term the original does not have, or has an entry for already; a
  // `term` which cannot be one, or which two entries share; a `link` in place of one to a
  // page of the site; and, as a caution, a `term` which is the name of another term of the
  // glossaries, `names`, as they are defined.
  def check(path: Text, text: Text, definitive: Text, names: List[Text] = Nil): List[Block] =
    Glossary.read(path, text) match
      case diagnostic: Block.Diagnostic =>
        List(diagnostic)

      case _ if !Glossary.faults(path, text, translation = true).nil =>
        Glossary.faults(path, text, translation = true)

      case _ =>
        val theirs: List[Entry] = entries(text)
        val ours: List[Entry] = entries(definitive)
        val all: List[Text] = (ours.map(_.name) + names).distinct

        def finding(tone: Tone, line: Int, message: Text, variant: Int): List[Block] =
          val location: Block.Location = Block.Location(path, line)
          val said: List[Block] = List(Block.paragraph(message))
          List(Block.Diagnostic(tone, location, said, code = bolt(variant)))

        def optional(value: Optional[Text]): List[Text] = value.lay(List[Text]())(List(_))

        val entered: List[Block] = theirs.indexed.bind:
          (entry: Entry, index: Ordinal) =>
            val earlier: List[Entry] = theirs.keep(index.n0)

            val unknown: List[Block] =
              finding
                ( Tone.Warning, entry.line,
                  t"The glossary as it was written has no term `${entry.name}`.", 2 )

            ours.seek(_.name == entry.name).lay(unknown): (original: Entry) =>
              if earlier.exists(_.name == entry.name) then
                finding(Tone.Failure, entry.line, t"`${entry.name}` has an entry already.", 3)
              else
                val named: List[Block] = optional(entry.term).bind: (term: Text) =>
                  val slug: Text = Blocks.slug(term)
                  val shared: Optional[Entry] = earlier.seek(_.term.lay(false)(Blocks.slug(_) == slug))

                  val friend: Optional[Text] =
                    all.seek { (name: Text) => name != entry.name && Blocks.slug(name) == slug }

                  if !Glossary.nameable(term) then
                    finding
                      ( Tone.Failure, entry.line,
                        t"`$term` cannot be a glossary term: a term is written in brackets, so " +
                          t"it cannot hold one.",
                        7 )
                  else shared.lay(optional(friend).bind { (name: Text) =>
                    finding
                      ( Tone.Warning, entry.line,
                        t"`$term` is also the name of another term, `$name`, as the glossary " +
                          t"defines it: in this language `[$term]` is `${entry.name}`.",
                        8 ) }):
                    (shared: Entry) =>
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$term` is the term of `${shared.name}` already, so `[$term]` would " +
                            t"not say which is meant.",
                          9 )

                // A synonym which cannot be a name, or which is a name in this language already:
                // this term's, or another's term or synonym.
                val synonymous: List[Block] =
                  val others: List[Text] =
                    theirs.filter(_.name != entry.name).bind: (other: Entry) =>
                      optional(other.term) + other.synonyms

                  entry.synonyms.indexed.bind: (synonym: Text, index: Ordinal) =>
                    val slug: Text = Blocks.slug(synonym)
                    val own: List[Text] = optional(entry.term) + entry.synonyms.keep(index.n0)

                    if !Glossary.nameable(synonym) then
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$synonym` cannot be a synonym: a name is written in brackets, so it " +
                            t"cannot hold one.",
                          10 )
                    else if (own + others).exists(Blocks.slug(_) == slug) then
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$synonym` is a name in this language already, so `[$synonym]` " +
                            t"would not say which term is meant.",
                          11 )
                    else Nil

                val linked: List[Block] = optional(entry.link).bind: (link: Text) =>
                  val local: Boolean = original.link.lay(false)(_.starts(t"/"))

                  if !local || original.link.lay(false)(_ == link) then Nil
                  else
                    finding
                      ( Tone.Warning, entry.line,
                        t"`${entry.name}` is explained by a page of the site, " +
                          t"`${original.link.or(t"")}`, which is the same page in every " +
                          t"language: this `link` is in place of it.",
                        12 )

                named + synonymous + linked

        entered
