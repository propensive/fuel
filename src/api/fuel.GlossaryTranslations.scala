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

// The translation of a glossary: a glossary of the same terms, each named as the glossary it
// translates names it, which says beneath the name what the term is in its own language, and
// whose `definition` is a `Translation` of the definitive glossary's:
//
//   define library
//     term bibliothèque
//     keyword lib
//     synonym  bibliothèque logicielle
//     definition
//         - A collection of code that other programs use.
//         + Un ensemble de code que d'autres programmes utilisent.
//
// So a term is tied to the one it translates by the file itself. An entry with no `term` is
// one nobody has translated, and is the definitive term; its `keyword`s and `synonym`s are its
// own language's, in place of the definitive term's; whether it is `unlisted` is the definitive
// term's, the same in every language; and a `link` is written only where the page
// which explains the term is another in this language, which a page of the site never is. A
// translation is made from the definitive glossary (`generate`), and made again from it when
// it changes; `check` holds one to the other; and `foreign` is the glossary a reader of the
// language has.
object GlossaryTranslations:
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

  // The translation of a definitive glossary: each of its terms an entry, with the `term`,
  // the `keyword`s, the `synonym`s and the `link` the translation `old` gives it (and the
  // definitive term's keywords and synonyms where it is new); and its definition a
  // `Translation` — that of `old`, brought up to date, or one nobody has translated. The
  // glossary is read as TEL and written again with each term rewritten, so that what else it
  // has, its comments among them, is kept. `Unset` if the glossary is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    val prior: List[Entry] = old.lay(List[Entry]())(entries(_))

    def written(entry: Entry, compound: Tel.Compound): Tel.Compound =
      val before: Optional[Entry] = prior.seek(_.name == entry.name)
      val keywords: List[Text] = before.lay(entry.keywords)(_.keywords)
      val synonyms: List[Text] = before.lay(entry.synonyms)(_.synonyms)

      val definition: List[Tel.Compound] = entry.definition.lay(List[Tel.Compound]()): (text: Text) =>
        val translation: Translation = before.let(_.definition).lay(Translation.skeleton(text)):
          (old: Text) => Translation.update(Translation.parse(old), text)

        List(Tel.Compound(t"definition", Array(Tel.Atom.Source(translation.serialize)), Unset, Array.empty))

      val children: List[Tel.Compound] =
        before.let(_.term).lay(List[Tel.Compound]())(term => List(Glossary.compound(t"term", term))) +
          keywords.map(Glossary.compound(t"keyword", _)) +
          synonyms.map(Glossary.compound(t"synonym", _)) +
          before.let(_.link).lay(List[Tel.Compound]())(link => List(Glossary.compound(t"link", link))) +
          definition

      val block: Tel.Block = Tel.Block(Array.empty, Unset, children.to[Array], 0)

      Tel.Compound
        ( t"define", Array(Glossary.atom(entry.name)), compound.remark,
          if children.nil then Array.empty else Array(block) )

    safely:
      val tel: Tel = definitive.read[Tel]
      val defined: List[Tel.Compound] = tel.childCompounds.to[List].filter(_.keyword == t"define")

      val replacements: List[Tel.Mutation.Op] =
        defined.zip(entries(definitive)).indexed.map:
          case ((compound: Tel.Compound, entry: Entry), index: Ordinal) =>
            val pointer: Tel.Pointer = Tel.Pointer.Empty / (t"define", index.n0)
            Tel.Mutation.Op.Replace(pointer, written(entry, compound))

      Tel.Mutation(tel, replacements).show

  private def translated(entry: Entry): Boolean =
    entry.term.present || entry.definition.lay(false): (text: Text) =>
      !Translation.parse(text).passages.filter(!_.foreign.nil).nil

  // The entries of a translation which someone has translated and the definitive glossary no
  // longer has: what making the translation again leaves out.
  def lost(definitive: Text, old: Text): List[Entry] =
    val kept: List[Text] = entries(definitive).map(_.name)
    entries(old).filter { (entry: Entry) => !kept.has(entry.name) && translated(entry) }

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

          val read: Text =
            entry.definition.let(Translation.parse(_).foreign.text).or(term.definition)

          val definition: Text =
            if name.lower == term.name.lower then read
            else original.lay(read) { (note: Text => Text) => t"$read\n\n${note(term.name)}" }

          Glossary.Term
            ( name, definition, entry.link.or(term.link), path, entry.line, entry.keywords,
              term.name, entry.synonyms, term.unlisted )

  // What is wrong with the translation of a glossary, read from `text` at `path`, of the
  // definitive glossary `definitive`: what is wrong with it as TEL; each definition which is
  // not a translation of the definitive term's, as `Translation` and `Mirror` find; a term
  // nobody has given a `term`; a term one has and the other lacks; a `term` which cannot be
  // one, or which two entries share; a `link` in place of one to a page of the site; and, as
  // a caution, a `term` which is the name of another term of the glossaries, `names`, as they
  // are defined.
  def check(path: Text, text: Text, definitive: Text, names: List[Text] = Nil): List[Block] =
    Glossary.read(path, text) match
      case diagnostic: Block.Diagnostic =>
        List(diagnostic)

      case _ if !Glossary.faults(path, text, translation = true).nil =>
        Glossary.faults(path, text, translation = true)

      case _ =>
        val theirs: List[Entry] = entries(text)
        val ours: List[Entry] = entries(definitive)
        val remedy: Text = t"`fuel translate --update` makes the translation again"
        val all: List[Text] = (ours.map(_.name) + names).distinct

        def finding(tone: Tone, line: Int, message: Text): List[Translation.Finding] =
          List(Translation.Finding(tone, line, message))

        def optional(value: Optional[Text]): List[Text] = value.lay(List[Text]())(List(_))

        val absent: List[Translation.Finding] = ours.bind: (entry: Entry) =>
          if theirs.exists(_.name == entry.name) then Nil
          else finding(Tone.Warning, 1, t"This has no entry for `${entry.name}`: $remedy.")

        val entered: List[Translation.Finding] = theirs.indexed.bind:
          (entry: Entry, index: Ordinal) =>
            val earlier: List[Entry] = theirs.keep(index.n0)

            val unknown: List[Translation.Finding] =
              finding
                ( Tone.Warning, entry.line,
                  t"The glossary this translates has no term `${entry.name}`: $remedy." )

            ours.seek(_.name == entry.name).lay(unknown): (original: Entry) =>
              if earlier.exists(_.name == entry.name) then
                finding(Tone.Failure, entry.line, t"`${entry.name}` has an entry already.")
              else
                val gone: List[Translation.Finding] =
                  entry.definition.lay(List[Translation.Finding]()): _ =>
                    finding
                      ( Tone.Warning, entry.site, t"The text this translates has gone: $remedy." )

                val prose: List[Translation.Finding] = original.definition.lay(gone):
                  (defined: Text) =>
                    val lacking: List[Translation.Finding] =
                      finding
                        ( Tone.Warning, entry.line,
                          t"This lacks the definition of `${entry.name}`, which is not " +
                            t"translated: $remedy." )

                    entry.definition.lay(lacking): (found: Text) =>
                      (Translation.findings(found, defined) + Mirror.findings(found))
                      . map(_.shifted(entry.site - 1))

                val unnamed: List[Translation.Finding] =
                  if entry.term.present then Nil else
                    val message: Text =
                      t"`${entry.name}` is not translated: a `term` beneath it says what it " +
                        t"is in this language."

                    List(Translation.Finding(Tone.Warning, entry.line, message, untranslated = true))

                val named: List[Translation.Finding] = optional(entry.term).bind: (term: Text) =>
                  val slug: Text = Blocks.slug(term)
                  val shared: Optional[Entry] = earlier.seek(_.term.lay(false)(Blocks.slug(_) == slug))

                  val friend: Optional[Text] =
                    all.seek { (name: Text) => name != entry.name && Blocks.slug(name) == slug }

                  if !Glossary.nameable(term) then
                    finding
                      ( Tone.Failure, entry.line,
                        t"`$term` cannot be a glossary term: a term is written in brackets, so " +
                          t"it cannot hold one." )
                  else shared.lay(optional(friend).bind { (name: Text) =>
                    finding
                      ( Tone.Warning, entry.line,
                        t"`$term` is also the name of another term, `$name`, as the glossary " +
                          t"defines it: in this language `[$term]` is `${entry.name}`." ) }):
                    (shared: Entry) =>
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$term` is the term of `${shared.name}` already, so `[$term]` would " +
                            t"not say which is meant." )

                // A synonym which cannot be a name, or which is a name in this language already:
                // this term's, or another's term or synonym.
                val synonymous: List[Translation.Finding] =
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
                            t"cannot hold one." )
                    else if (own + others).exists(Blocks.slug(_) == slug) then
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$synonym` is a name in this language already, so `[$synonym]` " +
                            t"would not say which term is meant." )
                    else Nil

                val linked: List[Translation.Finding] = optional(entry.link).bind: (link: Text) =>
                  val local: Boolean = original.link.lay(false)(_.starts(t"/"))

                  if !local || original.link.lay(false)(_ == link) then Nil
                  else
                    finding
                      ( Tone.Warning, entry.line,
                        t"`${entry.name}` is explained by a page of the site, " +
                          t"`${original.link.or(t"")}`, which is the same page in every " +
                          t"language: this `link` is in place of it." )

                unnamed + prose + named + synonymous + linked

        val diagnostics: List[Block.Diagnostic] = Translation.report(path, text, absent + entered)

        diagnostics.map { (diagnostic: Block.Diagnostic) => diagnostic: Block }

  // How much of a glossary's translation is done: the sum over its definitions, with each
  // term nobody has given a `term` one more to translate, and a term the translation lacks
  // one more change to make.
  def progress(text: Text, definitive: Text): Translation.Progress =
    val theirs: List[Entry] = entries(text)

    entries(definitive).fold(Translation.Progress(0, 0, 0, 0, 0)):
      (total: Translation.Progress, entry: Entry) =>
        theirs.seek(_.name == entry.name).lay(total.copy(stale = total.stale + 1)):
          (found: Entry) =>
            val prose: Translation.Progress = entry.definition.lay(Translation.Progress(0, 0, 0, 0, 0)):
              (defined: Text) =>
                found.definition.lay(Translation.Progress(0, 0, 0, 0, 1)): (text: Text) =>
                  Translation.progress(Translation.parse(text), defined)

            val named: Int = if found.term.present then 1 else 0

            Translation.Progress
              ( total.translated + prose.translated + named,
                total.untranslated + prose.untranslated + 1 - named,
                total.pending + prose.pending, total.orphaned + prose.orphaned,
                total.stale + prose.stale )

  // A glossary at `path` held to its translations, each a language and a path, as
  // `Translations.check` holds a document.
  def verify
    ( path:         Text,
      text:         Text,
      translations: List[(Text, Text)],
      read:         Text -> Optional[Text],
      partial:      List[Text] = Nil,
      names:        List[Text] = Nil )
  :   List[Block] =

    translations.bind: (tongue: Text, target: Text) =>
      val missing: List[Block] =
        if partial.has(tongue) then Nil else List(Translations.absent(path, tongue, target))

      read(target).lay(missing): (source: Text) =>
        check(target, source, text, names)
