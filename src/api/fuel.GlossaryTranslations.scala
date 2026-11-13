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
import pyrocosm.{Block, Tone}

// The translation of a glossary: a glossary of the same terms, each named as the glossary it
// translates names it, which says beneath the name what the term is in its own language, and
// whose `definition` is a `Translation` of the definitive glossary's:
//
//   define library
//     term bibliothèque
//     keyword lib
//     definition
//         - A collection of code that other programs use.
//         + Un ensemble de code que d'autres programmes utilisent.
//
// So a term is tied to the one it translates by the file itself. An entry with no `term` is
// one nobody has translated, and is the definitive term; its `keyword`s are its own
// language's, in place of the definitive term's; and a `link` is written only where the page
// which explains the term is another in this language, which a page of the site never is. A
// translation is made from the definitive glossary (`generate`), and made again from it when
// it changes; `check` holds one to the other; and `foreign` is the glossary a reader of the
// language has.
object GlossaryTranslations:
  // An entry of a glossary or of a translation of one, as it is written: the name on its
  // `define` line; its `term`, `keyword`s and `link`; its definition's text; the line (from 1)
  // of the entry; and the line its definition's text begins on.
  case class Entry
    ( name:       Text,
      term:       Optional[Text],
      keywords:   List[Text],
      link:       Optional[Text],
      definition: Optional[Text],
      line:       Int,
      site:       Int )

  private def parsed(text: Text): Optional[(Tel, Overlay.Lines)] =
    import strategies.throwUnsafely

    try
      val tel: Tel = Overlay.headed(text).read[Tel]
      (tel, Overlay.Lines(text, tel.childCompounds.to[List]))
    catch case error: Tel.Error => Unset

  private def joined(atoms: List[Text]): Text = atoms.filter(_ != t"").join(t" ").trim

  // Every entry of a glossary, in the order of the file; none if it is not TEL.
  def entries(text: Text): List[Entry] =
    parsed(text).lay(List[Entry]()): (tel: Tel, lines: Overlay.Lines) =>
      tel.childCompounds.to[List].filter(_.keyword == t"define").map: (compound: Tel.Compound) =>
        val children: List[Tel.Compound] = Overlay.childrenOf(compound)

        def child(keyword: Text): Optional[Text] =
          children.filter(_.keyword == keyword).head.let: (child: Tel.Compound) =>
            val text: Text = joined(Overlay.inlineAtoms(child))
            if text == t"" then Unset else text

        val keywords: List[Text] =
          children.filter(_.keyword == t"keyword").map(child => joined(Overlay.inlineAtoms(child)))
          . filter(_ != t"")

        val body: Optional[Tel.Compound] = children.filter(_.keyword == t"definition").head

        val definition: Optional[(Text, Int)] = body.let: (body: Tel.Compound) =>
          val brief: Text = joined(Overlay.inlineAtoms(body))

          Overlay.sourceAtom(body).let { (text: Text) => (text, lines(body) + 1) }.or:
            if brief == t"" then Unset else (brief, lines(body))

        Entry
          ( joined(Overlay.inlineAtoms(compound)), child(t"term"), keywords, child(t"link"),
            definition.let(_(0)), lines(compound), definition.let(_(1)).or(lines(compound)) )

  private def set(lines: sci.Vector[String], indent: Int): sci.Vector[String] =
    lines.map { (line: String) => if line == "" then "" else " ".repeat(indent).nn + line }

  // The translation of a definitive glossary: each of its terms an entry, with the `term`,
  // the `keyword`s and the `link` the translation `old` gives it (and the definitive term's
  // keywords where it is new), and its definition a `Translation` — that of `old`, brought up
  // to date, or one nobody has translated. What comes before the first term in the definitive
  // glossary comes first here. `Unset` if the glossary is not TEL.
  def generate(definitive: Text, old: Optional[Text] = Unset): Optional[Text] =
    if parsed(definitive).absent then Unset else
      val prior: sci.List[Entry] = old.lay(sci.List[Entry]())(entries(_).stdlib)

      val head: sci.Vector[String] =
        Translation.split(definitive).takeWhile(!_.startsWith("define"))
        . reverse.dropWhile(_.trim.nn == "").reverse

      val written: sci.List[sci.Vector[String]] = entries(definitive).stdlib.map: (entry: Entry) =>
        val before: Option[Entry] = prior.find(_.name == entry.name)

        val term: sci.Vector[String] =
          before.flatMap(_.term.option).toVector.map { (term: Text) => s"  term ${term.s}" }

        val keywords: sci.Vector[String] =
          before.map(_.keywords).getOrElse(entry.keywords).stdlib.toVector.map: (keyword: Text) =>
            s"  keyword ${keyword.s}"

        val link: sci.Vector[String] =
          before.flatMap(_.link.option).toVector.map { (link: Text) => s"  link ${link.s}" }

        val definition: sci.Vector[String] =
          entry.definition.option.toVector.flatMap: (text: Text) =>
            val translation: Translation =
              before.flatMap(_.definition.option) match
                case Some(old) => Translation.update(Translation.parse(old), text)
                case None      => Translation.skeleton(text)

            sci.Vector("  definition") ++ set(Translation.split(translation.serialize), 6)

        sci.Vector(s"define ${entry.name.s}") ++ term ++ keywords ++ link ++ definition

      val blocks: sci.List[sci.Vector[String]] =
        if head.isEmpty then written else head :: written

      Text(blocks.map(_.mkString("\n")).mkString("\n\n") + "\n")

  private def translated(entry: Entry): Boolean =
    entry.term.present || entry.definition.lay(false): (text: Text) =>
      !Translation.parse(text).passages.filter(!_.foreign.nil).nil

  // The entries of a translation which someone has translated and the definitive glossary no
  // longer has: what making the translation again leaves out.
  def lost(definitive: Text, old: Text): List[Entry] =
    val kept: sci.Set[Text] = entries(definitive).stdlib.map(_.name).toSet
    entries(old).filter { (entry: Entry) => !kept.contains(entry.name) && translated(entry) }

  // The glossary a reader of a translation's language has, of the glossary `definitive` read
  // from the file it translates: each of its terms as the translation says it — its `term`,
  // its definition as its translation reads, its own keywords, and its `link` if it has one —
  // and as it is defined where the translation says nothing of it.
  def foreign(path: Text, text: Text, definitive: Glossary): Glossary =
    val theirs: sci.List[Entry] = entries(text).stdlib

    Glossary:
      definitive.terms.map: (term: Glossary.Term) =>
        theirs.find(_.name == term.name) match
          case None =>
            term

          case Some(entry) =>
            val name: Text = entry.term.lay(term.name): (name: Text) =>
              if Glossary.nameable(name) then name else term.name

            val definition: Text =
              entry.definition.let(Translation.parse(_).foreign.text).or(term.definition)

            Glossary.Term
              ( name, definition, entry.link.or(term.link), path, entry.line, entry.keywords,
                term.name )

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

      case _ =>
        val theirs: sci.List[Entry] = entries(text).stdlib
        val ours: sci.List[Entry] = entries(definitive).stdlib
        val remedy: Text = t"`fuel translate --update` makes the translation again"
        val all: sci.List[Text] = (ours.map(_.name) ++ names.stdlib).distinct

        def finding(tone: Tone, line: Int, message: Text): sci.List[Translation.Finding] =
          sci.List(Translation.Finding(tone, line, message))

        val absent: sci.List[Translation.Finding] = ours.flatMap: (entry: Entry) =>
          if theirs.exists(_.name == entry.name) then sci.Nil
          else finding(Tone.Warning, 1, t"This has no entry for `${entry.name}`: $remedy.")

        val entered: sci.List[Translation.Finding] =
          theirs.zipWithIndex.flatMap: (entry: Entry, index: Int) =>
            val earlier: sci.List[Entry] = theirs.take(index)

            ours.find(_.name == entry.name) match
              case None =>
                finding
                  ( Tone.Warning, entry.line,
                    t"The glossary this translates has no term `${entry.name}`: $remedy." )

              case Some(_) if earlier.exists(_.name == entry.name) =>
                finding(Tone.Failure, entry.line, t"`${entry.name}` has an entry already.")

              case Some(original) =>
                val prose: sci.List[Translation.Finding] =
                  (original.definition.option, entry.definition.option) match
                    case (Some(defined), Some(found)) =>
                      (Translation.findings(found, defined) + Mirror.findings(found)).stdlib
                      . map(_.shifted(entry.site - 1))

                    case (Some(_), None) =>
                      finding
                        ( Tone.Warning, entry.line,
                          t"This lacks the definition of `${entry.name}`, which is not " +
                            t"translated: $remedy." )

                    case (None, Some(_)) =>
                      finding
                        ( Tone.Warning, entry.site, t"The text this translates has gone: $remedy." )

                    case _ =>
                      sci.Nil

                val unnamed: sci.List[Translation.Finding] =
                  if entry.term.present then sci.Nil else
                    val message: Text =
                      t"`${entry.name}` is not translated: a `term` beneath it says what it " +
                        t"is in this language."

                    val tone: Tone = Tone.Warning
                    sci.List(Translation.Finding(tone, entry.line, message, untranslated = true))

                val named: sci.List[Translation.Finding] =
                  entry.term.option.toList.flatMap: (term: Text) =>
                    val slug: Text = Blocks.slug(term)

                    val shared: Option[Entry] =
                      earlier.find(_.term.lay(false)(Blocks.slug(_) == slug))

                    val friend: Option[Text] =
                      all.find { (name: Text) => name != entry.name && Blocks.slug(name) == slug }

                    if !Glossary.nameable(term) then
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$term` cannot be a glossary term: a term is written in brackets, " +
                            t"so it cannot hold one." )
                    else if shared.isDefined then
                      finding
                        ( Tone.Failure, entry.line,
                          t"`$term` is the term of `${shared.get.name}` already, so `[$term]` " +
                            t"would not say which is meant." )
                    else friend.toList.flatMap: (name: Text) =>
                      finding
                        ( Tone.Warning, entry.line,
                          t"`$term` is also the name of another term, `$name`, as the glossary " +
                            t"defines it: in this language `[$term]` is `${entry.name}`." )

                val linked: sci.List[Translation.Finding] =
                  entry.link.option.toList.flatMap: (link: Text) =>
                    val local: Boolean = original.link.lay(false)(_.starts(t"/"))

                    if !local || original.link.lay(false)(_ == link) then sci.Nil
                    else
                      finding
                        ( Tone.Warning, entry.line,
                          t"`${entry.name}` is explained by a page of the site, " +
                            t"`${original.link.or(t"")}`, which is the same page in every " +
                            t"language: this `link` is in place of it." )

                unnamed ++ prose ++ named ++ linked

        val diagnostics: List[Block.Diagnostic] =
          Translation.report(path, text, List.from(absent ++ entered))

        diagnostics.map { (diagnostic: Block.Diagnostic) => diagnostic: Block }

  // How much of a glossary's translation is done: the sum over its definitions, with each
  // term nobody has given a `term` one more to translate, and a term the translation lacks
  // one more change to make.
  def progress(text: Text, definitive: Text): Translation.Progress =
    val theirs: sci.List[Entry] = entries(text).stdlib

    entries(definitive).stdlib.foldLeft(Translation.Progress(0, 0, 0, 0, 0)):
      (total: Translation.Progress, entry: Entry) =>
        theirs.find(_.name == entry.name) match
          case None =>
            total.copy(stale = total.stale + 1)

          case Some(found) =>
            val prose: Translation.Progress =
              (entry.definition.option, found.definition.option) match
                case (Some(defined), Some(text)) =>
                  Translation.progress(Translation.parse(text), defined)

                case (Some(_), None) => Translation.Progress(0, 0, 0, 0, 1)
                case _               => Translation.Progress(0, 0, 0, 0, 0)

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
