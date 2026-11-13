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

// Fuel's own words, in each language its interface is written in: what it says around the
// documents and the API's pages, as distinct from what they say, which their translations do
// (`Translation`). Each is read under the reader's `Locale`, as `Phrases.search()`. What is
// said of a failure — a diagnostic, a query which is not one — is in English alone.
//
// No `Locale` is in scope here, and none should be: `|` on a `Polyglot` picks one language at
// once where it finds one.
object Phrases:
  // The languages Fuel's interface is written in, which are Pyrocosm's.
  type Spoken = en & fr & de
  type Phrase = Polyglot[Text, Spoken]

  val english: Locale[Spoken] = Locale(en)

  // The locale Fuel's words are read in, for a language the documents are in: its own, where
  // Fuel has words for it, as `fr` for `fr-CA`; English otherwise.
  def locale(tongue: Optional[Text]): Locale[Spoken] =
    tongue.let(_.cut(t"-").stdlib.head.lower).or(t"en") match
      case t"fr" => Locale(fr)
      case t"de" => Locale(de)
      case _     => Locale(en)

  // The order of a language's dictionary, which its glossary is in.
  def collation(locale: Locale[Spoken]): Collation = locale.language.code match
    case t"fr" => summon[fr is Collatable].collation
    case t"de" => summon[de is Collatable].collation
    case _     => summon[en is Collatable].collation

  // The glossary page: where a term is explained in full, as `See /doc/guide.md.`
  val see: Phrase = en(t"See") | fr(t"Voir") | de(t"Siehe")

  // The dashboard.
  val documents: Phrase = en(t"Documents") | fr(t"Documents") | de(t"Dokumente")
  val glossary: Phrase = en(t"Glossary") | fr(t"Glossaire") | de(t"Glossar")
  val search: Phrase = en(t"Search") | fr(t"Recherche") | de(t"Suche")
  val query: Phrase = en(t"Query") | fr(t"Requête") | de(t"Abfrage")
  val notFound: Phrase = en(t"Not found") | fr(t"Introuvable") | de(t"Nicht gefunden")

  val searchHint: Phrase =
    en(t"Search the API: name, def, #Type, /pkg, +Result, -Param") |
      fr(t"Chercher dans l'API : nom, def, #Type, /pkg, +Résultat, -Param") |
      de(t"API durchsuchen: Name, def, #Typ, /pkg, +Ergebnis, -Param")

  val uncompiled: Phrase =
    en(t"Run `fuel compile` in a project, and its documents appear here.") |
      fr(t"Lancez `fuel compile` dans un projet, et ses documents apparaissent ici.") |
      de(t"Führen Sie `fuel compile` in einem Projekt aus, und seine Dokumente erscheinen hier.")

  val noDocuments: Phrase =
    en(t"No documents yet: run fuel compile in a project.") |
      fr(t"Pas encore de documents : lancez fuel compile dans un projet.") |
      de(t"Noch keine Dokumente: Führen Sie fuel compile in einem Projekt aus.")

  def noPage(path: Text): Phrase =
    en(t"There is no page at $path.") | fr(t"Il n'y a pas de page à $path.") |
      de(t"Unter $path gibt es keine Seite.")

  // Said above a document which is read in a language it has no translation into.
  val untranslated: Phrase =
    en(t"This page has not been translated yet: it is shown as it was written.") |
      fr
        ( t"Cette page n'a pas encore été traduite : elle est affichée telle qu'elle a été " +
          t"écrite." ) |
      de
        ( t"Diese Seite wurde noch nicht übersetzt: Sie wird so gezeigt, wie sie geschrieben " +
          t"wurde." )

  // The formats' names, as the navigation heads them.
  val api: Phrase = en(t"API") | fr(t"API") | de(t"API")
  val schemas: Phrase = en(t"Schemas") | fr(t"Schémas") | de(t"Schemata")

  // An entity's page.
  val parameter: Phrase = en(t"Parameter") | fr(t"Paramètre") | de(t"Parameter")
  val kind: Phrase = en(t"Type") | fr(t"Type") | de(t"Typ")
  val name: Phrase = en(t"Name") | fr(t"Nom") | de(t"Name")
  val signature: Phrase = en(t"Signature") | fr(t"Signature") | de(t"Signatur")
  val seeAlso: Phrase = en(t"See also") | fr(t"Voir aussi") | de(t"Siehe auch")
  val dependencies: Phrase = en(t"Dependencies") | fr(t"Dépendances") | de(t"Abhängigkeiten")
  val openIssues: Phrase = en(t"Open issues:") | fr(t"Tickets ouverts :") | de(t"Offene Issues:")
  val issues: Phrase = en(t"Issues") | fr(t"Tickets") | de(t"Issues")

  // That an entity is, or was, removed in a version: what is said before the name of what
  // replaces it, where something does, and after it.
  def removed(name: Text, version: Text, gone: Boolean): Phrase =
    if gone then
      en(t"$name was removed in $version") | fr(t"$name a été supprimé dans $version") |
        de(t"$name wurde in $version entfernt")
    else
      en(t"$name is removed in $version") | fr(t"$name est supprimé dans $version") |
        de(t"$name wird in $version entfernt")

  val useBefore: Phrase = en(t"; use ") | fr(t" ; utilisez ") | de(t"; verwenden Sie stattdessen ")
  val useAfter: Phrase = en(t" instead") | fr(t" à la place") | de(t"")

  // A search's results: what is said before the query, and after it when not all are shown.
  def resultsFor(count: Int): Phrase =
    if count == 1 then en(t"1 result for ") | fr(t"1 résultat pour ") | de(t"1 Ergebnis für ")
    else
      en(t"${count.toString.tt} results for ") | fr(t"${count.toString.tt} résultats pour ") |
        de(t"${count.toString.tt} Ergebnisse für ")

  def firstShown(shown: Int): Phrase =
    en(t"; the first ${shown.toString.tt} are shown. Narrow the query to see others.") |
      fr
        ( t" ; les ${shown.toString.tt} premiers sont affichés. Précisez la requête pour voir " +
          t"les autres." ) |
      de
        ( t"; die ersten ${shown.toString.tt} werden gezeigt. Grenzen Sie die Abfrage ein, um " +
          t"weitere zu sehen." )

  // What applies to a Scala type beyond its members.
  val extensions: Phrase = en(t"Extensions") | fr(t"Extensions") | de(t"Erweiterungen")
  val givens: Phrase = en(t"Givens") | fr(t"Givens") | de(t"Givens")
  val instances: Phrase = en(t"Instances:") | fr(t"Instances :") | de(t"Instanzen:")

  def givensFor(name: Text): Phrase =
    en(t"For $name:") | fr(t"Pour $name :") | de(t"Für $name:")

  val inCompanion: Phrase =
    en(t"In the companion object") | fr(t"Dans l'objet compagnon") | de(t"Im Begleitobjekt")

  val inCompanionOf: Phrase =
    en(t"In the companion object of ") | fr(t"Dans l'objet compagnon de ") |
      de(t"Im Begleitobjekt von ")

  val by: Phrase = en(t"By ") | fr(t"Par ") | de(t"Durch ")
  val within: Phrase = en(t", within ") | fr(t", dans ") | de(t", innerhalb von ")
  val withGiven: Phrase = en(t", with ") | fr(t", avec ") | de(t", mit ")

  // Where the extensions of every value are: what is said before the link, and after it.
  val everyValueBefore: Phrase =
    en(t"The extensions of every value are listed on ") |
      fr(t"Les extensions de toute valeur sont listées sur ") |
      de(t"Die Erweiterungen jedes Werts sind aufgeführt unter ")

  val everyValueAfter: Phrase = en(t".") | fr(t".") | de(t".")
