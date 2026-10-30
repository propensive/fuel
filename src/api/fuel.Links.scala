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

import dysasymptotics.linearSize
import pyrocosm.{Block, Tone}

// Every link of every document, checked once every document is converted, when every target
// is known: another document and a place on it, a page of the site (an entity's, with a member
// on it; a glossary term's), a sigilled reference nothing resolved, and — only when asked,
// since it means the network — a URL, which must answer. Each problem is a diagnostic at the
// line the link is on.
object Links:
  def check(corpus: Corpus, api: Api, glossary: Glossary, external: Boolean = false): List[Block] =
    corpus.sections.bind: (section: Corpus.Section) =>
      section.docs.bind: (doc: Doc) =>
        doc.references.bind: (reference: Linkage) =>
          problem(corpus, api, glossary, external, doc, reference).let(List(_)).or(Nil)

  private def problem
    ( corpus:    Corpus,
      api:       Api,
      glossary:  Glossary,
      external:  Boolean,
      doc:       Doc,
      reference: Linkage )
  :   Optional[Block] =

    // The link as it was written, as far as the model keeps it, to underline on its line.
    val written: Text = reference.target match
      case Linkage.Target.Document(_, path, _) => path
      case Linkage.Target.Site(path, _)        => path
      case Linkage.Target.Anchor(fragment)     => t"#$fragment"
      case Linkage.Target.Url(url)             => url
      case Linkage.Target.Sigil(text)          => text

    def report(tone: Tone, message: Text): Block =
      val style: Block.Note.Style =
        if tone == Tone.Failure then Block.Note.Style.Erroneous else Block.Note.Style.Caution

      val excerpt: List[Block] =
        if doc.text == t"" then Nil else Excerpts.plainAt(doc.text, reference.line, written, style)

      val location: Block.Location = Block.Location(doc.path, reference.line)
      Block.Diagnostic(tone, location, List(Block.paragraph(message)), excerpt)

    def failure(message: Text): Block = report(Tone.Failure, message)

    def decode(text: Text): Text = Text(java.net.URLDecoder.decode(text.s, "UTF-8").nn)

    // A document, and a place on it when the link names one.
    def document(section: Text, path: Text, fragment: Optional[Text]): Optional[Block] =
      corpus.doc(section, path) match
        case target: Doc =>
          fragment.let: (fragment: Text) =>
            if target.anchors.has(fragment) then Unset
            else failure(t"There is no `#$fragment` on `$path`.")

        case _ =>
          failure(t"There is no document `$path` in the section `$section`.")

    reference.target match
      case Linkage.Target.Document(section, path, fragment) =>
        document(section, path, fragment)

      case Linkage.Target.Anchor(fragment) =>
        if doc.anchors.has(fragment) then Unset
        else failure(t"There is no `#$fragment` on this page.")

      case Linkage.Target.Site(path, fragment) =>
        if path.starts(t"/doc/") then
          val rest: List[Text] = path.skip(5).cut(t"/").map(decode)
          document(rest.head.or(t""), rest.skip(1).join(t"/"), fragment)
        else if path.starts(t"/api/") then
          api.locate(path.skip(5)) match
            case entity: Api.Entity =>
              fragment.let: (fragment: Text) =>
                if api.children(entity).exists(_.name == fragment) then Unset
                else failure(t"`${entity.key}` has no member `$fragment`.")

            case _ =>
              failure(t"There is no API page at `$path`.")
        else if path == t"/glossary" then
          fragment.let: (fragment: Text) =>
            if glossary(fragment).present then Unset
            else failure(t"The glossary has no term `$fragment`.")
        else
          failure(t"There is no page at `$path`.")

      case Linkage.Target.Url(url) =>
        if !external then Unset else fetch(url).let(failure(_))

      // Without an API there is nothing a sigilled reference could resolve against, so it is
      // a caution: the page is shown, and a compile with the API will say.
      case Linkage.Target.Sigil(reference) =>
        val what: Text = if reference.starts(t"#") then t"type" else t"term"
        val message: Text = t"`$reference` names no $what in scope."
        if api.entities.isEmpty then report(Tone.Warning, message) else failure(message)

  // A URL fetched, with a short patience: what is wrong with it, or nothing. Only HTTP is
  // fetched; a `mailto:` or another scheme is taken as it is.
  private def fetch(url: Text): Optional[Text] =
    if !url.starts(t"http://") && !url.starts(t"https://") then Unset else
      try
        val client: java.net.http.HttpClient =
          java.net.http.HttpClient.newBuilder.nn
          . connectTimeout(java.time.Duration.ofSeconds(10)).nn
          . followRedirects(java.net.http.HttpClient.Redirect.NORMAL).nn
          . build.nn

        val request: java.net.http.HttpRequest =
          java.net.http.HttpRequest.newBuilder(java.net.URI.create(url.s)).nn
          . timeout(java.time.Duration.ofSeconds(20)).nn
          . GET.nn
          . build.nn

        val status: Int =
          client.send(request, java.net.http.HttpResponse.BodyHandlers.discarding).nn.statusCode

        if status < 400 then Unset else t"`$url` answered ${status.show}."
      catch case error: Exception => t"`$url` could not be fetched: ${error.toString.tt}"
