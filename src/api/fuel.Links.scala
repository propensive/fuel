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

import dysasymptotics.linearSize
import pyrocosm.{Block, Tone}

// Every link of every document, checked once every document is converted, when every target
// is known: another document and a place on it, a page of the site (an entity's, with a member
// on it; a glossary term's), a sigilled reference nothing resolved, and — only when asked,
// since it means the network — a URL, which must answer. Each problem is a diagnostic at the
// line the link is on.
object Links:
  // The code of a link's problem, `↯/f.26.3.<variant>`: a place another document lacks (1); a
  // document a section lacks (2); a place this document lacks (3); a member an API's page lacks
  // (4); an API's page, a glossary's term or a page which does not exist (5, 6, 7); a sigilled
  // reference which names nothing (8); and a URL which did not answer in time (9), which is not
  // one (10), which could not be fetched (11), or which answered with a failure (12). A URL's
  // own error carries its code.
  private def bolt(variant: Int): fulminate.Bolt = FuelBolts(FuelBolts.api, FuelBolts.link, variant)

  def check(corpus: Corpus, api: Api, glossary: Glossary, external: Boolean = false): List[Block] =
    check(corpus, Apis.of(api), glossary, external)

  def check(corpus: Corpus, apis: Apis, glossary: Glossary, external: Boolean)
  :   List[Block] =

    corpus.sections.bind: (section: Corpus.Section) =>
      section.docs.bind: (doc: Doc) =>
        doc.references.bind: (reference: Linkage) =>
          problem(corpus, apis, glossary, external, doc, reference).let(List(_)).or(Nil)

  private def problem
    ( corpus:    Corpus,
      apis:      Apis,
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

    def report(tone: Tone, message: Text, code: fulminate.Bolt): Block =
      val style: Block.Note.Style =
        if tone == Tone.Failure then Block.Note.Style.Erroneous else Block.Note.Style.Caution

      val excerpt: List[Block] =
        if doc.text == t"" then Nil else Excerpts.plainAt(doc.text, reference.line, written, style)

      val location: Block.Location = Block.Location(doc.path, reference.line)
      Block.Diagnostic(tone, location, List(Block.paragraph(message)), excerpt, code = code)

    def failure(message: Text, code: fulminate.Bolt): Block = report(Tone.Failure, message, code)

    // A document, and a place on it when the link names one.
    def document(section: Text, path: Text, fragment: Optional[Text]): Optional[Block] =
      corpus.doc(section, path) match
        case target: Doc =>
          fragment.let: (fragment: Text) =>
            if target.anchors.has(fragment) then Unset
            else failure(t"There is no `#$fragment` on `$path`.", bolt(1))

        case _ =>
          failure(t"There is no document `$path` in the section `$section`.", bolt(2))

    reference.target match
      case Linkage.Target.Document(section, path, fragment) =>
        document(section, path, fragment)

      case Linkage.Target.Anchor(fragment) =>
        if doc.anchors.has(fragment) then Unset
        else failure(t"There is no `#$fragment` on this page.", bolt(3))

      case Linkage.Target.Site(path, fragment) =>
        if path.starts(t"/doc/") then
          val rest: List[Text] = path.skip(5).cut(t"/").map(_.urlDecode)
          document(rest.head.or(t""), rest.skip(1).join(t"/"), fragment)
        else if path.starts(t"/api/") then
          apis.locate(path.skip(5)) match
            case entity: Api.Entity =>
              fragment.let: (fragment: Text) =>
                if apis.of(entity).children(entity).exists(_.name == fragment) then Unset
                else failure(t"`${entity.address}` has no member `$fragment`.", bolt(4))

            case _ =>
              failure(t"There is no API page at `$path`.", bolt(5))
        else if path == t"/glossary" then
          fragment.let: (fragment: Text) =>
            if glossary(fragment).present then Unset
            else failure(t"The glossary has no term `$fragment`.", bolt(6))
        else
          failure(t"There is no page at `$path`.", bolt(7))

      case Linkage.Target.Url(url) =>
        if !external then Unset else fetch(url).let(failure(_, _))

      // Without an API there is nothing a sigilled reference could resolve against, so it is
      // a caution: the page is shown, and a compile with the API will say.
      case Linkage.Target.Sigil(reference) =>
        val what: Text = if reference.starts(t"#") then t"type" else t"term"
        val message: Text = t"`$reference` names no $what in scope."
        if apis.isEmpty then report(Tone.Warning, message, bolt(8))
        else failure(message, bolt(8))

  // A URL fetched: what is wrong with it, with its code, or nothing. Only HTTP is fetched; a
  // `mailto:` or another scheme is taken as it is. Telekinesis sets no limit on how long a server
  // may take to answer, so the fetch runs as a task which is awaited for twenty seconds at most,
  // and cancelled if it has not answered by then.
  private def fetch(url: Text): Optional[(Text, fulminate.Bolt)] =
    if !url.starts(t"http://") && !url.starts(t"https://") then Unset else
      import probates.cancelProbate
      import threads.virtualThreads

      val late: (Text, fulminate.Bolt) = (t"`$url` did not answer within 20 seconds.", bolt(9))

      // An `Async.Error` the supervision raises itself is taken, as the await's is, as lateness.
      recover:
        case error: Async.Error => late

      . protect:
        supervise:
          val task = async(answer(url))

          recover:
            case error: Async.Error =>
              task.cancel()
              late

          . protect(task.await(20.0*Second))

  // What is wrong with an HTTP URL, as its answer says, or nothing. Redirects are followed, as
  // far as telekinesis follows them.
  private def answer(url: Text): Optional[(Text, fulminate.Bolt)] =
    import httpBackends.javaNetHttp
    import internetAccess.online
    import logging.silentLogging

    recover:
      case error: Url.Error =>
        (t"`$url` is not a valid URL.", bolt(10).carrying(error.bolt))

      case error: Connect.Error =>
        (t"`$url` could not be fetched: ${error.message}", bolt(11).carrying(error.bolt))

    . protect:
      val code: Int = url.as[HttpUrl].fetch().status.code
      if code < 400 then Unset else (t"`$url` answered ${code.show}.", bolt(12))
