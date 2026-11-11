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

import soundness.{Format as _, Language as _, Token as _, *}

import dysasymptotics.linearSize
import pyrocosm.{Block, Figure, Inline, Language, Token, Tone}

// An entity's page, and the navigation of the whole API, as blocks: Amok's `JvmFolio` layout.
// The page is its place (each parent a link), its name, its one-line info, then for each of
// its declarations the signature and a table of the members which belong to it (kind, name
// linked and anchored by its id, signature, info; the extensions it declares in a table per
// clause they extend) — the parameters, in a table of their own beneath the signature they
// belong to — then, for a type, the extensions which apply to it (a table per source, in the
// order `Applicables` ranks them, each headed by what brings it: the companion, an import, a
// given) and the givens of it and for it; then its Markdown detail, what it refers to, and —
// for an entity nothing declares any more — the notice of its removal and what replaces it.
object ApiPages:
  // The most hits a search page draws. A broad query (`def`, say, or a package of the standard
  // library) matches tens of thousands of declarations, and is typed on the way to a narrower
  // one: drawn whole, its table runs to tens of megabytes of HTML, which takes seconds to
  // render and to send on every keystroke, so a page shows the first few hits and the count.
  val shown: Int = 100

  // Tokens as code in the language of an API's format.
  def code(api: Api, tokens: List[Token]): Inline.Snippet =
    Inline.Snippet(api.format.language, tokens)

  // A signature's words as tokens, a space between each and the next: each in the accent of
  // what it is, and linked to the page of the entity it names, where there is one.
  def phrases(api: Api, phrases: List[Api.Phrase]): List[Token] =
    val tokens: List[Token] = phrases.map: (phrase: Api.Phrase) =>
      val accent: Token.Accent = phrase.role match
        case Api.Phrase.Role.Keyword  => Token.Accent.Keyword
        case Api.Phrase.Role.Modifier => Token.Accent.Modifier
        case Api.Phrase.Role.Type     => Token.Accent.Typal
        case Api.Phrase.Role.Term     => Token.Accent.Term
        case Api.Phrase.Role.Literal  => Token.Accent.String
        case Api.Phrase.Role.Symbol   => Token.Accent.Symbol
        case Api.Phrase.Role.Plain    => Token.Accent.Unparsed

      val link: Optional[Inline.Destination] =
        phrase.target.let(api(_)).let: (e: Api.Entity) => Inline.Destination.Local(e.url)

      Token(phrase.text, accent, Unset, link)

    tokens.flatMap(List(Token.plain(t" "), _)).skip(1)

  // What a format is given to make an entity's sections from: the entity and its API, how
  // references in it resolve, and its members — those which are parameters, listed apart, and
  // the others.
  case class Context(api: Api, entity: Api.Entity, resolver: Resolver, scope: Scope):
    val children: List[Api.Entity] = api.children(entity).filter { c => !c.hidden && !c.derived }
    val parameters: List[Api.Entity] = children.filter(_.isParameter)
    val others: List[Api.Entity] = children.filter(!_.isParameter)

    def row(member: Api.Entity, d: Api.Declaration, anchored: Boolean): Block.Row =
      ApiPages.row(member, d, anchored, api, scope, resolver)

    def rows(members: List[Api.Entity]): List[Block.Row] = members.bind: (member: Api.Entity) =>
      ApiPages.rows(member, true, api, scope, resolver)

    // The members of the entity's type form, or of its term form.
    def members(ofType: Boolean): List[Api.Entity] = others.filter: (child: Api.Entity) =>
      child.parent.let(_.isType).or(false) == ofType

    // A link to an entity by its name, where it has a page.
    def link(key: Text): List[Inline] = api(key).lay(Inline.text(key)): (e: Api.Entity) =>
      List(Inline.Link(Inline.Destination.Local(e.url), Inline.text(e.name)))

  // One declaration's section: its signature, what its format draws `beneath` it, a table of
  // its parameters — which belong to the signature of the type, where there is one, since a
  // class's are its constructor's; else to the term's — a table of the members `listed`, and
  // what its format puts `after` them.
  def section
    ( page:        Context,
      declaration: Api.Declaration,
      ofType:      Boolean,
      listed:      List[Api.Entity],
      beneath:     List[Block] = Nil,
      after:       List[Block] = Nil )
  :   List[Block] =

    val format: Format = page.api.format
    val signature: Block = format.signature(page.entity.name, declaration, page.api, page.scope)

    val own: List[Block.Row] =
      if ofType || page.entity.template.absent then page.rows(page.parameters) else Nil

    val parameterTable: List[Block] = table(t"Parameter", t"Type", own)
    val memberTable: List[Block] = table(t"Name", t"Signature", page.rows(listed))
    signature :: beneath + parameterTable + memberTable + after

  // An entity's sections as any format has them: one for its term and one for its type, each
  // with its members, and the signature of each further declaration of the name.
  def sections(page: Context): List[Block] =
    val entity: Api.Entity = page.entity
    val format: Format = page.api.format

    val definition: List[Block] =
      entity.definition.let(section(page, _, false, page.members(false))).or(Nil)

    val template: List[Block] =
      entity.template.let(section(page, _, true, page.members(true))).or(Nil)

    val overloads: List[Block] = entity.overloads.map: (d: Api.Declaration) =>
      format.signature(entity.name, d, page.api, page.scope)

    definition + template + overloads

  // A declaration's row in a table of members; a member's own rows are anchored by its name,
  // another page's are not (its anchors are its own). A `qualified` row, in a list drawn from
  // across the API, says where the member is before its name.
  def row
    ( member:    Api.Entity,
      d:         Api.Declaration,
      anchored:  Boolean,
      api:       Api,
      scope:     Scope,
      resolver:  Resolver,
      qualified: Boolean         = false )
  :   Block.Row =

    val exported: Boolean = d.exported || member.alias.present

    // The row of a name which is both a type and a term shows both glyphs; an overload's, its own.
    val principal: Boolean = member.template.or(member.definition).let(_ == d).or(false)

    val glyphs: List[Inline] =
      if principal then api.format.glyphs(member, exported)
      else List(api.format.glyph(d, exported))

    val accent: Token.Accent = if d.kind.isType then Token.Accent.Typal else Token.Accent.Term

    val where: List[Inline] =
      if !qualified then Nil
      else
        val end: Int = member.key.s.length - member.name.s.length
        List(Inline.Toned(Tone.Muted, Inline.text(Text(member.key.s.substring(0, end).nn))))

    val token: Token = Token(member.name, accent, Unset, Inline.Destination.Local(member.url))
    val name: List[Inline] = where + List(Inline.Strong(List(code(api, List(token)))))

    // An export's signature is what it exports, linked; any other's its clauses and result.
    val signature: List[Inline] =
      member.alias.lay(List(api.format.cell(d, api, scope))):
        (target: stenography.Designator) =>
          List(code(api, Signatures.designator(target, api, scope)))

    val info: List[Inline] = member.doc.let(_.info).let(Blocks.inline(_, resolver)).or(Nil)
    val tone: Optional[Tone] = if member.removed then Tone.Muted else Unset

    // The description is the row's note, which a table of fixed columns sets beneath the last of
    // them, so that the names stand clear and the detail is all to their right.
    val cells: List[Block.Cell] = List(Block.Cell(glyphs), Block.Cell(name), Block.Cell(signature))
    Block.Row(cells, tone, Unset, if anchored then member.name else Unset, info)

  // A member's rows: one for the member, whether it is a term, a type or both (a type and its
  // companion, an export of both), from its type where it has one, and one for each overload
  // of a method — a companion object, which the type's row covers, is not one; and an export
  // is one row however many forwarders its clause made.
  def rows(member: Api.Entity, anchored: Boolean, api: Api, scope: Scope, resolver: Resolver)
  :   List[Block.Row] =

    val principal: Optional[Api.Declaration] = member.template.or(member.definition)

    val overloads: List[Api.Declaration] =
      if member.exported then Nil
      else if member.template.absent then member.overloads
      else
        member.overloads.filter: d =>
          d.kind == Api.Kind.Def || d.kind == Api.Kind.Given || d.kind == Api.Kind.Extension

    def of(d: Api.Declaration): Block.Row = row(member, d, anchored, api, scope, resolver)
    principal.let(List(_)).or(Nil).map(of) + overloads.map(of)

  // The columns every member table shares, each a fixed share of the width, so that the tables
  // of a page line up.
  def table(heading: Text, signature: Text, rows: List[Block.Row]): List[Block] =
    val columns: List[Block.Column] =
      List
        ( Block.Column
            ( Inline.text(t""),
              alignment = Block.Alignment.End,
              sizing    = Block.Sizing.Fixed(10) ),
          Block.Column(Inline.text(heading), sizing = Block.Sizing.Fixed(26)),
          Block.Column(Inline.text(signature), sizing = Block.Sizing.Fixed(64)) )

    if rows.nil then Nil else List(Block.Table(columns, rows, Unset))

  // The results of a search, as a page: what was asked, what could be said of it, and a table
  // of the declarations found, each a row like a member's, said where it is.
  def search(api: Api, text: Text, outcome: Search.Outcome, resolver: Resolver, scope: Scope)
  :   List[Block] =

    search(Apis.of(api), text, outcome, resolver, scope)

  // The same, of a search of every format's API: each hit's row is its own format's.
  def search(apis: Apis, text: Text, outcome: Search.Outcome, resolver: Resolver, scope: Scope)
  :   List[Block] =

    val count: Int = outcome.hits.size
    val drawn: List[Search.Hit] = List.from(outcome.hits.stdlib.take(shown))

    val summary: Text =
      if count == 1 then t"1 result for " else t"${count.toString.tt} results for "

    val truncation: List[Inline] =
      if count <= shown then Nil
      else Inline.text(t"; the first ${shown.toString.tt} are shown. Narrow the query to see others.")

    val heading: Block = Block.Heading(1, Inline.text(t"Search"))
    val snippet: Inline = Inline.Snippet(Language.Scala, List(Token(text, Token.Accent.Unparsed)))
    val asked: Block = Block.Paragraph(Inline.text(summary) + List(snippet) + truncation)

    val notices: List[Block] = outcome.notices.map: (notice: Text) =>
      Block.Notice(Tone.Warning, Unset, List(Block.paragraph(notice)))

    val rows: List[Block.Row] = drawn.map: (hit: Search.Hit) =>
      val api: Api = apis.of(hit.entity)
      row(hit.entity, hit.declaration, false, api, scope, resolver, qualified = true)

    // The glossary's terms the query names, before the declarations: each a link to its entry,
    // with its definition.
    val terms: List[Block] =
      if outcome.terms.nil then Nil
      else
        val items: List[Block.Item] = outcome.terms.map: (term: Glossary.Term) =>
          val destination: Inline.Destination = Inline.Destination.Local(t"/glossary", term.slug)
          val link: Inline = Inline.Link(destination, Inline.text(term.name))
          val definition: List[Inline] = Blocks.inline(term.definition, resolver)
          val content: List[Inline] = link :: Inline.text(t": ") + definition
          Block.Item(List(Block.Paragraph(content)))

        List(Block.Heading(2, Inline.text(t"Glossary")), Block.Listing(false, items))

    List(heading, asked) + notices + terms + table(t"Name", t"Signature", rows)

  // An entity's page with its open issues: their numbers after the title, each a link to the
  // issue, and a list of them with their titles at the foot. They are put on at show time, not
  // rendered into the page, which the output directory keeps by a key which knows nothing of
  // issues, and which would be stale as soon as an issue closed.
  def withIssues(page: List[Block], issues: List[Issue]): List[Block] =
    if issues.nil then page else
      def link(issue: Issue, text: Text): Inline =
        Inline.Link(Inline.Destination.External(issue.url), Inline.text(text))

      val numbers: List[Inline] = issues.bind: (issue: Issue) =>
        List(Inline.Textual(t" "), link(issue, t"#${issue.number.show}"))

      val top: Block = Block.Paragraph(Inline.text(t"Open issues:") + numbers)

      val items: List[Block.Item] = issues.map: (issue: Issue) =>
        val content: List[Inline] = List(link(issue, t"#${issue.number.show}")) + Inline.text(t" ${issue.title}")
        Block.Item(List(Block.Paragraph(content)), Unset, Inline.Destination.External(issue.url))

      val foot: List[Block] = List(Block.Heading(3, Inline.text(t"Issues")), Block.Listing(false, items))

      // After the title, so that the place above it stays the first block.
      val (before, from) = page.stdlib.span { case Block.Heading(1, _, _) => false; case _ => true }
      List.from(before ++ from.take(1)) + List(top) + List.from(from.drop(1)) + foot

  def page
    ( api: Api, entity: Api.Entity, resolver: Resolver, scope: Scope, embeddings: List[Embedding] )
  :   List[Block] =

    val doc: Optional[Api.Entry] = entity.doc

    // Where the entity is, in code, as a qualified name is: each segment linked, and the name up
    // to it one thing, so that pointing at a segment underlines the path before it.
    val place: List[Block] =
      def crumbs(designator: Optional[Locator]): List[Token] =
        designator.lay(Nil: List[Token]): (d: Locator) =>
          val destination: Optional[Inline.Destination] =
            api(Api.key(d)).lay(Unset): (e: Api.Entity) => Inline.Destination.Local(e.url)

          val separator: Text = d.separator

          val parent: List[Token] = crumbs(d.parent)
          val chained: Boolean = !parent.nil

          val segment: Token =
            Token.plain(d.name).copy(link = destination, chained = chained)

          parent + List(segment, Token(separator, Token.Accent.Symbol).copy(chained = true))

      val above: List[Token] = crumbs(entity.parent)
      if above.nil then Nil else List(Block.Paragraph(List(code(api, above))))

    // The heading carries the page's bookmark button and, for a package, the toggle which
    // imports it into the reader's purview; both are the frontend's to answer.
    val marks: List[Inline] =
      val bookmark: Inline = Inline.Bookmark(entity.url, entity.key)

      if entity.isPackage then List(Inline.Import(entity.url, entity.key), bookmark)
      else List(bookmark)

    // The glyphs of what the name declares, a type's and a term's where it is both, before it.
    val glyph: List[Inline] = api.format.glyphs(entity, entity.exported)

    // The name as code, in the monospace face, which a heading sets at its own size and weight.
    val space: List[Inline] = List(Inline.Textual(t" "))
    val accent: Token.Accent = if entity.template.present then Token.Accent.Typal else Token.Accent.Term
    val name: Inline = code(api, List(Token(entity.name, accent)))
    val title: List[Inline] = glyph + space + List(name) + space + marks
    val heading: Block = Block.Heading(1, title)

    val info: List[Block] = doc.let(_.info).lay(Nil: List[Block]): (text: Text) =>
      List(Block.Paragraph(Blocks.inline(text, resolver)))

    val removed: List[Block] = doc.let(_.until).lay(Nil: List[Block]): (until: Api.Until) =>
      val replacement: List[Inline] = until.replacement.lay(Nil: List[Inline]): (name: Text) =>
        val named: Inline = code(api, List(Token(name, Token.Accent.Term)))

        val link: Inline = api.find(Locator(name)).lay(named): (e: Api.Entity) =>
          Inline.Link(Inline.Destination.Local(e.url), Inline.text(name))

        List(Inline.Textual(t"; use "), link, Inline.Textual(t" instead"))

      val verb: Text = if entity.removed then t"was removed in" else t"is removed in"
      val notice: Inline = Inline.Textual(t"${entity.name} $verb ${until.version}")
      val stop: Inline = Inline.Textual(t".")
      val paragraph: Block = Block.Paragraph(List(notice) + replacement + List(stop))
      List(Block.Notice(Tone.Warning, Unset, List(paragraph)))

    // What the entity declares, and what applies to it beyond that, are its format's to say.
    val context: Context = Context(api, entity, resolver, scope)
    val sections: List[Block] = api.format.sections(context)
    val applicable: List[Block] = api.format.applicable(context)

    val detail: List[Block] = doc.let(_.detail).lay(Nil: List[Block]): (text: Text) =>
      Blocks.of(Parser.parse(text), embeddings, resolver)

    val refer: List[Block] = doc.let(_.refer).lay(Nil: List[Block]): (names: List[Text]) =>
      if names.nil then Nil else
        val items: List[Block.Item] = names.map: (name: Text) =>
          val plain: Block.Item = Block.Item(List(Block.paragraph(name)))

          api.find(Locator(name)).lay(plain): (e: Api.Entity) =>
            val target: Inline.Destination = Inline.Destination.Local(e.url)
            Block.Item(List(Block.Paragraph(Inline.text(name))), Unset, target)

        List(Block.Heading(3, Inline.text(t"See also")), Block.Listing(false, items))

    place + List(heading) + info + removed + sections + applicable + detail + refer

  // The navigation: the project's own packages, then its dependencies' (closed), each node a
  // link to its page, open along the way to `current`, with the packages, types and objects
  // beneath it (not every method and value, which its page lists).
  def tree(api: Api, current: Optional[Text]): Block.Tree =
    val path: List[Text] = current.lay(Nil: List[Text]): (key: Text) =>
      def up(designator: Optional[Locator]): List[Text] =
        designator.lay(Nil: List[Text]): (d: Locator) => List(Api.key(d)) + up(d.parent)

      api(key).lay(Nil: List[Text]): (e: Api.Entity) => List(e.key) + up(e.parent)

    // The project's own tree is shown whole; a dependency's unfolds only along the way to the
    // current page, since the dependencies together are tens of thousands of entities.
    def branch(c: Api.Entity): Boolean = !c.hidden && api.format.branch(c)

    def node(entity: Api.Entity, whole: Boolean): Block.TreeNode =
      val children: List[Block.TreeNode] =
        if !whole && !path.has(entity.key) then Nil
        else api.children(entity).filter(branch).map(node(_, whole))

      val open: Optional[Boolean] = if children.nil then Unset else path.has(entity.key)

      val tone: Optional[Tone] =
        if entity.key == current then Tone.Accent else if entity.removed then Tone.Muted else Unset

      val destination: Inline.Destination = Inline.Destination.Local(entity.url)

      // The name is code, a type's in the type accent and a term's in the term's.
      val accent: Token.Accent =
        if entity.template.present then Token.Accent.Typal else Token.Accent.Term

      val label: List[Inline] = List(code(api, List(Token(entity.name, accent))))
      Block.TreeNode(label, children, tone, Unset, open, destination)

    val roots: List[Api.Entity] = api.roots
    val (own, dependencies) = roots.partition(ownBeneath(api, _))
    val ownNodes: List[Block.TreeNode] = own.map(node(_, true))

    val dependencyNodes: List[Block.TreeNode] =
      if dependencies.nil then Nil
      else
        val open: Boolean = dependencies.exists: (d: Api.Entity) => path.has(d.key)
        val nodes: List[Block.TreeNode] = dependencies.map(node(_, false))
        List(Block.TreeNode(Inline.text(t"Dependencies"), nodes, Tone.Muted, Unset, open))

    Block.Tree(ownNodes + dependencyNodes)

  // Whether a root package has any of the project's own entities beneath it.
  private def ownBeneath(api: Api, root: Api.Entity): Boolean =
    api.entities.values.exists: e =>
      e.own && (e.key == root.key || e.key.starts(t"${root.key}.") || e.key.starts(t"${root.key}#"))
