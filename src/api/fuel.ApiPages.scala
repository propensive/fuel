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

import soundness.{Language as _, Token as _, *}

import dysasymptotics.linearSize
import pyrocosm.{Block, Figure, Inline, Language, Token, Tone}
import stenography.Designator

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
      if principal then Icons.all(member, exported) else List(Icons.of(d, exported))

    val accent: Token.Accent = if d.kind.isType then Token.Accent.Typal else Token.Accent.Term

    val where: List[Inline] =
      if !qualified then Nil
      else
        val end: Int = member.key.s.length - member.name.s.length
        List(Inline.Toned(Tone.Muted, Inline.text(Text(member.key.s.substring(0, end).nn))))

    val token: Token = Token(member.name, accent, Unset, Inline.Destination.Local(member.url))
    val name: List[Inline] = where + List(Inline.Strong(List(Signatures.code(List(token)))))

    // An export's signature is what it exports, linked; any other's its clauses and result.
    val signature: List[Inline] =
      member.alias.lay(List(Signatures.cell(d, api, scope))): (target: Designator) =>
        List(Signatures.code(Signatures.designator(target, api, scope)))

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
      row(hit.entity, hit.declaration, false, api, scope, resolver, qualified = true)

    List(heading, asked) + notices + table(t"Name", t"Signature", rows)

  def page
    ( api: Api, entity: Api.Entity, resolver: Resolver, scope: Scope, embeddings: List[Embedding] )
  :   List[Block] =

    val doc: Optional[Api.Entry] = entity.doc

    // Where the entity is, in code, as a qualified name is: each segment linked, and the name up
    // to it one thing, so that pointing at a segment underlines the path before it.
    val place: List[Block] =
      def crumbs(designator: Optional[Designator]): List[Token] =
        designator.lay(Nil: List[Token]): (d: Designator) =>
          val destination: Optional[Inline.Destination] =
            api(Api.key(d)).lay(Unset): (e: Api.Entity) => Inline.Destination.Local(e.url)

          val separator: Text = d match
            case Designator.Type(_, _) => t"#"
            case _                     => t"."

          val parent: List[Token] = crumbs(d.parent)
          val chained: Boolean = !parent.nil

          val segment: Token =
            Token.plain(d.name).copy(link = destination, chained = chained)

          parent + List(segment, Token(separator, Token.Accent.Symbol).copy(chained = true))

      val above: List[Token] = crumbs(entity.parent)
      if above.nil then Nil else List(Block.Paragraph(List(Signatures.code(above))))

    // The heading carries the page's bookmark button and, for a package, the toggle which
    // imports it into the reader's purview; both are the frontend's to answer.
    val marks: List[Inline] =
      val bookmark: Inline = Inline.Bookmark(entity.url, entity.key)

      if entity.isPackage then List(Inline.Import(entity.url, entity.key), bookmark)
      else List(bookmark)

    // The glyphs of what the name declares, a type's and a term's where it is both, before it.
    val glyph: List[Inline] = Icons.all(entity, entity.exported)

    // The name as code, in the monospace face, which a heading sets at its own size and weight.
    val space: List[Inline] = List(Inline.Textual(t" "))
    val accent: Token.Accent = if entity.template.present then Token.Accent.Typal else Token.Accent.Term
    val name: Inline = Inline.Snippet(Language.Scala, List(Token(entity.name, accent)))
    val title: List[Inline] = glyph + space + List(name) + space + marks
    val heading: Block = Block.Heading(1, title)

    val info: List[Block] = doc.let(_.info).lay(Nil: List[Block]): (text: Text) =>
      List(Block.Paragraph(Blocks.inline(text, resolver)))

    val removed: List[Block] = doc.let(_.until).lay(Nil: List[Block]): (until: Api.Until) =>
      val replacement: List[Inline] = until.replacement.lay(Nil: List[Inline]): (name: Text) =>
        val code: Inline = Inline.Snippet(Language.Scala, List(Token(name, Token.Accent.Term)))

        val link: Inline = api.find(Designator(name)).lay(code): (e: Api.Entity) =>
          Inline.Link(Inline.Destination.Local(e.url), Inline.text(name))

        List(Inline.Textual(t"; use "), link, Inline.Textual(t" instead"))

      val verb: Text = if entity.removed then t"was removed in" else t"is removed in"
      val notice: Inline = Inline.Textual(t"${entity.name} $verb ${until.version}")
      val stop: Inline = Inline.Textual(t".")
      val paragraph: Block = Block.Paragraph(List(notice) + replacement + List(stop))
      List(Block.Notice(Tone.Warning, Unset, List(paragraph)))

    val children: List[Api.Entity] = api.children(entity).filter { c => !c.hidden && !c.derived }
    val (parameters, others) = children.partition(_.isParameter)

    def row(member: Api.Entity, d: Api.Declaration, anchored: Boolean): Block.Row =
      ApiPages.row(member, d, anchored, api, scope, resolver)

    def rows(members: List[Api.Entity]): List[Block.Row] = members.bind: (member: Api.Entity) =>
      ApiPages.rows(member, true, api, scope, resolver)

    // The parameters belong to the signature of the type, where there is one, since a class's
    // are its constructor's; else to the term's.
    def section(declaration: Api.Declaration, ofType: Boolean): List[Block] =
      val signature: Block = Signatures.block(entity.name, declaration, api, scope)

      // A type's place in the hierarchy, drawn beneath its signature where there is one to draw.
      val none: List[Block] = Nil

      def figure(graph: Hierarchy.Graph, layout: Hierarchy.Layout): Block =
        Block.Figure(Figure(Inline.text(Hierarchy.alt(graph)), Hierarchy.svg(layout)))

      val hierarchy: List[Block] =
        if !ofType then none
        else
          Hierarchy.of(api, entity, scope).lay(none): (graph: Hierarchy.Graph) =>
            Hierarchy.layout(graph).lay(none): (layout: Hierarchy.Layout) =>
              List(figure(graph, layout))

      val members: List[Api.Entity] = others.filter: (child: Api.Entity) =>
        val onType: Boolean = child.parent.lay(false):
          case Designator.Type(_, _) => true
          case _                     => false

        onType == ofType

      // The extensions this entity declares are grouped under the clauses they extend, each
      // group a table of its own headed by its clause, after the other members, which have a
      // row each.
      val extended: List[(Api.Entity, Api.Declaration)] = members.bind: (m: Api.Entity) =>
        m.declarations.filter(_.kind == Api.Kind.Extension).map((m, _))

      val plain: List[Api.Entity] =
        members.filter(_.declarations.exists(_.kind != Api.Kind.Extension))

      val groups: List[Block] =
        def clauses(d: Api.Declaration): Text = d.group.map(_.qualified).join(t" ")
        val keys: List[Text] = extended.map: pair => clauses(pair(1))

        keys.distinct.bind: (key: Text) =>
          val these: List[(Api.Entity, Api.Declaration)] =
            extended.filter: pair => clauses(pair(1)) == key

          val first: Api.Declaration = these.stdlib.head(1)
          val clause: Block = Signatures.line(Signatures.group(first.group, api, scope))
          val entries: List[Block.Row] = these.map: (m, d) => row(m, d, true)
          clause :: table(t"Name", t"Signature", entries)

      val own: List[Block.Row] = if ofType || entity.template.absent then rows(parameters) else Nil
      val parameterTable: List[Block] = table(t"Parameter", t"Type", own)
      val memberTable: List[Block] = table(t"Name", t"Signature", rows(plain))
      signature :: hierarchy + parameterTable + memberTable + groups

    val definition: List[Block] = entity.definition.let(section(_, false)).or(Nil)
    val template: List[Block] = entity.template.let(section(_, true)).or(Nil)

    val overloads: List[Block] = entity.overloads.bind: (d: Api.Declaration) =>
      List(Signatures.block(entity.name, d, api, scope))

    // An export is one signature, of what it exports, whatever forwarders its clause made.
    val sections: List[Block] =
      if entity.exported then List(Signatures.exported(entity, api, scope))
      else definition + template + overloads

    // A link to an entity by its name, where it has a page.
    def link(key: Text): List[Inline] = api(key).lay(Inline.text(key)): (e: Api.Entity) =>
      List(Inline.Link(Inline.Destination.Local(e.url), Inline.text(e.name)))

    // The extensions which apply to the type, each run of one source one table, headed by what
    // brings it: declared in the companion (or a base's), or by an import, or within a given;
    // and the given which makes a type parameter's extension apply, where one does.
    val extensions: List[Block] =
      if entity.extensions.nil then Nil else
        def source(first: Api.Applicable): List[Block] =
          val from: Optional[Api.Entity] = first.importFrom.let(api(_))

          val where: List[Inline] = from match
            case from: Api.Entity =>
              val companion: Text = t"In the companion object of "

              if from.key == entity.key then Inline.text(t"In the companion object")
              else if entity.bases.has(from.key) then Inline.text(companion) + link(from.key)
              else
                val tokens: List[Token] =
                  List
                    ( Token(t"import", Token.Accent.Keyword),
                      Token.plain(t" "),
                      Token(from.key, Token.Accent.Term, Unset, Inline.Destination.Local(from.url)),
                      Token(t".*", Token.Accent.Symbol) )

                List(Inline.Textual(t"By "), Signatures.code(tokens))

            case _ => Nil

          val via: List[Inline] = first.via.let(api(_)).lay(Nil: List[Inline]): (g: Api.Entity) =>
            val within: Boolean = first.key.starts(t"${g.key}.")
            val lead: Text = if within then t", within " else t", with "

            val signature: List[Inline] =
              g.definition.lay(Nil: List[Inline]): (d: Api.Declaration) =>
                List(Signatures.code(Signatures.tokens(Syntax0.of(d), api, scope)))

            val local: Inline.Destination = Inline.Destination.Local(g.url)
            val name: Token = Token(g.name, Token.Accent.Term, Unset, local)
            val intro: List[Token] = List(Token(t"given", Token.Accent.Keyword), Token.plain(t" "))
            val named: Inline = Inline.Snippet(Language.Scala, intro + List(name))
            List(Inline.Textual(lead), named) + signature

          if where.nil && via.nil then Nil else List(Block.Paragraph(where + via))

        // Consecutive extensions of one source and one given are one table.
        def runs(remaining: List[Api.Applicable]): List[(Api.Applicable, List[Api.Applicable])] =
          remaining match
            case first :: _ =>
              val (run, rest) = remaining.span: (e: Api.Applicable) =>
                e.importFrom == first.importFrom && e.via == first.via

              (first, run) :: runs(rest)

            case _ => Nil

        val tables: List[Block] =
          runs(entity.extensions).bind: (first: Api.Applicable, run: List[Api.Applicable]) =>
            val these: List[Block.Row] = run.bind: (e: Api.Applicable) =>
              api(e.key).lay(Nil: List[Block.Row]): (member: Api.Entity) =>
                member.declarations.stdlib.lift(e.index) match
                  case Some(d) => List(row(member, d, false))
                  case None    => Nil

            source(first) + table(t"Name", t"Signature", these)

        val every: List[Block] =
          if entity.key == Applicables.any then Nil
          else api(Applicables.any).lay(Nil: List[Block]): (any: Api.Entity) =>
            val lead: Text = t"The extensions of every value are listed on "
            val text: List[Inline] = Inline.text(lead) + link(any.key) + Inline.text(t".")
            if any.extensions.nil then Nil else List(Block.Paragraph(text))

        List(Block.Heading(3, Inline.text(t"Extensions"))) + tables + every

    // The givens which provide this type, as a type class, and those which provide another for
    // it.
    val givens: List[Block] =
      if entity.givens.nil then Nil else
        val all: List[Api.Entity] = entity.givens.bind: (key: Text) => api(key).let(List(_)).or(Nil)

        def instance(g: Api.Entity): Boolean =
          g.declarations.exists(_.provides.let(_.head == entity.key).or(false))

        // A derived instance is its type class's, but for the type, its `derives` clause says it.
        val (instances, others0) = all.partition(instance)
        val others: List[Api.Entity] = others0.filter(!_.derived)

        def part(title: Text, these: List[Api.Entity]): List[Block] =
          val entries: List[Block.Row] = these.bind: (g: Api.Entity) =>
            g.declarations.map(row(g, _, false))

          if these.nil then Nil
          else List(Block.Paragraph(Inline.text(title))) + table(t"Name", t"Signature", entries)

        val givensHeading: Block = Block.Heading(3, Inline.text(t"Givens"))
        val parts: List[Block] =
          part(t"Instances:", instances) + part(t"For ${entity.name}:", others)

        if parts.nil then Nil else List(givensHeading) + parts

    val detail: List[Block] = doc.let(_.detail).lay(Nil: List[Block]): (text: Text) =>
      Blocks.of(Parser.parse(text), embeddings, resolver)

    val refer: List[Block] = doc.let(_.refer).lay(Nil: List[Block]): (names: List[Text]) =>
      if names.nil then Nil else
        val items: List[Block.Item] = names.map: (name: Text) =>
          val plain: Block.Item = Block.Item(List(Block.paragraph(name)))

          api.find(Designator(name)).lay(plain): (e: Api.Entity) =>
            val target: Inline.Destination = Inline.Destination.Local(e.url)
            Block.Item(List(Block.Paragraph(Inline.text(name))), Unset, target)

        List(Block.Heading(3, Inline.text(t"See also")), Block.Listing(false, items))

    place + List(heading) + info + removed + sections + extensions + givens + detail + refer

  // The navigation: the project's own packages, then its dependencies' (closed), each node a
  // link to its page, open along the way to `current`, with the packages, types and objects
  // beneath it (not every method and value, which its page lists).
  def tree(api: Api, current: Optional[Text]): Block.Tree =
    val path: List[Text] = current.lay(Nil: List[Text]): (key: Text) =>
      def up(designator: Optional[Designator]): List[Text] =
        designator.lay(Nil: List[Text]): (d: Designator) => List(Api.key(d)) + up(d.parent)

      api(key).lay(Nil: List[Text]): (e: Api.Entity) => List(e.key) + up(e.parent)

    // The project's own tree is shown whole; a dependency's unfolds only along the way to the
    // current page, since the dependencies together are tens of thousands of entities.
    def branch(c: Api.Entity): Boolean =
      val container: Boolean =
        c.isPackage || c.template.present || c.definition.let(_.kind == Api.Kind.Object).or(false)

      !c.hidden && !c.isParameter && container

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

      val label: List[Inline] = List(Signatures.code(List(Token(entity.name, accent))))
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

// A declaration's signature apart from its keyword and name, as one syntax: its clauses and
// its result, for a member's row.
private object Syntax0:
  def of(declaration: Api.Declaration): stenography.Syntax =
    val result: List[stenography.Syntax] =
      declaration.result.lay(Nil: List[stenography.Syntax]): (r: stenography.Syntax) =>
        declaration.kind match
          case Api.Kind.Type      => List(stenography.Syntax.Symbolic(t" = "), r)
          case Api.Kind.TypeParam => List(r)
          case _                  => List(stenography.Syntax.Symbolic(t": "), r)

    // A given object's type is its first parent.
    val provided: List[stenography.Syntax] =
      if declaration.kind == Api.Kind.Given && declaration.result.absent && !declaration.parents.nil
      then List(stenography.Syntax.Symbolic(t": "), declaration.parents.stdlib.head)
      else Nil

    stenography.Syntax.Compound(declaration.clauses + result + provided)
