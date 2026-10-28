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
import pyrocosm.{Block, Inline, Language, Token, Tone}
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
  def page(api: Api, entity: Api.Entity, resolver: Resolver, scope: Scope, embeddings: List[Embedding]): List[Block] =
    val doc: Optional[Api.Entry] = entity.doc

    val place: List[Block] =
      def crumbs(designator: Optional[Designator]): List[Inline] = designator.lay(Nil: List[Inline]): (d: Designator) =>
        val link: Inline = api(Api.key(d)).lay(Inline.Textual(d.name)) { (e: Api.Entity) => Inline.Link(Inline.Destination.Local(e.url), Inline.text(d.name)) }
        val separator: Text = d match
          case Designator.Type(_, _) => t"#"
          case _                     => t"."

        crumbs(d.parent) + List(link, Inline.Toned(Tone.Muted, Inline.text(separator)))

      val above: List[Inline] = crumbs(entity.parent)
      if above.nil then Nil else List(Block.Paragraph(above))

    val heading: Block = Block.Heading(1, Inline.text(entity.name))
    val info: List[Block] = doc.let(_.info).let { (text: Text) => List(Block.Paragraph(Blocks.inline(text, resolver))) }.or(Nil)

    val removed: List[Block] = doc.let(_.until).lay(Nil: List[Block]): (until: Api.Until) =>
      val replacement: List[Inline] = until.replacement.lay(Nil: List[Inline]): (name: Text) =>
        val link: Inline = api.find(Designator(name)).lay(Inline.Snippet(pyrocosm.Language.Scala, List(pyrocosm.Token(name, pyrocosm.Token.Accent.Term)))):
          (e: Api.Entity) => Inline.Link(Inline.Destination.Local(e.url), Inline.text(name))
        List(Inline.Textual(t"; use "), link, Inline.Textual(t" instead"))

      val verb: Text = if entity.removed then t"was removed in" else t"is removed in"
      List(Block.Notice(Tone.Warning, Unset, List(Block.Paragraph(List(Inline.Textual(t"${entity.name} $verb ${until.version}")) + replacement + List(Inline.Textual(t"."))))))

    val children: List[Api.Entity] = api.children(entity).filter(!_.hidden)
    val (parameters, others) = children.partition(_.isParameter)

    // A declaration's row; a member's own rows are anchored by its name, another page's are
    // not (its anchors are its own).
    def row(member: Api.Entity, d: Api.Declaration, anchored: Boolean): Block.Row =
      val kind: List[Inline] = List(Inline.Toned(Tone.Muted, Inline.text(d.kind.keyword)))
      val name: List[Inline] = List(Inline.Strong(List(Inline.Link(Inline.Destination.Local(member.url), Inline.text(member.name)))))
      val signature: List[Inline] = List(Signatures.code(Signatures.tokens(Syntax0.of(d), api, scope)))
      val info: List[Inline] = member.doc.let(_.info).let(Blocks.inline(_, resolver)).or(Nil)
      val tone: Optional[Tone] = if member.removed then Tone.Muted else Unset
      Block.Row(List(Block.Cell(kind), Block.Cell(name), Block.Cell(signature), Block.Cell(info)), tone, Unset, if anchored then member.name else Unset)

    def rows(members: List[Api.Entity]): List[Block.Row] = members.bind: (member: Api.Entity) =>
      member.declarations.map(row(member, _, true))

    def table(heading: Text, signature: Text, rows: List[Block.Row]): List[Block] =
      val columns: List[Block.Column] =
        List(Block.Column(Inline.text(t"")), Block.Column(Inline.text(heading)), Block.Column(Inline.text(signature)), Block.Column(Inline.text(t"Description")))

      if rows.nil then Nil else List(Block.Table(columns, rows, Unset))

    // The parameters belong to the signature of the type, where there is one, since a class's
    // are its constructor's; else to the term's.
    def section(declaration: Api.Declaration, ofType: Boolean): List[Block] =
      val signature: Block = Signatures.line(Signatures.declaration(entity.name, declaration, api, scope))

      val members: List[Api.Entity] = others.filter: (child: Api.Entity) =>
        child.parent.let(_.isInstanceOf[Designator.Type]).or(false) == ofType

      // The extensions this entity declares are grouped under the clauses they extend, each
      // group a table of its own headed by its clause, after the other members.
      val declared: List[(Api.Entity, Api.Declaration)] =
        members.bind { (m: Api.Entity) => m.declarations.map((m, _)) }

      val (extended, plain) = declared.partition(_(1).kind == Api.Kind.Extension)

      val groups: List[Block] =
        val keys: List[Text] = extended.map(_(1).group.map(_.qualified).join(t" "))

        keys.distinct.bind: (key: Text) =>
          val these: List[(Api.Entity, Api.Declaration)] = extended.filter(_(1).group.map(_.qualified).join(t" ") == key)
          val clause: Block = Signatures.line(Signatures.group(these.stdlib.head(1).group, api, scope))
          clause :: table(t"Name", t"Signature", these.map { (m: Api.Entity, d: Api.Declaration) => row(m, d, true) })

      val own: List[Block.Row] = if ofType || entity.template.absent then rows(parameters) else Nil
      signature :: table(t"Parameter", t"Type", own) + table(t"Name", t"Signature", plain.map { (m: Api.Entity, d: Api.Declaration) => row(m, d, true) }) + groups

    val sections: List[Block] =
      entity.definition.let(section(_, false)).or(Nil)
      + entity.template.let(section(_, true)).or(Nil)
      + entity.overloads.bind { (d: Api.Declaration) => List(Signatures.line(Signatures.declaration(entity.name, d, api, scope))) }

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
            case Unset => Nil
            case from: Api.Entity =>
              if from.key == entity.key then Inline.text(t"In the companion object")
              else if entity.bases.has(from.key) then Inline.text(t"In the companion object of ") + link(from.key)
              else
                val tokens: List[Token] =
                  List(Token(t"import", Token.Accent.Keyword), Token.plain(t" "), Token(from.key, Token.Accent.Term, Unset, Inline.Destination.Local(from.url)), Token(t".*", Token.Accent.Symbol))

                List(Inline.Textual(t"By "), Signatures.code(tokens))

          val via: List[Inline] = first.via.let(api(_)).lay(Nil: List[Inline]): (g: Api.Entity) =>
            val within: Boolean = first.key.starts(t"${g.key}.")
            val lead: Text = if within then t", within " else t", with "
            val signature: List[Inline] = g.definition.let { (d: Api.Declaration) => List(Signatures.code(Signatures.tokens(Syntax0.of(d), api, scope))) }.or(Nil)
            val named: Inline = Inline.Snippet(Language.Scala, List(Token(t"given", Token.Accent.Keyword), Token.plain(t" "), Token(g.name, Token.Accent.Term, Unset, Inline.Destination.Local(g.url))))
            List(Inline.Textual(lead), named) + signature

          if where.nil && via.nil then Nil else List(Block.Paragraph(where + via))

        // Consecutive extensions of one source and one given are one table.
        def runs(remaining: List[Api.Applicable]): List[(Api.Applicable, List[Api.Applicable])] = remaining match
          case Nil => Nil
          case first :: _ =>
            val (run, rest) = remaining.span { (e: Api.Applicable) => e.importFrom == first.importFrom && e.via == first.via }
            (first, run) :: runs(rest)

        val tables: List[Block] = runs(entity.extensions).bind: (first: Api.Applicable, run: List[Api.Applicable]) =>
          val these: List[Block.Row] = run.bind: (e: Api.Applicable) =>
            api(e.key).lay(Nil: List[Block.Row]): (member: Api.Entity) =>
              member.declarations.stdlib.lift(e.index) match
                case Some(d) => List(row(member, d, false))
                case None    => Nil

          source(first) + table(t"Name", t"Signature", these)

        val every: List[Block] =
          if entity.key == Applicables.any then Nil
          else api(Applicables.any).lay(Nil: List[Block]): (any: Api.Entity) =>
            if any.extensions.nil then Nil else List(Block.Paragraph(Inline.text(t"The extensions of every value are listed on ") + link(any.key) + Inline.text(t".")))

        List(Block.Heading(3, Inline.text(t"Extensions"))) + tables + every

    // The givens which provide this type, as a type class, and those which provide another for
    // it.
    val givens: List[Block] =
      if entity.givens.nil then Nil else
        val all: List[Api.Entity] = entity.givens.bind { (key: Text) => api(key).let(List(_)).or(Nil) }
        val (instances, others) = all.partition(_.declarations.exists(_.provides.let(_.head == entity.key).or(false)))

        def part(title: Text, these: List[Api.Entity]): List[Block] =
          if these.nil then Nil else List(Block.Paragraph(Inline.text(title))) + table(t"Name", t"Signature", these.bind { (g: Api.Entity) => g.declarations.map(row(g, _, false)) })

        List(Block.Heading(3, Inline.text(t"Givens"))) + part(t"Instances:", instances) + part(t"For ${entity.name}:", others)

    val detail: List[Block] = doc.let(_.detail).let { (text: Text) => Blocks.of(Parser.parse(text), embeddings, resolver) }.or(Nil)

    val refer: List[Block] = doc.let(_.refer).lay(Nil: List[Block]): (names: List[Text]) =>
      if names.nil then Nil else
        val items: List[Block.Item] = names.map: (name: Text) =>
          api.find(Designator(name)).lay(Block.Item(List(Block.paragraph(name)))):
            (e: Api.Entity) => Block.Item(List(Block.Paragraph(Inline.text(name))), Unset, Inline.Destination.Local(e.url))

        List(Block.Heading(3, Inline.text(t"See also")), Block.Listing(false, items))

    place + List(heading) + info + removed + sections + extensions + givens + detail + refer

  // The navigation: the project's own packages, then its dependencies' (closed), each node a
  // link to its page, open along the way to `current`, with the packages, types and objects
  // beneath it (not every method and value, which its page lists).
  def tree(api: Api, current: Optional[Text]): Block.Tree =
    val path: List[Text] = current.lay(Nil: List[Text]): (key: Text) =>
      def up(designator: Optional[Designator]): List[Text] = designator.lay(Nil: List[Text]) { (d: Designator) => List(Api.key(d)) + up(d.parent) }
      api(key).lay(Nil: List[Text]) { (e: Api.Entity) => List(e.key) + up(e.parent) }

    // The project's own tree is shown whole; a dependency's unfolds only along the way to the
    // current page, since the dependencies together are tens of thousands of entities.
    def node(entity: Api.Entity, whole: Boolean): Block.TreeNode =
      val children: List[Block.TreeNode] =
        if !whole && !path.has(entity.key) then Nil
        else api.children(entity).filter { (c: Api.Entity) => !c.hidden && !c.isParameter && (c.isPackage || c.template.present || c.definition.let(_.kind == Api.Kind.Object).or(false)) }.map(node(_, whole))

      val open: Optional[Boolean] = if children.nil then Unset else path.has(entity.key)
      val tone: Optional[Tone] = if entity.key == current then Tone.Accent else if entity.removed then Tone.Muted else Unset
      Block.TreeNode(Inline.text(entity.name), children, tone, Unset, open, Inline.Destination.Local(entity.url))

    val roots: List[Api.Entity] = api.roots
    val (own, dependencies) = roots.partition(ownBeneath(api, _))
    val ownNodes: List[Block.TreeNode] = own.map(node(_, true))

    val dependencyNodes: List[Block.TreeNode] =
      if dependencies.nil then Nil
      else List(Block.TreeNode(Inline.text(t"Dependencies"), dependencies.map(node(_, false)), Tone.Muted, Unset, dependencies.exists { (d: Api.Entity) => path.has(d.key) }))

    Block.Tree(ownNodes + dependencyNodes)

  // Whether a root package has any of the project's own entities beneath it.
  private def ownBeneath(api: Api, root: Api.Entity): Boolean =
    api.entities.values.exists { e => e.own && (e.key == root.key || e.key.starts(t"${root.key}.") || e.key.starts(t"${root.key}#")) }

// A declaration's signature apart from its keyword and name, as one syntax: its clauses and
// its result, for a member's row.
private object Syntax0:
  def of(declaration: Api.Declaration): stenography.Syntax =
    val result: List[stenography.Syntax] = declaration.result.lay(Nil: List[stenography.Syntax]): (r: stenography.Syntax) =>
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
