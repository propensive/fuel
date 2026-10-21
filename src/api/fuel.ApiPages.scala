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
import pyrocosm.{Block, Inline, Tone}
import stenography.Designator

// An entity's page, and the navigation of the whole API, as blocks: Amok's `JvmFolio` layout.
// The page is its place (each parent a link), its name, its one-line info, then for each of
// its declarations the signature and a table of the members which belong to it (kind, name
// linked and anchored by its id, signature, info; an extension's group heads its rows), then
// its Markdown detail, what it refers to, and — for an entity nothing declares any more — the
// notice of its removal and what replaces it.
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
        val link: Inline = api.find(Designator(name)).lay(Inline.Code(pyrocosm.Language.Scala, List(pyrocosm.Token(name, pyrocosm.Token.Accent.Term)))):
          (e: Api.Entity) => Inline.Link(Inline.Destination.Local(e.url), Inline.text(name))
        List(Inline.Textual(t"; use "), link, Inline.Textual(t" instead"))

      val verb: Text = if entity.removed then t"was removed in" else t"is removed in"
      List(Block.Notice(Tone.Warning, Unset, List(Block.Paragraph(List(Inline.Textual(t"${entity.name} $verb ${until.version}")) + replacement + List(Inline.Textual(t"."))))))

    val children: List[Api.Entity] = api.children(entity).filter(!_.hidden)

    def section(declaration: Api.Declaration, ofType: Boolean): List[Block] =
      val signature: Block = Signatures.line(Signatures.declaration(entity.name, declaration, api, scope))

      val members: List[Api.Entity] = children.filter: (child: Api.Entity) =>
        child.parent.let(_.isInstanceOf[Designator.Type]).or(false) == ofType

      val rows: List[Block.Row] = members.bind: (member: Api.Entity) =>
        member.declarations.map: (d: Api.Declaration) =>
          val kind: List[Inline] = List(Inline.Toned(Tone.Muted, Inline.text(d.kind.keyword)))
          val name: List[Inline] = List(Inline.Strong(List(Inline.Link(Inline.Destination.Local(member.url), Inline.text(member.name)))))
          val signature: List[Inline] = List(Signatures.code(Signatures.tokens(Syntax0.of(d), api, scope)))
          val info: List[Inline] = member.doc.let(_.info).let(Blocks.inline(_, resolver)).or(Nil)
          val tone: Optional[Tone] = if member.removed then Tone.Muted else Unset
          Block.Row(List(Block.Cell(kind), Block.Cell(name), Block.Cell(signature), Block.Cell(info)), tone, Unset, member.name)

      // Extension rows are grouped under the clause they extend.
      val grouped: List[Block.Row] =
        val (extensions, plain) = members.partition(_.declarations.exists(_.kind == Api.Kind.Extension))
        rows

      val columns: List[Block.Column] =
        List(Block.Column(Inline.text(t"")), Block.Column(Inline.text(t"Name")), Block.Column(Inline.text(t"Signature")), Block.Column(Inline.text(t"Description")))

      val table: List[Block] = if rows.nil then Nil else List(Block.Table(columns, grouped, Unset))
      signature :: table

    val sections: List[Block] =
      entity.definition.let(section(_, false)).or(Nil)
      + entity.template.let(section(_, true)).or(Nil)
      + entity.overloads.bind { (d: Api.Declaration) => List(Signatures.line(Signatures.declaration(entity.name, d, api, scope))) }

    val detail: List[Block] = doc.let(_.detail).let { (text: Text) => Blocks.of(Parser.parse(text), embeddings, resolver) }.or(Nil)

    val refer: List[Block] = doc.let(_.refer).lay(Nil: List[Block]): (names: List[Text]) =>
      if names.nil then Nil else
        val items: List[Block.Item] = names.map: (name: Text) =>
          api.find(Designator(name)).lay(Block.Item(List(Block.paragraph(name)))):
            (e: Api.Entity) => Block.Item(List(Block.Paragraph(Inline.text(name))), Unset, Inline.Destination.Local(e.url))

        List(Block.Heading(3, Inline.text(t"See also")), Block.Listing(false, items))

    place + List(heading) + info + removed + sections + detail + refer

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
        else api.children(entity).filter { (c: Api.Entity) => !c.hidden && (c.isPackage || c.template.present || c.definition.let(_.kind == Api.Kind.Object).or(false)) }.map(node(_, whole))

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
        case Api.Kind.Type => List(stenography.Syntax.Symbolic(t" = "), r)
        case _             => List(stenography.Syntax.Symbolic(t": "), r)

    stenography.Syntax.Compound(declaration.clauses + result)
