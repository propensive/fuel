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

import soundness.{Format as _, Language as _, Token as _, *}

import dysasymptotics.linearSize
import pyrocosm.{Block, Figure, Inline, Language, Token, Tone}

// Scala, and Java as Scala sees it: the format of what `Inspection` reads from compiled code.
// Its glyphs are `Icons`' and its signatures `Signatures`'; a type's section has its place in
// the hierarchy beneath its signature, and the extensions an entity declares in a table per
// clause they extend; and what applies to a type beyond its members is the extensions which
// reach it (a table per source, in the order `Applicables` ranks them, each headed by what
// brings it: the companion, an import, a given) and the givens of it and for it.
object ScalaFormat extends Format:
  def id: Text = t"scala"
  def title: Phrases.Phrase = Phrases.api
  def language: Language = Language.Scala
  def noun: Text = t"package"

  override def glyph(declaration: Api.Declaration, exported: Boolean = false): Inline =
    Icons.of(declaration, exported)

  override def signature(name: Text, declaration: Api.Declaration, api: Api, scope: Scope)
  :   Block =

    Signatures.block(name, declaration, api, scope)

  override def cell(declaration: Api.Declaration, api: Api, scope: Scope): Inline =
    Signatures.cell(declaration, api, scope)

  // The packages, the types and the objects, not every method and value, which a page lists.
  override def branch(entity: Api.Entity): Boolean =
    val container: Boolean =
      entity.isPackage || entity.template.present
      || entity.definition.let(_.kind == Api.Kind.Object).or(false)

    !entity.isParameter && container

  override def documentable(entity: Api.Entity): Boolean =
    !entity.isPackage && !entity.isParameter && !entity.derived

  override def typedRoots: Boolean = false
  override def standard(configured: List[Locator]): List[Locator] = configured
  override def decorate(api: Api): Api = Applicables.decorate(api)

  override def sections(page: ApiPages.Context): List[Block] =
    given Locale[Phrases.Spoken] = page.locale
    import page.{api, entity, scope}

    def row(member: Api.Entity, d: Api.Declaration, anchored: Boolean): Block.Row =
      page.row(member, d, anchored)

    def section(declaration: Api.Declaration, ofType: Boolean): List[Block] =
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

      val members: List[Api.Entity] = page.members(ofType)

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
          clause :: ApiPages.table(Phrases.name(), Phrases.signature(), entries)

      ApiPages.section(page, declaration, ofType, plain, hierarchy, groups)

    val definition: List[Block] = entity.definition.let(section(_, false)).or(Nil)
    val template: List[Block] = entity.template.let(section(_, true)).or(Nil)

    val overloads: List[Block] = entity.overloads.bind: (d: Api.Declaration) =>
      List(Signatures.block(entity.name, d, api, scope))

    // An export is one signature, of what it exports, whatever forwarders its clause made.
    if entity.exported then List(Signatures.exported(entity, api, scope))
    else definition + template + overloads

  override def applicable(page: ApiPages.Context): List[Block] =
    given Locale[Phrases.Spoken] = page.locale
    import page.{api, entity, scope, link}

    def row(member: Api.Entity, d: Api.Declaration, anchored: Boolean): Block.Row =
      page.row(member, d, anchored)

    // The extensions which apply to the type, each run of one source one table, headed by what
    // brings it: declared in the companion (or a base's), or by an import, or within a given;
    // and the given which makes a type parameter's extension apply, where one does.
    val extensions: List[Block] =
      if entity.extensions.nil then Nil else
        def source(first: Api.Applicable): List[Block] =
          val from: Optional[Api.Entity] = first.importFrom.let(api(_))

          val where: List[Inline] = from match
            case from: Api.Entity =>
              val companion: Text = Phrases.inCompanionOf()

              if from.key == entity.key then Inline.text(Phrases.inCompanion())
              else if entity.bases.has(from.key) then Inline.text(companion) + link(from.key)
              else
                val tokens: List[Token] =
                  List
                    ( Token(t"import", Token.Accent.Keyword),
                      Token.plain(t" "),
                      Token(from.key, Token.Accent.Term, Unset, Inline.Destination.Local(from.url)),
                      Token(t".*", Token.Accent.Symbol) )

                List(Inline.Textual(Phrases.by()), Signatures.code(tokens))

            case _ => Nil

          val via: List[Inline] = first.via.let(api(_)).lay(Nil: List[Inline]): (g: Api.Entity) =>
            val within: Boolean = first.key.starts(t"${g.key}.")
            val lead: Text = if within then Phrases.within() else Phrases.withGiven()

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

            source(first) + ApiPages.table(Phrases.name(), Phrases.signature(), these)

        val every: List[Block] =
          if entity.key == Applicables.any then Nil
          else api(Applicables.any).lay(Nil: List[Block]): (any: Api.Entity) =>
            val lead: Text = Phrases.everyValueBefore()

            val text: List[Inline] =
              Inline.text(lead) + link(any.key) + Inline.text(Phrases.everyValueAfter())

            if any.extensions.nil then Nil else List(Block.Paragraph(text))

        List(Block.Heading(3, Inline.text(Phrases.extensions()))) + tables + every

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
          else
            List(Block.Paragraph(Inline.text(title))) +
              ApiPages.table(Phrases.name(), Phrases.signature(), entries)

        val givensHeading: Block = Block.Heading(3, Inline.text(Phrases.givens()))

        val parts: List[Block] =
          part(Phrases.instances(), instances) + part(Phrases.givensFor(entity.name)(), others)

        if parts.nil then Nil else List(givensHeading) + parts

    extensions + givens

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
