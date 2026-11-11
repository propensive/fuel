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

import pyrocosm.{Block, Inline, Language, Token}
import stenography.Designator

// What an API is declared in — Scala, a TEL schema, a tool's commands — as far as documenting
// it differs: entities of every format are documented by overlays and shown on pages of one
// layout, and a format says what is its own. Its `id` names it in an overlay's `format` and in
// an address; its `title` heads its entities in the navigation; its `language` is what its
// names and signatures are set as; its `noun` is what an overlay's `base` names, in a message.
// It draws the glyph of a declaration, and writes its signature, whole (for the page) and
// without its keyword and name (for a row's cell) — from the words the declaration keeps as
// its `signature`, unless the format writes them its own way; it may give an entity's page
// sections of its own, and others for what applies to the entity beyond what it declares; and
// it says which entities are nodes of the navigation, which are cautioned when nothing
// documents them, what resolves in it without an import, and what a whole model gains once it
// is assembled.
trait Format:
  def id: Text
  def title: Text
  def language: Language
  def noun: Text

  // A format without drawings of its own has an empty holder, described by the keyword.
  def glyph(declaration: Api.Declaration, exported: Boolean = false): Inline =
    Inline.Icon(t"", declaration.kind.keyword)

  def signature(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): Block =
    val accent: Token.Accent =
      if declaration.kind.isType then Token.Accent.Typal else Token.Accent.Term

    val space: Token = Token.plain(t" ")
    val words: Optional[Api.Signature] = declaration.signature
    val tail: List[Token] = ApiPages.phrases(api, words.let(_.tail).or(Nil))

    val head: List[Token] =
      List
        ( Token(declaration.kind.keyword, Token.Accent.Keyword),
          space,
          Token(name, accent, Token.Role.Binding) )

    val first: List[Token] = if tail.nil then head else head + List(space) + tail

    val body: List[Block.Line] = words.let(_.body).or(Nil).map: (line: List[Api.Phrase]) =>
      Block.Line(Token.plain(t"  ") :: ApiPages.phrases(api, line))

    Block.Code(language, Block.Line(first) :: body)

  def cell(declaration: Api.Declaration, api: Api, scope: Scope): Inline =
    Inline.Snippet(language, ApiPages.phrases(api, declaration.signature.let(_.tail).or(Nil)))

  // The glyphs of a name: its type's and its term's, where it is both, or that of the one it is;
  // apart, so that a terminal, which says each one's keywords, keeps them apart too.
  def glyphs(entity: Api.Entity, exported: Boolean = false): List[Inline] =
    (entity.template, entity.definition) match
      case (template: Api.Declaration, definition: Api.Declaration) =>
        List(glyph(template, exported), Inline.Textual(t" "), glyph(definition, exported))

      case (template: Api.Declaration, _) =>
        List(glyph(template, exported))

      case (_, definition: Api.Declaration) =>
        List(glyph(definition, exported))

      case _ =>
        Nil

  def sections(page: ApiPages.Context): List[Block] = ApiPages.sections(page)
  def applicable(page: ApiPages.Context): List[Block] = Nil

  // A node of the navigation: by default, whatever has members of its own to lead to.
  def branch(entity: Api.Entity): Boolean = !entity.isParameter && !entity.members.nil

  def documentable(entity: Api.Entity): Boolean = !entity.isParameter

  // What an entity's references resolve through without an import, of what the project's
  // options say every Scala source sees: nothing, but for Scala.
  def standard(configured: List[Designator]): List[Designator] = Nil
  def decorate(api: Api): Api = api
