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

import soundness.{Format as _, Language as _, *}

import pyrocosm.{Block, Inline, Language}

// What an API is declared in — Scala, a TEL schema, a tool's commands — as far as documenting
// it differs: entities of every format are documented by overlays and shown on pages of one
// layout, and a format says what is its own. Its `id` names it in an overlay's `format` and in
// an address; its `title` heads its entities in the navigation; its `language` is what its
// names and signatures are set as; its `noun` is what an overlay's `base` names, in a message.
// It draws the glyph of a declaration, and writes its signature, whole (for the page) and
// without its keyword and name (for a row's cell); it may give an entity's page sections of its
// own, and others for what applies to the entity beyond what it declares; and it says which
// entities are nodes of the navigation, which are cautioned when nothing documents them, and
// what a whole model gains once it is assembled.
trait Format:
  def id: Text
  def title: Text
  def language: Language
  def noun: Text

  def glyph(declaration: Api.Declaration, exported: Boolean = false): Inline
  def signature(name: Text, declaration: Api.Declaration, api: Api, scope: Scope): Block
  def cell(declaration: Api.Declaration, api: Api, scope: Scope): Inline

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
  def decorate(api: Api): Api = api
