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

import scala.collection.immutable as sci

import soundness.{Format as _, Query as _, *}

// The APIs of a project, one for each format it documents, Scala's first: each keeps its own
// entities by their own keys, and its own search, and an entity is named among them all by its
// address — its key, after its format's `id` and a `/` unless it is Scala's — as its page is
// by `/api/` and the same of its path. Since no Scala path holds a `/`, and no Scala key
// begins with a format's `id` and one, only `<id>/…` is another format's: `tels` alone is a
// package of that name.
object Apis:
  val empty: Apis = Apis(Nil)
  def of(api: Api): Apis = Apis(List(api))

case class Apis(all: List[Api]):
  def primary: Api = all match
    case first :: _ => first
    case _          => Api.empty

  def others: List[Api] = all.filter(_.format != primary.format)
  def isEmpty: Boolean = !all.exists(!_.entities.isEmpty)

  def apply(format: Text): Optional[Api] = all.seek(_.format.id == format)
  def of(entity: Api.Entity): Api = apply(entity.format).or(primary)

  // Every format's own words of a search, together.
  def vocabulary: sci.Map[Text, Query.Word] =
    all.stdlib.foldLeft(sci.Map[Text, Query.Word]()): (words, api) =>
      words ++ api.format.vocabulary

  // A search of every format's API, Scala's hits first: each format answers for itself, and
  // what one has to say of the query is kept only where it found something, or — when none
  // did — is Scala's.
  def search(query: Query, glossary: Glossary = Glossary.none): Search.Outcome =
    val outcomes: List[Search.Outcome] =
      all.filter(!_.entities.isEmpty).map(_.search(query, glossary))

    val found: List[Search.Outcome] = outcomes.filter(!_.hits.nil)
    val saying: List[Search.Outcome] =
      if found.nil then List.from(outcomes.stdlib.take(1)) else found

    Search.Outcome
      ( outcomes.bind(_.hits),
        saying.bind(_.notices).distinct,
        outcomes.bind(_.terms).distinct )

  // The API an address or a path belongs to, where it begins with the `id` of a format other
  // than the first and a `/`, with what follows them.
  def split(text: Text): Optional[(Api, Text)] =
    others.seek { (api: Api) => text.starts(t"${api.format.id}/") }.let: (api: Api) =>
      (api, text.skip(api.format.id.length + 1))

  // The entity an address names.
  def entity(address: Text): Optional[Api.Entity] = split(address) match
    case (api: Api, key: Text) => api(key)
    case _                     => primary(address)

  // The entity whose page is at a path beneath `/api/`.
  def locate(path: Text): Optional[Api.Entity] = split(path) match
    case (api: Api, rest: Text) => api.locate(rest)
    case _                      => primary.locate(path)
