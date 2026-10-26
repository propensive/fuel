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

import soundness.{Language as _, *}

import pyrocosm.{Block, Language}

// What `fuel compile` writes into its output directory, each record as BinTEL under the schema
// derived from its type: a document's prose, with a sentinel where each fence was; the output
// of one fence; and an API entity's page. Every record is stored once, named by the Blake3
// hash of its own bytes, and the manifest says which inputs produced which.
object Output:
  // The tactic a derived codec raises through: vouched pure, as a derived instance must be
  // under capture checking (Pyrocosm's `Channel.derivation.throwing` is the same).
  object derivation:
    given throwing: [error <: Hazard] => Tactic[error] =
      scala.caps.unsafe.unsafeAssumePure[Tactic[error]](ThrowTactic[error, Any]())

  // One fence's output: its blocks (the code, and the diagnostics which follow it), the
  // problems it raised, and what it left to its session — whether it joined it, the names it
  // defines, and its top-level imports — so that a later fragment can be compiled after it
  // without compiling it again; the text a fragment which follows it is given; and the line
  // of the fence it was compiled at, from which its lines are shifted to wherever the fence
  // is now.
  case class Fragment
    ( blocks:   List[Block],
      problems: List[Block],
      joined:   Boolean,
      defined:  List[Text],
      imports:  List[Text],
      follow:   Text,
      line:     Int )

  // A document's prose: its blocks, each fence a sentinel to be filled from its fragment, and
  // the problems of its prose alone (references which named several entities).
  case class Prose(blocks: List[Block], problems: List[Block])

  // A document as it was compiled: its path within its section, its title, its blocks with
  // every fence filled, and its problems.
  case class Document(path: Text, title: Text, blocks: List[Block], problems: List[Block])

  // An API entity's page: the entity's key (`gossamer.Text#length`), its path beneath `/api/`
  // (`gossamer.Text~length`) and its blocks.
  case class Page(entity: Text, path: Text, blocks: List[Block])

  // The manifest, `index.fuel`: what produced the output — Fuel's version, the hash of the
  // classpath, and the hash of every input the whole output depends on — each document (by its
  // section, the document directory relative to the project root, and its path within it)
  // and each page, with the hash of the record which holds it; and the cache, from the key of
  // each fence's and each prose's inputs to the hash of the record which holds its output.
  case class Manifest
    ( fuel:        Text,
      classpath:   Text,
      environment: Text,
      documents:   List[Manifest.Document],
      pages:       List[Manifest.Page],
      cache:       List[Manifest.Entry] )

  object Manifest:
    case class Document(section: Text, path: Text, title: Text, record: Text)
    case class Page(entity: Text, path: Text, key: Text, record: Text)
    case class Entry(key: Text, record: Text)

  // Each record's BinTEL, derived here, once, where Pyrocosm's package-level codecs are in
  // scope as the model's own derivations see them; `Unset` for bytes which are not one.
  object Fragment:
    import derivation.throwing
    import pyrocosm.given

    def encode(fragment: Fragment): Data = fragment.bintel
    def decode(data: Data): Optional[Fragment] = try Bintel.read[Fragment](data) catch case _: Exception => Unset

  object Prose:
    import derivation.throwing
    import pyrocosm.given

    def encode(prose: Prose): Data = prose.bintel
    def decode(data: Data): Optional[Prose] = try Bintel.read[Prose](data) catch case _: Exception => Unset

  object Page:
    import derivation.throwing
    import pyrocosm.given

    def encode(page: Page): Data = page.bintel
    def decode(data: Data): Optional[Page] = try Bintel.read[Page](data) catch case _: Exception => Unset

  object Document:
    import derivation.throwing
    import pyrocosm.given

    def encode(document: Document): Data = document.bintel

    def decode(data: Data): Optional[Document] =
      try Bintel.read[Document](data) catch case _: Exception => Unset

  object ManifestCodec:
    import derivation.throwing

    def encode(manifest: Manifest): Data = manifest.bintel

    def decode(data: Data): Optional[Manifest] =
      try Bintel.read[Manifest](data) catch case _: Exception => Unset

  // A fence's place in a document's prose, until its output fills it: a code block of no
  // lines, in a language of Fuel's own, numbered by the fence's index in the document.
  val fence: Language = Language(t"fuel-fence")

  def sentinel(index: Int): Block = Block.Code(fence, Nil, Nil, index)
