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

import pyrocosm.{Block, Tone}

// Where translations are, and a Markdown document held to its own. The configuration gives
// each `document` directory, with the overlays inside it, a directory for each language it is
// translated into, and the translation of a file is the file of the same relative path there:
// with `translation fr doc/fr` beneath `document doc/en`, `doc/en/prose/intro.md` is
// translated into French by `doc/fr/prose/intro.md`, and an overlay `doc/en/api/gossamer.tel`
// by `doc/fr/api/gossamer.tel`. Nothing names a translation but its place. A language may
// have a `fallback`, another language of the same directory, or the language the directory is
// written in, whose text stands for its own where it has none: such a language need not
// translate everything. A document is checked against each translation: read through `read`,
// as a `Translation` of the document's text and a `Mirror` of its structure. Everything about
// a translation's text is reported in the translation's file; that there is none, in the
// document.
object Translations:
  // A language's translations of a `document` directory, `base`: the directory which mirrors
  // it, and the language which stands in where it has no file.
  case class Target
    ( tongue: Text, base: Text, directory: Text, fallback: Optional[Text] = Unset )

  // The translations of the directory a file is in.
  def within(path: Text, targets: List[Target]): List[Target] =
    targets.filter { (target: Target) => beneath(path, target.base).present }

  // The languages which need not translate everything, having another to fall back to.
  def partial(targets: List[Target]): List[Text] =
    targets.filter(_.fallback.present).map(_.tongue)

  // The languages a file is read in for one language, in order: itself, then its fallback,
  // and that one's, among the translations of one directory; a language is tried once, should
  // the fallbacks come round. The last may be the language the directory is written in.
  def chain(tongue: Text, targets: List[Target]): List[Text] =
    def recur(tongue: Text, seen: List[Text]): List[Text] =
      if seen.has(tongue) then seen else
        val next: Optional[Text] = targets.filter(_.tongue == tongue).head.let(_.fallback)
        next.lay(seen + List(tongue))(recur(_, seen + List(tongue)))

    recur(tongue, Nil)

  // The file which is read for a file in a language: its translation, or the first there is
  // among its fallbacks'; `Unset` where the definitive file itself is what there is.
  def serving(path: Text, tongue: Text, targets: List[Target], exists: Text => Boolean)
  :   Optional[Text] =

    val places: List[(Text, Text)] = of(path, targets)

    chain(tongue, within(path, targets)).bind: (tongue: Text) =>
      places.filter(_(0) == tongue).map(_(1)).filter(exists)

    . head

  // A path beneath a directory, as it is from there.
  private def beneath(path: Text, directory: Text): Optional[Text] =
    if path.starts(t"$directory/") then path.skip(directory.length + 1) else Unset

  // The translations of a file: a language and a path each, one for a language.
  def of(path: Text, targets: List[Target]): List[(Text, Text)] =
    val all: List[(Text, Text)] = targets.bind: (target: Target) =>
      beneath(path, target.base).lay(List[(Text, Text)]()): (within: Text) =>
        List((target.tongue, t"${target.directory}/$within"))

    all.filter { (tongue: Text, file: Text) => all.filter(_(0) == tongue).head == (tongue, file) }

  // What a file is a translation of, and into which language, if it is in a directory of
  // translations: such a file is not a document, nor an overlay, of its own.
  def original(path: Text, targets: List[Target]): Optional[(Text, Text)] =
    targets.bind: (target: Target) =>
      beneath(path, target.directory).lay(List[(Text, Text)]()): (within: Text) =>
        List((target.tongue, t"${target.base}/$within"))

    . head

  def held(path: Text, targets: List[Target]): Boolean = original(path, targets).present

  def problem(tone: Tone, at: Text, message: Text): Block =
    Block.Diagnostic(tone, Block.Location(at, Unset), List(Block.paragraph(message)), Nil)

  // That a file has no translation into a language.
  def absent(path: Text, tongue: Text, target: Text): Block =
    val message: Text =
      t"This has no translation into `$tongue`, which would be `$target`: `fuel translate` " +
        t"writes it."

    problem(Tone.Warning, path, message)

  // What is wrong with a document's translations, each a language and a path: `path` is the
  // document as it is reported. A language among `partial` may have no translation of it.
  def check
    ( path:         Text,
      text:         Text,
      translations: List[(Text, Text)],
      read:         Text => Optional[Text],
      partial:      List[Text] = Nil )
  :   List[Block] =

    translations.bind: (tongue: Text, target: Text) =>
      val missing: List[Block] =
        if partial.has(tongue) then Nil else List(absent(path, tongue, target))

      read(target).lay(missing): (source: Text) =>
        val findings: List[Translation.Finding] =
          Translation.findings(source, text) + Mirror.findings(source)

        Translation.report(target, source, findings).map { diagnostic => diagnostic: Block }
