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

import scala.caps

import soundness.*

import stratiform.TelSchematic

// The schema of a link's target, a sum, typed at a member position as TEL's specification
// types a sum there: a struct holding a `SelectRef` to it, which is how the derived codecs
// write a sum-typed field. Stratiform's derivation types such a member as a `Reference` to
// the select instead, which type assignment rejects as a kind mismatch (E217,
// propensive/soundness#2319); Pyrocosm wraps its own sums the same way, and this goes once
// stratiform derives the member form itself.
private[fuel] object TargetSchema:
  lazy val derived: Linkage.Target is TelSchematic over Tels.Type =
    stratiform.TelsDerivation.derived[Linkage.Target]

// A sum's type at a member position: a struct holding a `SelectRef` to it.
private def selectMember(name: Text): Tels.Type =
  val reference = Tels.SelectRef(Tels.Polarity.Implicit, Tels.Polarity.Implicit, name)
  Tels.Struct(Array(reference), Array.empty)

// Definitions with each member typed as a `Reference` to one of their selects retyped as the
// member form.
private def membered(definitions: TelSchematic.Definitions): TelSchematic.Definitions =
  val selects: List[Text] = definitions.selects.map(_.name)

  def retyped(member: Tels.Member): Tels.Member = member match
    case field: Tels.Field => field.fieldType match
      case Tels.Reference(name) if selects.has(name) => field.copy(fieldType = selectMember(name))
      case _                                         => field

    case other =>
      other

  val records: List[Tels.RecordDefinition] =
    definitions.records.map: (record: Tels.RecordDefinition) =>
      record.copy(members = Array.from(record.members.stdlib.toList.map(retyped)))

  TelSchematic.Definitions(records, definitions.selects)

// A derived sum's schematic, typed at a member position as the member form.
private[fuel] def atMember[value](derived: => value is TelSchematic over Tels.Type)
:   value is TelSchematic over Tels.Type =

  caps.unsafe.unsafeAssumePure:
    new TelSchematic:
      type Self = value
      private lazy val inner: value is TelSchematic over Tels.Type = derived

      def schema(): Tels.Type = inner.schema()

      override def fieldType: Tels.Type = inner.schema() match
        case Tels.Reference(name) => selectMember(name)
        case other                => other

      override def polarity: Tels.Polarity = inner.polarity
      override def repeatable: Tels.Polarity = inner.repeatable
      override def selectDefinitions: List[Tels.SelectDefinition] = inner.selectDefinitions

      override def definitions(seen: scala.collection.immutable.Set[Text])
      :   TelSchematic.Definitions =

        membered(inner.definitions(seen))

      override def layers(seen: scala.collection.immutable.Set[Text]): List[Tels.Layer] =
        inner.layers(seen)
