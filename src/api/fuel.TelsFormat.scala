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

import soundness.{Format as _, Language as _, Query as _, Token as _, *}

import codepages.utf8Codepage
import dysasymptotics.{linearAccess, linearSize}
import errorDiagnostics.emptyDiagnostics
import pyrocosm.{Block, Inline, Language, Token}
import stratiform.Tels

// A TEL schema, as an API: what a document written to it may say. The schema is the top-level
// name. Its definitions — records, selects and scalars — are the members of its term form
// (`presentation.Slide`), and the fields of its `document` those of its type form
// (`presentation#title`); a record's fields and a select's variants are members of the type
// form of theirs (`presentation.Slide#transcript`, `presentation.Part#markdown`). A member's
// signature is the words which declare it: a field's type and whether it is `optional`,
// `repeatable` or the `key`, with its `default`; a scalar's validators, patterns and encoding;
// a record's `select`s, whose variants are the record's to say, so that a reference to one
// through the record (`Slide#markdown`) finds it. Members keep the order the schema declares
// them in, records first, then selects and scalars.
//
// What a definition, a field or a variant says of itself in its `description` documents it,
// unless an overlay does: the first sentence is its `info`, and the whole its `detail` where
// there is more. A schema is read from its file, held to the schema of schemas — each fault at
// its place — and to the rules every schema is, and contributes nothing where it fails either.
object TelsFormat extends Format:
  def id: Text = t"tels"
  def title: Phrases.Phrase = Phrases.schemas
  def language: Language = Language.Tel
  def noun: Text = t"schema"

  sealed abstract class Kind(val keyword: Text, val isType: Boolean = false) extends Api.Kind

  object Kind:
    case object Schema   extends Kind(t"schema")
    case object Document extends Kind(t"document", true)
    case object Record   extends Kind(t"record", true)
    case object Select   extends Kind(t"select", true)
    case object Scalar   extends Kind(t"scalar", true)
    case object Field    extends Kind(t"field")
    case object Variant  extends Kind(t"variant")

  private val modifiers: List[Text] =
    List(t"optional", t"required", t"repeatable", t"irrepeatable", t"key")

  override val vocabulary: sci.Map[Text, Query.Word] =
    val kinds: List[Kind] =
      List
        ( Kind.Schema, Kind.Document, Kind.Record, Kind.Select, Kind.Scalar, Kind.Field,
          Kind.Variant )

    val words: scala.List[(Text, Query.Word)] =
      kinds.stdlib.toList.map { (kind: Kind) => (kind.keyword, Query.Word(sci.Set(kind))) }
      ++ modifiers.stdlib.toList.map { (word: Text) => (word, Query.Word(modifier = word)) }

    sci.Map.from(words)

  // The glyph of the Scala keyword a kind is most like, until it has a drawing of its own: a
  // schema's is a package's, a record's a class's, a select's an enum's, a variant's a case's,
  // a scalar's a type's and a field's a value's. A member which is `optional` has a small `?`
  // beside its glyph, one which is `repeatable` a `+`, one which is both a `*`, and the `key` a
  // small key (`Icons`). A document has none: the schema's glyph stands for both of its forms.
  override def glyph(declaration: Api.Declaration, exported: Boolean = false): Inline =
    val analogue: Optional[Api.Kind] = declaration.kind match
      case Kind.Schema  => Api.Kind.Package
      case Kind.Record  => Api.Kind.Class
      case Kind.Select  => Api.Kind.Enum
      case Kind.Variant => Api.Kind.Case
      case Kind.Scalar  => Api.Kind.Type
      case Kind.Field   => Api.Kind.Val
      case _            => Unset

    val words: Text = (declaration.modifiers + List(declaration.kind.keyword)).join(t" ")
    Inline.Icon(analogue.let(Icons.markup(_, declaration.modifiers)).or(t""), words)

  override def glyphs(entity: Api.Entity, exported: Boolean = false): List[Inline] =
    entity.definition.or(entity.template).let(glyph(_, exported)).let(List(_)).or(Nil)

  // A document is written `document`, the schema's name being the schema's.
  override def signature(name: Text, declaration: Api.Declaration, api: Api, scope: Scope)
  :   Block =

    if declaration.kind != Kind.Document then super.signature(name, declaration, api, scope)
    else
      val body: List[Block.Line] =
        declaration.signature.let(_.body).or(Nil).map: (line: List[Api.Phrase]) =>
          Block.Line(Token.plain(t"  ") :: ApiPages.phrases(api, line))

      Block.Code(language, Block.Line(List(Token(t"document", Token.Accent.Keyword))) :: body)

  // A schema is documented by its overlay's own `info` and `detail`, as a package is.
  override def documentable(entity: Api.Entity): Boolean = entity.parent.present

  // The line each definition, and each member of one, is declared on, found by reading the
  // file's lines: a definition's is the unindented line which declares it, and a member's the
  // first line beneath it, one level in, which declares a field or a variant of its keyword;
  // the document's fields are those beneath `document`.
  private class Lines(text: Text):
    private val definitions: scala.collection.mutable.HashMap[Text, Int] =
      scala.collection.mutable.HashMap()

    private val members: scala.collection.mutable.HashMap[(Text, Text), Int] =
      scala.collection.mutable.HashMap()

    locally:
      var owner: Optional[Text] = Unset

      text.cut(t"\n").indexed.each: (line: Text, index: Ordinal) =>
        val words: List[Text] = line.trim.cut(t" ").filter(_ != t"")
        val keyword: Text = words.head.or(t"")
        val name: Text = words.skip(1).head.or(t"")

        if !line.starts(t" ") then
          keyword match
            case t"record" | t"select" | t"scalar" =>
              owner = name
              if !definitions.contains(name) then definitions(name) = index.n1

            case t"document" =>
              owner = t""
              if !definitions.contains(t"") then definitions(t"") = index.n1

            case t"" =>
              ()

            case _ =>
              owner = Unset

        else if line.starts(t"  ") && !line.starts(t"   ") then
          owner.let: (owner: Text) =>
            if (keyword == t"field" || keyword == t"variant") && !members.contains((owner, name))
            then members((owner, name)) = index.n1

    def definition(name: Text): Optional[Int] = definitions.get(name).getOrElse(Unset)
    def member(owner: Text, name: Text): Optional[Int] = members.get((owner, name)).getOrElse(Unset)

  // What a description says, as documentation: its first sentence — of its first paragraph, as
  // one line — and the whole of it, where it says more than that.
  def described(description: Optional[Text], path: Text, line: Optional[Int]): Optional[Api.Entry] =
    description.let(_.trim).let: (text: Text) =>
      if text == t"" then Unset else
        val first: Text = text.cut(t"\n\n") match
          case first :: _ => first
          case _          => text

        val paragraph: Text = first.cut(t"\n").map(_.trim).filter(_ != t"").join(t" ")

        val end: Int = paragraph.s.indexOf(". ")
        val sentence: Text = if end < 0 then paragraph else paragraph.keep(end + 1)
        val whole: Boolean = sentence == paragraph && paragraph == text
        val detail: Optional[Text] = if whole then Unset else text
        Api.Entry(info = sentence, detail = detail, file = path, line = line)

  // The faults the schema of schemas finds, as they are accrued, each with its place.
  private case class Faults(items: List[(Tel.Error, Span)] = Nil)
  extends Error(m"the schema has ${items.size} faults"):
    def +(error: Tel.Error, span: Span): Faults = Faults(items :+ (error, span))

  // A schema read from its file: its API, or what is wrong with it.
  def read(path: Text, text: Text): (Optional[Api], List[Block]) =
    import strategies.throwUnsafely
    import parsing.trackPositions

    try
      val tel: Tel = text.read[Tel]

      val faults: Faults =
        validate[Tel.Focus](Faults()):
          case error: Tel.Error => accrual + (error, prior.lay(Span.empty)(_.span))

        . protect(Tel.Type.assign(tel, Tels.Axiom.tels))

      if !faults.items.nil then
        val problems: List[Block] = faults.items.map: (error, span) =>
          Block.Diagnostic.of(error, path, text, if span.exists then span else Unset)

        val none: Optional[Api] = Unset
        (none, problems)
      else
        val api: Api = model(path, text, Tels.Validation.validate(tel.as[Tels]))
        val none: List[Block] = Nil
        (api, none)

    catch case error: Tel.Error =>
      val problem: List[Block] = List(Block.Diagnostic.of(error, path, text))
      (Unset, problem)

  // The types every schema has, which no schema declares.
  private val builtins: List[Text] =
    List(t"String", t"Identifier", t"TypeName", t"Sigil", t"Flag")

  // A schema as an API.
  def model(path: Text, text: Text, schema: Tels): Api =
    val lines: Lines = Lines(text)
    val root: Locator = Locator(Unset, schema.name)
    val scalars: List[Tels.ScalarDefinition] =
      schema.scalars.to[List].filter { (scalar: Tels.ScalarDefinition) => !builtins.has(scalar.name) }
    val records: List[Tels.RecordDefinition] = schema.records.to[List]
    val selects: List[Tels.SelectDefinition] = schema.selects.to[List]
    val named: List[Text] = records.map(_.name) + selects.map(_.name) + scalars.map(_.name)

    def keyOf(name: Text): Optional[Text] = if named.has(name) then Api.key(root, name) else Unset
    def where(line: Optional[Int]): Optional[Api.Location] = line.let(Api.Location(path, _))
    def word(text: Text): Api.Phrase = Api.Phrase(text, Api.Phrase.Role.Modifier)
    def keyword(text: Text): Api.Phrase = Api.Phrase(text, Api.Phrase.Role.Keyword)
    def literal(text: Text): Api.Phrase = Api.Phrase(text, Api.Phrase.Role.Literal)

    def typed(tpe: Tels.Type): Api.Phrase = tpe match
      case Tels.Reference(name) => Api.Phrase(name, Api.Phrase.Role.Type, keyOf(name))
      case Tels.Flag            => Api.Phrase(t"Flag", Api.Phrase.Role.Type)
      case Tels.Scalar(_, _, _) => Api.Phrase(t"String", Api.Phrase.Role.Type)
      case Tels.Struct(_, _)    => Api.Phrase(t"{…}", Api.Phrase.Role.Symbol)

    def shape(tpe: Tels.Type): Optional[Api.Shape] = tpe match
      case Tels.Reference(name) => keyOf(name).let(Api.Shape.Named(_))
      case _                    => Unset

    def polarities(required: Tels.Polarity, repeatable: Tels.Polarity): List[Text] =
      val first: List[Text] = required match
        case Tels.Polarity.Loose => List(t"optional")
        case Tels.Polarity.Tight => List(t"required")
        case _                   => Nil

      val second: List[Text] = repeatable match
        case Tels.Polarity.Loose => List(t"repeatable")
        case Tels.Polarity.Tight => List(t"irrepeatable")
        case _                   => Nil

      first + second

    // The members of a struct, under the type form of what has them: an entity for each field,
    // and a line of the owner's signature — with the select's key — for each select it says.
    def members(parent: Locator, owner: Text, members: List[Tels.Member])
    :   (List[Api.Entity], List[List[Api.Phrase]], List[Text]) =

      val fields: List[Api.Entity] = members.bind:
        case field: Tels.Field =>
          val said: List[Text] =
            polarities(field.required, field.repeatable) + (if field.key then List(t"key") else Nil)

          val default: List[Api.Phrase] =
            field.default.lay(Nil: List[Api.Phrase]): (value: Text) =>
              List(keyword(t"default"), literal(value))

          val declaration: Api.Declaration =
            Api.Declaration
              ( Kind.Field,
                said,
                returns   = shape(field.fieldType),
                signature = Api.Signature(typed(field.fieldType) :: said.map(word) + default) )

          val line: Optional[Int] = lines.member(owner, field.keyword)

          List:
            Api.Entity
              ( parent,
                field.keyword,
                true,
                declaration,
                location = where(line),
                doc      = described(field.description, path, line),
                format   = id )

        case _ =>
          Nil

      val said: List[(List[Api.Phrase], Optional[Text])] = members.bind:
        case select: Tels.SelectRef =>
          val named: Api.Phrase =
            Api.Phrase(select.reference, Api.Phrase.Role.Type, keyOf(select.reference))

          val words: List[Api.Phrase] =
            List(keyword(t"select"), named)
            + polarities(select.required, select.repeatable).map(word)

          List((words, keyOf(select.reference)))

        case _ =>
          Nil

      (fields, said.map(_(0)), said.bind(_(1).let(List(_)).or(Nil)))

    def validators(names: List[Text]): List[List[Api.Phrase]] =
      names.map { (name: Text) => List(keyword(t"validate"), literal(name)) }

    val recordEntities: List[Api.Entity] = records.bind: (record: Tels.RecordDefinition) =>
      val (fields, said, bases) =
        members(Locator(root, record.name, true), record.name, record.members.to[List])

      val body: List[List[Api.Phrase]] = said + validators(record.validators.to[List])
      val line: Optional[Int] = lines.definition(record.name)

      val entity: Api.Entity =
        Api.Entity
          ( root,
            record.name,
            true,
            template = Api.Declaration(Kind.Record, signature = Api.Signature(Nil, body)),
            members  = fields.map(_.key),
            location = where(line),
            doc      = described(record.description, path, line),
            bases    = bases,
            format   = id )

      entity :: fields

    val selectEntities: List[Api.Entity] = selects.bind: (select: Tels.SelectDefinition) =>
      val parent: Locator = Locator(root, select.name, true)

      val variants: List[Api.Entity] = select.variants.to[List].map: (variant: Tels.Variant) =>
        val line: Optional[Int] = lines.member(select.name, variant.keyword)

        val declaration: Api.Declaration =
          Api.Declaration
            ( Kind.Variant,
              returns   = shape(variant.variantType),
              signature = Api.Signature(List(typed(variant.variantType))) )

        Api.Entity
          ( parent,
            variant.keyword,
            true,
            declaration,
            location = where(line),
            doc      = described(variant.description, path, line),
            format   = id )

      val body: List[List[Api.Phrase]] = validators(select.validators.to[List])
      val line: Optional[Int] = lines.definition(select.name)

      val entity: Api.Entity =
        Api.Entity
          ( root,
            select.name,
            true,
            template = Api.Declaration(Kind.Select, signature = Api.Signature(Nil, body)),
            members  = variants.map(_.key),
            location = where(line),
            doc      = described(select.description, path, line),
            format   = id )

      entity :: variants

    val scalarEntities: List[Api.Entity] = scalars.map: (scalar: Tels.ScalarDefinition) =>
      val patterns: List[List[Api.Phrase]] = scalar.patterns.to[List].map: (pattern: Text) =>
        List(keyword(t"pattern"), literal(pattern))

      val encoding: List[List[Api.Phrase]] =
        scalar.encoding.lay(Nil: List[List[Api.Phrase]]): (encoding: Text) =>
          List(List(keyword(t"encoding"), literal(encoding)))

      val body: List[List[Api.Phrase]] =
        validators(scalar.validators.to[List]) + patterns + encoding
      val line: Optional[Int] = lines.definition(scalar.name)

      Api.Entity
        ( root,
          scalar.name,
          true,
          template = Api.Declaration(Kind.Scalar, signature = Api.Signature(Nil, body)),
          location = where(line),
          doc      = described(scalar.description, path, line),
          format   = id )

    val (fields, said, bases) = members(root.tpe, t"", schema.document.members.to[List])
    val definitions: List[Api.Entity] = recordEntities + selectEntities + scalarEntities
    val direct: List[Text] = definitions.filter(_.parent == root).map(_.key)

    val top: Api.Entity =
      Api.Entity
        ( Unset,
          schema.name,
          true,
          definition = Api.Declaration(Kind.Schema),
          template   = Api.Declaration(Kind.Document, signature = Api.Signature(Nil, said)),
          members    = direct + fields.map(_.key),
          location   = where(lines.definition(t"").or(1)),
          bases      = bases,
          format     = id )

    val all: List[Api.Entity] = top :: definitions + fields
    Api(sci.Map.from(all.stdlib.toList.map { (entity: Api.Entity) => (entity.key, entity) }), this)

  // Several schemas' APIs as one, in the order they were read; a schema named as an earlier
  // one is, is left out, and named for the caller to report.
  def combine(apis: List[Api]): (Api, List[Text]) =
    apis.stdlib.foldLeft((Api(sci.Map(), this), Nil: List[Text])): (state, api) =>
      val (whole, clashes) = state
      val repeated: List[Text] = api.roots.map(_.key).filter(whole.entities.contains(_))

      if repeated.nil then (whole.copy(entities = whole.entities ++ api.entities), clashes)
      else (whole, clashes + repeated)
