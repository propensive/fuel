# Fuel

__Fuel__ is the documentation tool of the [Soundness](https://soundness.dev/) ecosystem. It
compiles a project's Markdown documentation — with the code embedded in it highlighted, and in
time checked against the project's classpath and animated — into
[Pyrocosm](https://github.com/propensive/pyrocosm)'s semantic model, and serves it on the web,
with the project's API — its structure and signatures from the compiled code, its prose from
TEL overlays — as pages of the same site, one per type and term at a predictable URL, which
everything that names one links to. Its design and roadmap are
[`doc/design.md`](../doc/design.md).

Fuel runs as an [Ethereal](https://soundness.dev/ethereal/) daemon and is a Pyrocosm tool, so
the subcommands and configuration files every Pyrocosm tool shares are fuel's too.

## Usage

```sh
fuel compile       # compile the configured directories of Markdown documents
fuel compile --force  # the same, ignoring what the output directory keeps
fuel serve         # serve the compiled documents at http://localhost:8090/fuel/, in the background
fuel serve stop    # stop serving them
fuel show doc/x.md # one document, set in the terminal: justified, hyphenated, highlighted
fuel present doc/slides.tel  # a presentation's slides in a browser until Ctrl+C
fuel present doc/slides.tel --autoplay  # the same, played by itself to a schedule
fuel present doc/slides.tel --align talk.wav  # the same, timed to a recording of the transcript
fuel transcript doc/slides.tel  # the presentation's transcript, set to the terminal's width
fuel sync github propensive/fuel  # attach a repository's issues to the API pages, and fetch them
fuel sync          # fetch every attached tracker's issues again
fuel translate     # write the translations nobody has begun, into each configured language,
                   # and say how far each is done, and what is wrong with any
fuel translate doc/x.md doc/y.md  # the same, for these documents (or overlays) alone
fuel translate --language fr  # the same, into French alone
fuel translate --update  # bring each translation up to the text it translates
fuel install       # install tab-completions and the manpage
fuel about         # fuel's version, and the daemon serving it
fuel --version     # the version alone
fuel quit          # stop the daemon, and any server it runs
```

A failure of the daemon itself while it carries out a command (running out of memory, say) is
reported on the standard error, where it was thrown, and the command exits with status 6, apart
from a compile's own failures (status 4), from a tracker whose issues could not be fetched
(status 7), from a presentation whose transcript and alignment do not correspond (status 8),
from an aligner that could not be installed or run (status 9), from a translation which is not
consistent with the text it translates (status 10, from `fuel translate`) and from a command
line it did not understand (status 2).

`fuel compile` reads every `*.md` beneath each configured `document` directory, parses it as
CommonMark, and converts it to Pyrocosm blocks: a fenced block whose info string is `scala` or
`java` is highlighted through [Harlequin](https://soundness.dev/harlequin/); any other fence is
shown plain under its own language name. The result is kept in the daemon, and `fuel serve` (or
a bare `serve` in the configuration) shows it: every section's documents, by title, beside the
chosen document. A document's title is its first level-one heading, else its filename.

When a `classpath` is configured, every Scala fence is also *compiled*, in one warm compiler
session on that classpath with the configured `scalac-option`s, so a fragment that does not
typecheck fails the compile (exit status 4). A macro the fragments call is run against that
classpath, never fuel's own copy of a library it uses (Proscala's `-Zisolated-macros`). Every problem — a fragment that does not typecheck,
a preamble that is not TEL, a file that cannot be read — is a Pyrocosm diagnostic: `fuel
compile` prints it as the terminal renders one (`path:line:col: error`, the message, and the
lines concerned numbered from the file, with the place underlined), and in the served document
it follows the fragment, whose lines are numbered from the file too. Every link is checked
too, once every document is compiled: a link to another document (`guide/x.md`, `../x.md`,
resolved against the document's directory as the files on disk relate, so that
`../philosophy/x.md` from `doc/modules` leads into the section `doc/philosophy`) or to a place
on one (`#set-up`, a heading's slug as GitHub makes it, or a fence's `id`), to an API page
(`/api/gossamer.Text#length`) or a glossary term (`/glossary#interface`) must have a target, or
it is a failure at its line; a link to the web is fetched only when `check-links` is set. A
fence marked
` ```scala fuel ` begins with a [TEL](https://soundness.dev/tel/) preamble, ended by a line
holding `##`:

```
context term            # top (the default), term or type: how the fragment is wrapped
id setup                # a name another fragment may follow
follows setup           # the fragments whose text precedes this one when it compiles
errors show             # fail (the default), show (errors are expected and shown), ignore
language java           # what the fragment is highlighted as: the fence's language unless said
highlight Mi..es  why   # a range, by its first and last characters or literally, and a caption
error  oops
  caption  or beneath it
  line 3                # the line of the fragment to look on, and which occurrence, when the
  occurrence 2          # range occurs more than once
frame                   # a frame of an animation: its code, which must be there, and may be
  code                  # on the line for a one-liner; a label (Markdown phrasing); the seconds
      val x = 1         # it rests when played; its time on the page's clock; its own
  label the *first*     # language; and marks of its own, as above
  hold 2
  at 12.5
  language java
  highlight x  why
restart                 # in a session, a fresh one begins at this fence
typecheck               # how far the compiler goes: parse, typecheck (the default) or compile;
feature experimental.captureChecking   # a language import from this fence on
run                     # compile the fence and run it: what it prints is shown beneath it
path config.json        # the fence is this file's content, named above it; a `run` fragment
                        # reads it by that relative path
substitute Lit..ond  ...   # a range shown as other text, in italics, once the code compiled
focus Mi..es            # a mark which fades everything else in the block
alternative Bash        # a version of the code, under a tab: its `code` or `frame`s, and a
  code                  # `language`, marks and substitutes of its own; the first is the
      export X=1        # fence's own, which joins a session
```

`caution`, `param` and `focus` mark ranges as `highlight` and `error` do, `focus` by fading
everything else in the block, and in an animation a focus comes and goes with its frame's other
marks. A comment in any fence is set in italics, in its own colour. A `substitute` replaces a
range of what is shown — with `...`, say, where code is elided — in italics, while the code
compiles as written. A fence with a `path` is the content of that file, named above the code;
every such file of a document is written to a directory of the document's own, which a
fragment marked `run` resolves a relative path against, so that `t"config.json"` reads it. A
fence with `alternative`s shows one version at a time, under tabs above the code — Bash, Zsh
and Fish, or braces and whitespace — each with its own code or frames, compiled as the fence
is; the first is the fence's own, which a session takes up. Repeated `frame`s make the
fence an *animation*: one block that steps through the frames, each token keeping its identity
across them so that a step moves what stays and fades what arrives and departs, with each
frame's marks fading out before the step and in after it; `autoplay` plays it when shown, and
`loop` starts it again at the end. A frame with an `at` puts the block on the page's one clock,
which a bar at the foot of the page plays. Each Scala frame is compiled on its own; a Java frame
is highlighted. With `session` configured, every fence follows the ones before it, so a
tutorial reads as one REPL session; a fence which names a mode is checked but, to a checker
which runs fences, not run; and a line ending `// does not compile` is compiled alone and must
fail. A fence which says `run` is compiled through every phase and run, as Flame runs a line,
and what it writes to the standard streams is shown beneath it as a terminal would show it,
styled, a colour it leaves to the terminal being the reader's own; what it throws is an error,
as `errors` says. Its output is captured once, with the fence, and changes only when the fence
does, or with `--force`. [`doc/markdown.md`](../doc/markdown.md) shows each.

A fence marked ` ```tabular ` is a table, written in TEL to the schema `tabular`: `column`s, each
with a `title` and perhaps `align start` or `align end`; `row`s, each with a repeatable `cell`;
and perhaps a `caption`. A field may follow its keyword on one line, after two spaces, so a
row's cells follow `row`, and an empty cell is a `cell` alone beneath it. Titles, cells and the
caption are Markdown phrasing; a column of numbers is set at the end. Anything the schema does
not allow fails the compile at its place:

````
```tabular
column  Planet
column  Moons
column  Notes
row  Mercury  0  Closest to the Sun
row  Earth  1  Home
caption  The *inner* planets
```
````

## Presentations

`fuel present <file>` shows a presentation in a browser, at `http://localhost:8061/` (or the
`present-port`), until Ctrl+C: the slides alone, filling the window, each a 16:9 canvas scaled to
the screen. The arrow, page and space keys step through the slides, an animation's frames first;
`f` toggles full screen. A presentation is a TEL document of `slide`s, each holding Markdown or
a block of code ([`doc/slides.tel`](../doc/slides.tel) is an example):

```
title  Effects, gently

slide
  markdown
      # Effects, gently
      A *short* talk.

slide
  syntax scala
      val x = 1
  transcript
      One value, to start with.

slide
  syntax scala
    label      a second name
    highlight  y  new
    code
        val x = 1
        val y = x + 1
```

A slide's `syntax` names the language its code is highlighted as, and is either the code alone,
or what a fence's preamble says — marks, `context`, `errors`, `parse`/`typecheck`/`compile`,
`frame`s, `autoplay`, `loop` and the rest — with the code itself beneath `code`, as a `frame`'s
is. A presentation is checked against its TEL schema, `presentation`
(`src/core/resources/fuel/presentation.schema.tel`), and each fault is reported at its line.
Consecutive slides which each hold one block of code
without frames of its own, in the same language, are one animated block, its frames those
slides, each with the slide's marks and its `label`, `hold` and `at`: stepping from one to the
next moves what stays and fades what arrives and departs, as a fence's `frame`s do. A slide
whose code has `frame`s of its own is an animation alone. Code is compiled as `fuel show`
compiles a document, against the configured classpath where there is one, and what it reports is
printed before the slides are served.

`fuel present <file> --autoplay` plays the presentation by itself, to a schedule: a time, from the
page's opening, at which each step lands — the frame an animation steps to fully arrived, or the
next slide fully in. A step is started as long before its time as it will take: an animation's
step by its own measure (its fades and the move of what stays), and a change of slide, which
fades the slide out over 0.4 s, shows nothing for 0.4 s, and fades the next in over 0.4 s, by
that 1.2 s. The schedule is a stand-in for now, a step every four seconds, and the command says
how many steps the deck has; the timings will be read from a file alongside the presentation
later. Pressing a key takes the deck back from the schedule.

The schedule can instead come from a recording of the transcript being read:
`fuel present <file> --align talk.wav` aligns the recording against the transcript
(`fuel transcript <file>` prints it, to read from) with
[Montreal Forced Aligner](https://montreal-forced-aligner.readthedocs.io/), and each slide after
the first is shown at the moment the first word of its transcript is spoken — or of the first
word of it the aligner caught. `--align` also takes an alignment already made, in any of MFA's
formats — a TextGrid (its default), JSON or CSV — telling a recording from an alignment by its
extension or its first bytes. Every slide must have a transcript, and the transcripts and the
alignment must correspond: a word the aligner did not know (`<unk>`) stands for any one word,
but if more than one word in ten is in the one and not the other, the presentation is not
shown, and the differences are printed as a word diff (`-word` for a word of the transcript not
spoken, `+word` for one spoken that the transcript lacks), with status 8; fewer differences are
shown the same way, and the presentation goes ahead. `--align` implies `--autoplay`.

MFA is published only as conda-forge packages, so fuel installs it itself the first time a
recording is aligned, into `$XDG_CACHE_HOME/fuel/mfa` (`~/.cache/fuel/mfa`): it fetches
micromamba (a single static executable which installs conda-forge packages into a directory,
with no Anaconda, no shell set-up and no activation), makes an environment there with MFA 3.4
(about 2 GB, a few minutes), and downloads the `english_mfa` dictionary and acoustic model.
Nothing else on the machine is touched; `rm -rf ~/.cache/fuel/mfa` removes it all. An alignment
is kept in the project's cache by a digest of the recording and the transcript, so a
presentation is aligned again only when either changes; MFA itself takes tens of seconds to
start and align. If fuel cannot install MFA — there is no micromamba for the platform, or a
download fails — it says so and prints the three commands to do it by hand (`brew install
micromamba`, `micromamba create -p ~/.cache/fuel/mfa/env -c conda-forge
"montreal-forced-aligner>=3.4,<4"`, `mfa model download …`), and exits with status 9. Record in
one take, as WAV or FLAC for choice (MFA's own sox and ffmpeg convert the other formats); a
whole talk is aligned as one utterance, which MFA manages but is its weakest case, so if it
finds no alignment fuel tries once more with wider beams.

A slide may have a `transcript`: what is said while it is shown. `fuel transcript <file>` prints
the presentation's transcript — each slide's, in order, a blank line dividing paragraphs — set
to the terminal's width without breaking a word across lines, with what the reading of the
presentation reports after it.

## Output

With an `output` directory configured, `fuel compile` writes everything it compiled there, as
BinTEL under the schema derived from each record's type, and compiles again only what changed:

```
dist/doc/
  index.fuel          # the manifest
  objects/<hash>.fuel # every record, named by the Blake3 hash of its bytes
```

The manifest (`Output.Manifest`) records Fuel's version, the hash of the classpath (of its
bytes, until a hash of its API is available), the hash of every input the whole output depends
on, each document by its section (its `document` directory relative to the project root) and its
path, each API page by its entity, and the cache: from the key of each input to the record which
holds its output. Naming records by their content means names never collide, whatever a
document or an entity is called, and a record already written is not written again; the manifest
is written last, by a move, so a reader sees one whole manifest or the last one, and a record no
longer named is deleted.

A document is compiled in parts, each kept by a key of its own inputs:

- its **prose**, with a sentinel where each fence is, keyed by that prose and by the imports its
  fences make, which its references resolve through;
- each **fence**, keyed by its info string and content, by everything the compile depends on (the
  classpath, the project's sources, the compiler's options, the `imports` setting, the entities
  the API has) and by the document's path; in a `session`, chained from the fence before it (and
  the fixture), since a fragment sees what came before it — so changing a fence compiles it and
  those after it in its document, and nothing before it; outside a session, a fence depends only
  on what it `follows`.

A fence's line is not part of its key: its output moves with it, so prose can change around it
without compiling it again. Each API page is keyed the same way, by the entity and the members
its page lists. `--force` compiles everything again. A compile prints how many records it kept
and how many it compiled.

## API documentation

With `source` directories configured, `fuel compile` compiles the project's own sources in the
same warm session (against the `classpath`, which is the project's dependencies), and builds a
model of all the code — the project's and its dependencies' — from the compiled output and the
dependency jars: every package, type and term, with the signatures of its declarations. A type
and its companion object are one *entity* and one page, served at `/api/<name>`: a member of a
term follows `.` and a member of a type follows `~` (`#` being a URL's fragment), so
`gossamer.Text`'s `length` is at `/api/gossamer.Text~length`, and a member's row on its parent's
page is the anchor `/api/gossamer.Text#length`. A dependency's page is rendered when it is asked
for; a project's own pages are rendered by the compile, with the examples in them compiled.
An overloaded name is one page, since each of its variants means the same thing, and a type and
its companion object are one page, which can speak of "the type" and "the companion object".
A glyph of the member's keyword — the icons of Amok, drawn on their grid — precedes its name in
every table and in its page's title, transformed by its modifiers: a full stop follows a `final`
one, an `inline` one is sheared, a `transparent` one striped.
A parameter is an entity too, a member of the type form of what it parameterises: in package
`quux`, `def foo[T](bar: Baz)` is the page `quux.foo`, and its parameters are `quux.foo#bar` and
`quux.foo#T`, listed in a table of their own on its page; a class's constructor parameters and
type parameters are members of the class (`quux.Box#T`), where a field of the same name — a case
class's — is the parameter's entity.

An **overlay** documents a package, in a TEL file beneath an `overlay` directory: Amok's format.

```
tel 1.0

base gossamer                        # the package this file documents
import soundness                     # scope for references in this file's prose (repeated)
info a one-line summary of the package
detail                               # Markdown, as an indented source atom
    All about _gossamer_.

entry .Text   a one-line summary     # `.name` is a member of the term, `#Name` of the type
  detail
      Markdown about `Text`, with examples in fences, which are compiled.
  import gossamer.*                  # a scope of this entry's own, inherited by those within
  entry #length   its length          # a leaf entry's Markdown may follow it as a source atom
  entry .apply    makes one
    entry #text   the characters       # a parameter, a member of `apply`'s type form
  entry #whisper  gone
    until 0.74.0  gossamer.Text#shout # removed in that version; its replacement, if any
  entry .Internal
    hidden                           # not listed, and its members not asked for
  refer gossamer.Textual             # "See also"
  keyword string                     # found by a search for `string`; shown nowhere
  keyword character sequence
```

An overlay is held to its format, each failure at its line: a keyword other than those above
(`base` only at the top), `base`, `info`, `detail`, `hidden` or `until` given twice, an entry's
info given both on its line and as `info` (or its detail both as a source atom and as `detail`),
two entries for the same member, an `entry` without a name, and an `info` longer than 140
characters as it reads (Markdown's markup not counted): an `info` is a line, and the rest belongs
in the `detail`.

The overlay and the code are held to each other: an entry naming a member nothing declares
fails the compile (unless it is marked `until`, when it keeps a page saying what replaced it),
an `until` on a member which still exists is a caution, and a public member of a documented
package with no entry is a caution at the line it is declared, naming the entry to write. A
member which overrides or implements an inherited one is documented by it, and a parameter need
not be documented at all.

A type's page lists what applies to it beyond its own members. Its **extensions** are every
extension method whose receiver the type satisfies — one declared on the type itself, on a base
of it, on a type parameter bounded by one of them, or on a type parameter with a context bound
(or a `using` clause) which some given *for this type* satisfies — ordered from the closest to
the most general: those in the type's companion (or a base's) first, which need no import, then
those in its own package, then those elsewhere; those on a base after those on the type; those
reached through a given after those reached by the receiver alone, nearer givens first. Each
run of one source is a table headed by what brings it: the companion, the `import` to write, the
given which enables it. An extension whose context bound no given satisfies is not listed, and
an unbounded extension (`extension [T](value: T)`, or one enabled only by a given for every
type) is listed once, on the page of `scala.Any`, which every page with extensions points to.
Its **givens** are the instances *of* it, for a type class, and the givens *for* it, for a
type. Both are found in one pass over the whole model once the project and its dependencies are
merged (`Applicables`), so an extension in one library extends a type in another, and the
modularity form `T is Typeclass` is read as `Typeclass[T]`.

Everything that names an entity links to its page, when the page exists: the identifiers in a
signature; inline code in prose (`` `Text` ``), resolved as a name in code would be — as
written, then under the page's own entity and those enclosing it, then under the `import`s of
the overlay (or, for a Markdown document, the `imports` setting and the imports its own fences
make), then `scala`, `scala.Predef` and `java.lang` — a name which several entities answer to
in the same scope is a caution and stays plain. A name is followed through an export in its
prefix (`soundness.Text.s` is the `s` of the `anticipation.internal.Text` it exports), a member
of a type after `#` is found where the type inherits it (`#List#toList`) or among the
extensions which apply to the type and reach it — from its companion or an object enclosing it,
or from what is imported —
(`#Angle#principal`), and a bare name, last, among the extensions in reach (`.stdlib`, an
ambiguity when several receivers have one); and the typed tokens of a compiled fence, when
the token is the name of what its type names. A sigil says what is meant: `` `#Name` `` is a
type (or a member of one), `` `.name` `` a term (or a member of one), and the sigil is not shown
once it resolves — on an entity's page, Amok's `` `#member` `` and `` `.member` `` name a member
of the page's own entity, which is the same rule in the page's own scope. A sigilled reference
nothing resolves is a failure (a caution when no API is configured to resolve it against), and
is read only before a name as code would write one, so `` `.pyrocosm/fuel/config.tel` `` is
code. In a Markdown document, a name a fence defines at its top level — `val greeting`, `class
Shape` — is in scope from then on, ahead of the API: `` `greeting` `` links to the fence which
defined it last, by its `id` (`fragment-3` when the preamble gives none). A qualified name — in
a signature, in inline code, or in a fence — is a link for each segment, and pointing at one
underlines the name up to it, as on Amok's pages: `com`, then `com.example`, then
`com.example.Foo`.

### Other formats

An API need not be Scala's. A `schema` setting names a [TEL](https://soundness.dev/tel/) schema
(repeated, for several), which is documented as the code is: by overlays, on pages of the same
layout, found by the same search. Each *format* has an `id` — `scala`, which need not be said,
and `tels` — which begins the address of its entities and the path of their pages, so that a
schema's are apart from a package of the same name: the record `Slide` of the schema
`presentation` is `tels/presentation.Slide`, at `/api/tels/presentation.Slide`. A schema is the
top-level name; its records, selects and scalars are the members of its term form
(`presentation.Slide`), and the fields of its `document` those of its type form
(`presentation#title`, at `/api/tels/presentation~title`); a record's fields and a select's
variants are members of the type form of theirs (`presentation.Slide#transcript`,
`presentation.Part#markdown`). A member's signature is the words which declare it — a field's
type, and whether it is `optional`, `repeatable` or the `key` — and a record's page says the
`select`s it holds, whose variants a reference finds through it (`Slide#markdown`). There is no
hierarchy to draw, and nothing extends a record. The glyphs are, for now, those of the Scala
keywords most like each kind: a schema's is a package's, a record's a class's, a select's an
enum's, a variant's a case's, a scalar's a type's and a field's a value's; beside a member's,
a small `?` says it is optional, a `+` that it is said once or more, a `*` that it is both, and
a small key that it is the key.

An overlay says the format it documents, with `format` before its `base`; without one, it
documents Scala:

```
tel 1.0

format tels
base presentation                    # the schema this file documents
info the format of a presentation

entry #title   the presentation's title        # a field of the document
entry .Slide                                   # a record
  entry #transcript   what is said while the slide is shown
```

What a schema says of a definition, a field or a variant in its `description` documents it
where no overlay does — its first sentence as the `info`, and the whole as the `detail` when
there is more — so a member is cautioned as undocumented only when it has neither. A schema is
held to the schema of schemas, each fault at its line, and to the rules every schema is; one
which fails contributes nothing. In prose, a reference names another format's entity by its
address (`` `tels/presentation.Slide` ``), or as written where nothing of its own format
answers to the name; an `imports` of `tels/presentation` puts the schema's members in scope.
A search knows a schema's words — `record`, `select`, `scalar`, `field`, `variant`, `optional`,
`repeatable`, `key` — beside Scala's, and `+Slide` finds the fields which hold a `Slide`.

A format is a `Format` (`fuel-api`): its glyphs, its signatures, the sections of a page which
are its own, and its words in a search. An entity is named by a `Locator`, which is no
format's own: Scala's `Designator` converts to one, and unlike it a locator's top-level name
has a type form as well as a term's.

## Glossary

A `glossary` is a TEL file of terms (several may be configured, and are combined), each with a
Markdown `definition` and, when a page explains it in full, a `link` to that page, on the site
(`/doc/guide/interfaces.md`) or elsewhere:

```
tel 1.0

define interface
  link  /doc/guide/interfaces.md
  keyword API                        # found by a search for `API`; shown nowhere
  definition
      A shared boundary, connection, or point of interaction where two or more separate
      systems, devices, or people meet and exchange information.
```

A term in brackets in prose — `[interface]`, in any case, or `[interfaces][interface]` for an
inflected form — shows its definition when pointed at, and leads to its link, or to the term's
entry on the glossary page at `/glossary`, which lists every term and is in the navigation. The
brackets are CommonMark's own reference link, since the glossary's definitions are appended to
each document before it is parsed, so an author's own `[interface]: url` wins; a term defined
twice is a failure, as is a fence left open (which would swallow the definitions). The page
lists its terms in the order of the dictionary of the language it is read in.

A glossary named beneath a `document`, by its path within the document's directory, is
translated as the document is: see [Translations](#translations).

## Translations

A document or an overlay is written in one language, its *definitive* text, and may be
translated into others. A translation is a file of its own which holds the text it translates
beside the translation of it, so that it is consistent by itself, and can be held to the
definitive text as that changes. A line of it is one of four kinds, by how it begins:

```
- The definitive text, a line of it.
+ Its translation.
! The definitive text as it was when the translation was written.
A line both languages share.
```

A *passage* is a run of `!` lines, then `-` lines, then `+` lines: its `+` lines translate its
`-` lines together, so a paragraph may be reflowed. A sigil alone is an empty line of its kind.
A line both languages share never begins with a sigil and a space, so one which would — an item
of a Markdown list which is the same in both languages — is a passage whose `+` line repeats
its `-` line. Taking the `-` lines with the shared lines gives the definitive text back, and
taking the `+` lines with them gives the translation.

````
- # Tutorial
+ # Tutoriel

- Fuel compiles every Markdown file,
- and serves it on the web.
+ Fuel compile chaque fichier Markdown, et le sert sur le web.

- - first, write a document
+ - d'abord, écrivez un document

```scala
val x = 1
```
````

A language is written as its ISO 639 code, perhaps with a region: `fr`, `pt-BR`. Where
translations are is said by the configuration alone, and by no document or overlay. A
`document` says the `language` it is written in, and holds a `translation` for each language
it is translated into: the language, and the directory which mirrors the document's. The
translation of a file is the file of the same relative path there. An `overlay` beneath a
`document` is a directory inside the document's, so it is translated with it.

```
document doc/en
  language en-US
  overlay api                   # `doc/en/api`
  translation fr     doc/fr     # `doc/en/prose/intro.md` is translated by
                                # `doc/fr/prose/intro.md`, and `doc/en/api/gossamer.tel`
                                # by `doc/fr/api/gossamer.tel`
  translation fr-CA  doc/fr-CA
    fallback fr                 # where `fr-CA` has no file, the French one stands for it
```

A `translation` always names its directory, which nothing is assumed about: it is resolved
as the document's is, and need not be beside it. A language with a `fallback` need not
translate everything: a file it has no translation of is read in its fallback (and that
language's own fallback, in turn), and as it is written where none of them has one. So such a
language's missing translations are no caution, and `fuel translate` writes them only when it
is asked for the language by name, with `--language`. A fallback is another language the
document is translated into, or the language it is written in. There may be several
`document`s, each with its own language, overlays and translations.

A file in a directory of translations is not a document or an overlay of its own, even where
that directory is inside a directory of documents. These are failures of the configuration:
a `translation` which is not beneath a `document`, or is not a language and a directory; a
translated `document` which does not say its `language`, or is translated into it; and a
fallback which is neither that language nor another translation of the document.

An overlay's translation is an overlay with the same entries, which TEL reads as any other,
whose prose alone is paired: each `info` and `detail` is written beneath its keyword, as a
source atom of passages. An `info` the definitive overlay gives on an entry's line is an `info`
beneath the entry in its translation. An entry's `hidden`, `until`, `refer` and `import` are
the same in every language.

```
base gossamer

entry .Text
  info
      - a one-line summary
      + un résumé d'une ligne
  detail
      - Markdown about `Text`.
      + Du Markdown sur `Text`.
```

A glossary's translation is a glossary of the same terms, each named on its `define` line as
the glossary it translates names it, so the file itself says which term is which. Beneath the
name, a `term` says what the term is in its own language, and the `definition` is a source
atom of passages:

```
define library
  term bibliothèque
  keyword bib
  definition
      - A collection of code that other programs use.
      + Un ensemble de code que d'autres programmes utilisent.
```

An entry with no `term` is one nobody has translated, and reads as the definitive term. Its
`keyword`s are its own language's, in place of the definitive term's. Its `link` is the
definitive term's, since a page of the site is the same page in every language; a `link` is
written only for a page elsewhere which another explains in this language.

A translated document names a term as its own language does, `[bibliothèque]`, and may name
it as the glossary defines it too, `[library]`, as a passage nobody has translated does;
where one name is two terms, it is this language's. The term's anchor on the glossary page is
the definitive term's in every language, `/glossary#library`.

`fuel compile` holds every translation to its definitive text, and `fuel translate`
does so alone, without compiling anything, with a line for each translation saying how far it
is done. Each of these is reported at its line of the translation's file:

- the `-` lines are not the definitive text: a caution, with the change as it reads
  (`every Markdown {+and TEL+} file`, `[-gone-]`);
- a passage nobody has translated (it reads as the definitive text until someone does): one
  caution for the file, listing their lines;
- a passage which still has `!` lines, or has no `-` lines: a caution;
- `+` lines which translate nothing: a failure;
- a translation whose structure is not the definitive text's — a block of another kind, a
  heading of another level, a list of more or fewer items, a block more or fewer: a failure. A
  fence must be the same in every language, apart from a `tabular` fence, whose content is
  prose. A paragraph which names different inline code, or links to different places, is a
  caution. Structure is checked once no passage is waiting to be revised;
- for an overlay, an entry or a run of prose which one of the two has and the other lacks: a
  caution; an entry which differs in more than its prose: a failure;
- for a glossary, a term which one of the two has and the other lacks: a caution; an entry
  with no `term`: counted with the passages nobody has translated; a `term` which holds a
  bracket, or which two entries share: a failure; a `term` which is the name of another term
  as the glossary defines it: a caution, since `[term]` then means this one in this language;
  a `link` in place of one to a page of the site: a caution.

A document, an overlay or a glossary with no file where its translation should be is a caution, unless
the language has a fallback, and
`fuel translate` reports a file in a directory of translations which translates nothing.

`fuel translate` writes each translation nobody has begun — every line of prose a `-` line,
fences and blank lines shared — in its place, making the directories it needs; it never
overwrites one. It does so for every document, overlay and glossary, or for the files it is
given, and
into every language the configuration has a directory for, or into the one given with
`--language` (`-l`); then it checks them, as above. When the definitive text
changes, `fuel translate --update` (which takes files, translations among them, and
`--language`, in the same way)
rewrites each translation so that it is of the text as it stands, and shows the translator
what to do: a passage whose text changed keeps its `+` lines, takes the new text as its `-`
lines, and keeps the text its translation was written for as `!` lines.

```
! Fuel compiles every Markdown file,
! and serves it on the web.
- Fuel compiles every Markdown and TEL file,
- and serves it on the web.
+ Fuel compile chaque fichier Markdown, et le sert sur le web.
```

The translator revises the `+` lines and deletes the `!` lines. New text arrives as a passage
with no `+` lines, and text which has gone leaves its `!` and `+` lines, to be deleted. A
second update before the first is dealt with keeps the first `!` lines, which are what the
`+` lines still say. An overlay's translation is made again from the overlay, each run of its
prose updated in this way; the translation of an entry the overlay no longer has is left out,
and `fuel translate --update` says so. A glossary's translation is made again from the
glossary in the same way: each entry keeps its `term`, its `keyword`s and its `link`, a new
term takes the definitive term's keywords, and the translation of a term the glossary no
longer has — one which was renamed, too — is left out, and said to be.

The dashboard is read in a language. Where the documents are translated, its menu bar offers
the languages they are in, each by its two letters (in full, as `fr-CA`, where two share them),
and a reader's choice is kept for every tool of the hub. Without one, a reader has the
`language` of `~/.config/pyrocosm/config.tel`, then the first their browser asks for which the
documents are in, then the language they are written in.

- A document is shown as its translation reads: the language's own, or the first there is
  along its fallbacks. One which nothing translates is shown as it is written, under a note
  which says so. A translated heading keeps the anchor of the heading it translates, so a
  link to a place in a document means the same in every language.
- An entity's page says what the overlay's translation says of it, and a search's results
  with it. The pages of a translation are rendered when they are asked
  for, their examples highlighted and not compiled again: an example is the same in every
  language, and was compiled for the language it is written in.
- The glossary is the language's own: its page in the order of that language's dictionary,
  each term under its own name, and a search finds a term by its name in either language and
  by its own keywords. A language with no translation of a glossary, along its fallbacks,
  reads it as it is defined.
- Fuel's own words — the navigation's headings, a table's columns, a search's summary — are
  in English, French and German, and follow the language read: a regional variant reads as
  its language, and a language Fuel has no words for as English. Diagnostics are in English.

Presentations and a schema's descriptions are not yet translated, and the output
directory keeps the documents as they are written alone.

## Issues

A tracker's issues can be attached to the API's pages: `fuel sync github propensive/fuel`
attaches a GitHub repository (or a `sync github propensive/fuel` line of the configuration
does), fetches its open issues, and keeps them fresh while the dashboard is served. An issue is
attached to an entity only when its body says so, with the keyword `affects` followed by the
names in backticks, as `closes` names an issue from a pull request:

```
The result is off by one. This affects `gossamer.Text#length` and `gossamer.Text#skip`.
```

Names resolve as references in prose do, through the configured `imports`, an export followed
to its target; one that resolves to nothing, or to more than one entity, is reported by `fuel
sync` and by `fuel compile`, and attaches to nothing. On an affected entity's page, the issues
which remain open are listed after the title by number, each a link to the issue, and at the
foot with their titles. The attachment, and the issues fetched, are kept in the project's cache,
so a daemon started afresh shows them from its first `fuel compile`; `fuel sync` alone fetches
every attached tracker's again, and deleting the tracker's file beneath the cache's `sync`
directory detaches it.

While the dashboard is served, the daemon polls every ten minutes, asking GitHub only for what
changed since the latest change it saw, with the ETag of its last answer: a poll which finds
nothing new is answered `304 Not Modified`, which GitHub does not count against its rate limit,
and a change costs a request or two. Unauthenticated, GitHub allows sixty requests an hour; a
`github` credential (`GITHUB_TOKEN`, or a source declared in `~/.config/pyrocosm/credentials.tel`,
such as `command gh auth token`) lifts the limit, and is needed for a private repository. The
output directory never holds issues: they are put on a page as it is shown.

## Configuration files

A project configures fuel with a [TEL](https://soundness.dev/tel/) document at
`.pyrocosm/fuel/config.tel`, where the `.pyrocosm` directory sits in the project root (typically
alongside `.git`) and holds one subdirectory per tool; fuel finds the file from the current
directory or any ancestor. A user's own defaults go in `~/.config/fuel/config.tel`.

The schema is fuel's set of settings: each setting's name is a TEL keyword, a keyword's atom is
its value, a bare keyword means `true`, and a repeated keyword's atoms join with `:`. Relative
paths resolve against the project root.

```
tel 1.0

# The classpath embedded code is compiled against, and the compiler options it needs.
classpath out/core/assembly.dest/out.jar
scalac-option -deprecation

# The directories of Markdown documents, each a section named by its last segment.
document doc
document doc/modules

# Compile each document's fences as one session, each following the ones before it (as a
# REPL would), with `doc/fixtures/<section>/<document>.scala` (else
# `doc/fixtures/<document>.scala`) compiled first where there is one.
session
fixtures doc/fixtures

# The project's own sources, whose API is documented by the overlays beneath doc/api, and the
# packages a document's references resolve through.
source src/core
overlay doc/api
imports soundness

# The tracker whose open issues the API's pages show: read whole, as `serve` and `port` are.
sync github propensive/fuel

# A TEL schema, documented as the API is, by the overlays which say `format tels`.
schema etc/project.schema.tel

# The glossary's terms, which a document names in brackets; and whether a link to the web is
# fetched when the documents compile (a link within the site is always checked).
glossary doc/glossary.tel
check-links

# Write the compiled documents and API pages here, as BinTEL, compiling only what changed.
output dist/doc

# A directory of documents may say more: the language it is written in, the overlays and the
# glossaries inside it, and its translations, each a language and the directory which mirrors
# this one. A file without its translation there is a caution, and `fuel translate` writes
# it; a language with a fallback need not translate everything.
document doc/en
  language en-US
  overlay api
  glossary glossary.tel
  translation fr     doc/fr
  translation fr-CA  doc/fr-CA
    fallback fr

# Serve the documents from the daemon's start, behind the machine's Pyrocosm hub at
# http://localhost:8090/fuel/; `port` would serve them on a port of their own as well.
serve

# Offer the API's documentation to other tools on this machine — Flame shows an entity's
# `info` beside its completion — on `help-port` (8094 by default).
help
```

Each setting is also reachable as a flag (`--document doc`), a `fuel.`-prefixed system property
and a `FUEL_`-prefixed environment variable, in that order of priority ahead of the files.

## Modules

- `fuel-core` (`src/core`): `Doc`, `Corpus`, the `Blocks` conversion of Markdown to Pyrocosm
  blocks (recording every `Linkage` it makes), the `Glossary`, the `Embedding` of fenced
  code, for another tool to embed, and a `Translation` with the `Mirror` of its structure and
  the `Translations` which say where each is.
- `fuel-api` (`src/api`): the `Api` model of each `Format` (`Apis`), Scala's read from
  compiled code by `Inspection` and a TEL schema's by `TelsFormat`, documented by `Overlay`s,
  with the `Resolver` of references, `Signatures` as tokens, `ApiPages`, the `Links` check, and
  `OverlayTranslations`.
- `fuel-client` (`src/client`): the `Tool`, its subcommands and settings, the daemon's corpus
  and model, and the dashboard.
- `launcher` (`src/launcher`): the entry point alone, built into the executable.
- `test` (`src/test`): the suite, run by fume.

## Building

```sh
make run          # publish the libraries locally, assemble the launcher, run it
make fuel         # the native executable, through Burdock and xek
make test         # the suite, under the fume pinned in etc/tools
make test-plain   # the suite, through its plain-java entry point
make check        # flare, against Consequent Style
```

Dependencies are pinned in `etc/refs` and installed by `make sync-deps`; see `AGENTS.md` and
[propensive/.github](https://github.com/propensive/.github) for the release flow.

## License

Fuel is copyright &copy; 2026 Jon Pretty &amp; Propensive O&Uuml;, and is made available under
the [Apache 2.0 License](/LICENSE).
