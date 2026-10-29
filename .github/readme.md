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
fuel serve         # serve the compiled documents at http://localhost:8090/fuel/ until Ctrl+C
fuel show doc/x.md # one document, set in the terminal: justified, hyphenated, highlighted
fuel present doc/slides.tel  # a presentation's slides in a browser until Ctrl+C
fuel install       # install tab-completions and the manpage
fuel about         # fuel's version, and the daemon serving it
fuel --version     # the version alone
fuel quit          # stop the daemon, and any server it runs
```

`fuel compile` reads every `*.md` beneath each configured `document` directory, parses it as
CommonMark, and converts it to Pyrocosm blocks: a fenced block whose info string is `scala` or
`java` is highlighted through [Harlequin](https://soundness.dev/harlequin/); any other fence is
shown plain under its own language name. The result is kept in the daemon, and `fuel serve` (or
a bare `serve` in the configuration) shows it: every section's documents, by title, beside the
chosen document. A document's title is its first level-one heading, else its filename.

When a `classpath` is configured, every Scala fence is also *compiled*, in one warm compiler
session on that classpath with the configured `scalac-option`s, so a fragment that does not
typecheck fails the compile (exit status 4). Every problem — a fragment that does not typecheck,
a preamble that is not TEL, a file that cannot be read — is a Pyrocosm diagnostic: `fuel
compile` prints it as the terminal renders one (`path:line:col: error`, the message, and the
lines concerned numbered from the file, with the place underlined), and in the served document
it follows the fragment, whose lines are numbered from the file too. Every link is checked
too, once every document is compiled: a link to another document (`guide/x.md`, `../x.md`,
resolved against the document's directory within its section) or to a place on one
(`#set-up`, a heading's slug as GitHub makes it, or a fence's `id`), to an API page
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
```

`caution` and `param` mark ranges as `highlight` and `error` do. Repeated `frame`s make the
fence an *animation*: one block that steps through the frames, each token keeping its identity
across them so that a step moves what stays and fades what arrives and departs, with each
frame's marks fading out before the step and in after it; `autoplay` plays it when shown, and
`loop` starts it again at the end. A frame with an `at` puts the block on the page's one clock,
which a bar at the foot of the page plays. Each Scala frame is compiled on its own; a Java frame
is highlighted. With `session` configured, every fence follows the ones before it, so a
tutorial reads as one REPL session; a fence which names a mode is checked but, to a checker
which runs fences, not run; and a line ending `// does not compile` is compiled alone and must
fail. [`doc/markdown.md`](../doc/markdown.md) shows each.

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
  code scala
      val x = 1

slide
  code scala
    label      a second name
    highlight  y  new
    code
        val x = 1
        val y = x + 1
```

A slide's code is either the code alone, or what a fence's preamble says — marks, `context`,
`errors`, `parse`/`typecheck`/`compile`, `frame`s, `autoplay`, `loop` and the rest — with the code
itself beneath `code`, as a `frame`'s is. Consecutive slides which each hold one block of code
without frames of its own, in the same language, are one animated block, its frames those
slides, each with the slide's marks and its `label`, `hold` and `at`: stepping from one to the
next moves what stays and fades what arrives and departs, as a fence's `frame`s do. A slide
whose code has `frame`s of its own is an animation alone. Code is compiled as `fuel show`
compiles a document, against the configured classpath where there is one, and what it reports is
printed before the slides are served.

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
in the same scope is a caution and stays plain; and the typed tokens of a compiled fence, when
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

## Glossary

A `glossary` is a TEL file of terms (several may be configured, and are combined), each with a
Markdown `definition` and, when a page explains it in full, a `link` to that page, on the site
(`/doc/guide/interfaces.md`) or elsewhere:

```
tel 1.0

define interface
  link  /doc/guide/interfaces.md
  definition
      A shared boundary, connection, or point of interaction where two or more separate
      systems, devices, or people meet and exchange information.
```

A term in brackets in prose — `[interface]`, in any case, or `[interfaces][interface]` for an
inflected form — shows its definition when pointed at, and leads to its link, or to the term's
entry on the glossary page at `/glossary`, which lists every term and is in the navigation. The
brackets are CommonMark's own reference link, since the glossary's definitions are appended to
each document before it is parsed, so an author's own `[interface]: url` wins; a term defined
twice is a failure, as is a fence left open (which would swallow the definitions).

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
# REPL would), with `doc/fixtures/<document>.scala` compiled first where there is one.
session
fixtures doc/fixtures

# The project's own sources, whose API is documented by the overlays beneath doc/api, and the
# packages a document's references resolve through.
source src/core
overlay doc/api
imports soundness

# The glossary's terms, which a document names in brackets; and whether a link to the web is
# fetched when the documents compile (a link within the site is always checked).
glossary doc/glossary.tel
check-links

# Write the compiled documents and API pages here, as BinTEL, compiling only what changed.
output dist/doc

# Serve the documents from the daemon's start, behind the machine's Pyrocosm hub at
# http://localhost:8090/fuel/; `port` would serve them on a port of their own as well.
serve
```

Each setting is also reachable as a flag (`--document doc`), a `fuel.`-prefixed system property
and a `FUEL_`-prefixed environment variable, in that order of priority ahead of the files.

## Modules

- `fuel-core` (`src/core`): `Doc`, `Corpus`, the `Blocks` conversion of Markdown to Pyrocosm
  blocks (recording every `Linkage` it makes), the `Glossary`, and the `Embedding` of fenced
  code, for another tool to embed.
- `fuel-api` (`src/api`): the `Api` model, read from compiled code by `Inspection`, documented
  by `Overlay`s, with the `Resolver` of references, `Signatures` as tokens, `ApiPages`, and the
  `Links` check.
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
