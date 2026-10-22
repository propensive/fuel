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
fuel serve         # serve the compiled documents at http://localhost:8060/ until Ctrl+C
fuel show doc/x.md # one document, set in the terminal: justified, hyphenated, highlighted
fuel blocks doc/x.md  # one document's blocks as TEL, for another tool to show
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
it follows the fragment, whose lines are numbered from the file too. A fence marked
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
  entry #whisper  gone
    until 0.74.0  gossamer.Text#shout # removed in that version; its replacement, if any
  entry .Internal
    hidden                           # not listed, and its members not asked for
  refer gossamer.Textual             # "See also"
```

The overlay and the code are held to each other: an entry naming a member nothing declares
fails the compile (unless it is marked `until`, when it keeps a page saying what replaced it),
an `until` on a member which still exists is a caution, and a public member of a documented
package with no entry is a caution at the line it is declared, naming the entry to write. A
member which overrides or implements an inherited one is documented by it.

Everything that names an entity links to its page, when the page exists: the identifiers in a
signature; inline code in prose (`` `Text` ``), resolved as a name in code would be — as
written, then under the page's own entity and those enclosing it, then under the `import`s of
the overlay (or, for a Markdown document, the `imports` setting and the imports its own fences
make), then `scala`, `scala.Predef` and `java.lang` — a name which several entities answer to
in the same scope is a caution and stays plain; and the typed tokens of a compiled fence, when
the token is the name of what its type names. Amok's `` `#member` `` and `` `.member` `` name a
member of the page's own entity.

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

# Serve the documents from the daemon's start, on this port (8060 by default).
serve
port 8060
```

Each setting is also reachable as a flag (`--document doc`), a `fuel.`-prefixed system property
and a `FUEL_`-prefixed environment variable, in that order of priority ahead of the files.

## Modules

- `fuel-core` (`src/core`): `Doc`, `Corpus`, the `Blocks` conversion of Markdown to Pyrocosm
  blocks, and the `Embedding` of fenced code, for another tool to embed.
- `fuel-api` (`src/api`): the `Api` model, read from compiled code by `Inspection`, documented
  by `Overlay`s, with the `Resolver` of references, `Signatures` as tokens and `ApiPages`.
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
make check        # flair, against Consequent Style
```

Dependencies are pinned in `etc/refs` and installed by `make sync-deps`; see `AGENTS.md` and
[propensive/.github](https://github.com/propensive/.github) for the release flow.

## License

Fuel is copyright &copy; 2026 Jon Pretty &amp; Propensive O&Uuml;, and is made available under
the [Apache 2.0 License](/LICENSE).
