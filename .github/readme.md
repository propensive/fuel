# Fuel

__Fuel__ is the documentation tool of the [Soundness](https://soundness.dev/) ecosystem. It
compiles a project's Markdown documentation — with the code embedded in it highlighted, and in
time checked against the project's classpath and animated — into
[Pyrocosm](https://github.com/propensive/pyrocosm)'s semantic model, and serves it on the web.
Later it will document a project's API, from its TASTy and TEL overlays, as a navigable site.
Its design and roadmap are [`doc/design.md`](../doc/design.md).

Fuel runs as an [Ethereal](https://soundness.dev/ethereal/) daemon and is a Pyrocosm tool, so
the subcommands and configuration files every Pyrocosm tool shares are fuel's too.

## Usage

```sh
fuel compile       # compile the configured directories of Markdown documents
fuel serve         # serve the compiled documents at http://localhost:8060/ until Ctrl+C
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
highlight Mi..es  why   # a range, by its first and last characters or literally, and a caption
error  oops
  caption  or beneath it
```

`caution` and `param` mark ranges as `highlight` and `error` do. Repeated `frame`s (each one's
code indented beneath it, or on the line for a one-liner) make the fence an *animation*: one
block that steps through the frames, each token keeping its identity across them so that a
step moves what stays and fades what arrives and departs; `autoplay` plays it when shown, and
`loop` starts it again at the end. Each frame is compiled on its own.
[`doc/markdown.md`](../doc/markdown.md) shows each.

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

# Serve the documents from the daemon's start, on this port (8060 by default).
serve
port 8060
```

Each setting is also reachable as a flag (`--document doc`), a `fuel.`-prefixed system property
and a `FUEL_`-prefixed environment variable, in that order of priority ahead of the files.

## Modules

- `fuel-core` (`src/core`): `Doc`, `Corpus`, the `Blocks` conversion of Markdown to Pyrocosm
  blocks, and the `Embedding` of fenced code, for another tool to embed.
- `fuel-client` (`src/client`): the `Tool`, its subcommands and settings, the daemon's corpus
  and the dashboard.
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
