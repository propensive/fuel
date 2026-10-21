# Fuel: design and roadmap

Fuel is the documentation tool of the Pyrocosm family (Fever, Fury, Flame, Flair, Fume). It
takes Markdown with embedded code, and later API documentation written in TEL with embedded
Markdown, and renders them — through Pyrocosm's one semantic model — to a live web page, a
static site, the terminal, or as fragments another tool can embed. Embedded code is highlighted,
compiled against a configured classpath so that it is known to be valid, and, where a block
describes a sequence of fragments, animated step by step.

Fuel is a rewrite of Amok (`~/work/amok`, renamed Fluence in its last commit, `~/work/fluence`
being a byte-identical checkout). Amok was built on Soundness 0.61 and nothing of it was written
for Pyrocosm; about half of its sources are no longer compiled by its own build. Fuel reuses its
designs — the frame-animation algorithm, the TEL preamble on code blocks, the API docs overlay
format, the TASTy walker — and none of its layering.

## Principles

1. **Fuel renders to Pyrocosm `Block`s, never to HTML.** A document becomes `List[Block]`; the
   web page, the terminal view, the static site and the serialised fragment are all Pyrocosm
   renderings of the same value. Amok wrote honeycomb, CSS and JavaScript of its own, and that
   is where it rotted. What Pyrocosm cannot yet express (animated code, a documentation site's
   navigation) is added to Pyrocosm, not worked around in Fuel.
2. **Layers, each useful alone.** Plain code is highlighted; code with a TEL preamble is
   enriched (notes, compile-checking, frames); Markdown embeds code of either kind; API docs
   embed Markdown in a type/term hierarchy. Each layer is a function of the one below, and each
   is a `Block`-valued thing a tool can ask for on its own.
3. **Compiled means checked.** A code fragment under a classpath is typechecked, and the result
   is part of the rendering (a `Block.Note` on an error, or a failed compile), not a side
   channel. Documentation that drifts from its API fails to compile, as the API does.
4. **The configuration is the compile step's input.** A `.pyrocosm/fuel/config.tel` names the
   classpath and the directories of documents, in the style every Pyrocosm tool shares; `fuel
   compile` needs no arguments.
5. **Error reporting is Pyrocosm's job.** Fuel reports a failure in one line and moves on. The
   positions, causes and presentation of failures are to be addressed in Pyrocosm, which Fuel
   will then inherit.

## The layers

```
                 ┌──────────────────────────────────────────────────────────────┐
  API docs (M5)  │  TEL hierarchy of types and terms, each entry holding Markdown │
                 └───────────────────────────────┬──────────────────────────────┘
                 ┌───────────────────────────────▼──────────────────────────────┐
  Markdown (M1)  │  punctuation `Markdown of Layout` → List[Block]              │
                 │  every fenced block dispatched by its info string             │
                 └──────────────┬─────────────────────────────┬─────────────────┘
                 ┌──────────────▼──────────────┐ ┌────────────▼─────────────────┐
  Code (M1, M2)  │  plain fence: harlequin     │ │ TEL preamble `##` code:      │
                 │  tokens → Block.Code        │ │ notes, context, compile,     │
                 │                             │ │ frames → Block.Code(+frames) │
                 └─────────────────────────────┘ └──────────────────────────────┘
```

Every layer's output is `List[Block]`, which Pyrocosm renders. Serialised, it is the TEL (or
BinTEL) wire form Pyrocosm's model already carries, which is what an MCP server, Flame, or a
static-site batch step consumes.

## What Amok has, and where it goes

Amok's sources were surveyed file by file (Soundness-version drift aside, the designs are
sound). Their disposition:

| Amok | What it is | Fuel |
|---|---|---|
| `web/FluenceRenderer.scala:33-80` | Frame animation: highlight each frame, align lines across frames with `dissonance.evolve`, then tokens within aligned lines; emit tokens with presence sets | **Keep the algorithm** (M3). Its output becomes frames on `Block.Code`, rendered by Pyrocosm; the radio-button CSS trick (`res/fluence/code.css`) becomes the web renderer's concern. |
| `web/Preamble.scala`, `Frame`, `Highlight`, `Selection`, `Range`, `Note` | The TEL schema of a ```` ```fluence ```` block: `syntax`, `context term\|type`, `highlight`/`error`/`caution`/`param <start..end> <caption>`, repeated `frame` | **Keep the schema** (M2/M3), renamed `fuel`. `Note` is already in Pyrocosm as `Block.Note(line, start, end, style, caption)` with the same four styles; `Selection.findIn` becomes the way a note's range is located. |
| `web/Transformation.scala`, `Replacement.scala` | Frames derived by editing a base frame (`replace`, `before`, `after`) | Modelled, never used. Drop; revisit if snapshots prove verbose. |
| `cli/app.scala` (unbuilt) | `check`: collect fenced `scala`/`fluence` blocks, join, compile with anthology `Scalac`, map notices back to fragments; `Fragment(id, language, errors: Fail\|Ignore\|Highlight\|Show, follows)` | **Keep the design** (M2): `errors` policy and `follows` chaining are the right vocabulary. Compile through a warm `Scalac.Session`, and harlequin's `typecheckedScala` depth gives per-token types for free. |
| `tool/Model.scala`, `Declaration.scala`, `Node.scala`, `Member.scala`, `Modifier.scala` | TASTy walker (`TastyInspector`), `Definition`/`Template` enums, `Model` of nodes with a `Lexicon` for fuzzy search, `overlay(flux)` merging TEL docs onto nodes | **Keep** (M5). `stenography.Typename` is now `Designator`; check `degustation.Inspection` as the newer walker pattern. |
| `tool/Flux.scala`, `example/*.tel` | The API-docs overlay format: `domain`, `base`, nested `entry .term` / `entry #Type` with `info`, `detail` (Markdown), `until <version> <name>`, `hidden`, `refer` | **Keep** (M5). This *is* "API documentation written in TEL with embedded Markdown"; its loader was removed in commit `9db21a8` and `Model.overlay` is unwired. |
| `tool/Criteria.scala` + tests | Search mini-language (`!pkg`, `:Type`, `.term`, `?given`, `~ext`, `+Ret`, `-Param`) | Keep (M5), as the dashboard's search field. Its 17 tests are the only tests Amok has. |
| `tool/fluence.scala` (`syntaxIsRenderable`, `memberIsRenderable`), `JvmFolio.scala`, `Page.scala`, `Server.scala`, `Mountpoint.scala`, `Folio.scala` | HTML rendering of signatures, the `<details>` nav tree, iframe layout, import elision cookie, HTTP routing | Replace: signatures become `Inline.Code` of stenography syntax; the tree is `Block.Tree` in a `Navigation` panel; routing is `WebFrontend`. The *import elision* idea (names qualified only beyond Predef, scala, java.lang, parents and the reader's own imports) is worth keeping as a document-level setting. |
| `tool/FluenceDoc.scala`, `runner/Fluence.scala`, `presentation.tel`, `scalaio.tel` | Presentations: a TEL header up to `##`, Markdown body split on `---`, served with long-poll reload, `navigate.js` | Out of scope until the Markdown layer is done; then a presentation is a `Markdown.sections` interface with one panel per slide (M7, if wanted). The two decks are the richest corpus of `frame` animations and make good M3 fixtures (`presentation.tel` uses the older `version` keyword). |
| `res/fluence/*.css`, `*.js`, icons | Presentation | Drop. Pyrocosm owns the stylesheet and the one script. Entity icons (`ti-def.svg` and friends) may return as `hints.web.Icon`. |
| `core/`, `model/` (unbuilt) | An older TASTy walker and path model | Drop. |

Soundness has since absorbed some of it: harlequin's `md` module (`CommonFormattable`, still
wrapping output in `Div.amok`), `Block.Note` in Pyrocosm ("fluence's annotated samples"), and the
```` ```amok ```` fixtures in `punctuation/res/bench`. Pyrocosm's design doc lists Fluence as its
M7 ("Fluence pages as blocks with the API tree as navigation"); Fuel is that work.

## What Pyrocosm and Soundness already provide

- `punctuation.Parser.parse(text): Markdown of Layout` — a complete CommonMark parser.
  `Layout.CodeBlock(line, info: List[Text], content)` carries the fence's info string split on
  spaces; the language is `info.prim` by convention. No GFM tables, no frontmatter, and raw
  HTML is emitted as a comment. `Markdown#sections` splits on thematic breaks.
- `pyrocosm.Presentable`: `(Markdown of Layout) is Presentable in Block`
  (`pyrocosm.Presentable.scala:145`) converts node for node, with code blocks as unhighlighted
  `Block.Code`. Fuel's conversion is this with a hook for code blocks (below); it should be
  offered upstream as Pyrocosm's M8 "Markdown renderer" once it has settled in Fuel.
- `pyrocosm-compiler`: `SourceCode#modelLines` and `sourceCodePresentable` turn harlequin's
  tokens into `Block.Code`. `harlequin.Scala.highlight(text, context)` tokenises with dotc's
  scanner; `Scala.Context.{Top, Term, Type}` wraps a fragment so an expression or a type
  highlights alone. `highlighting.typecheckedScala`/`sessionScala` add per-token types and
  `Diagnostic`s given a `Scalac`/`LocalClasspath`.
- `anthology.Scalac[3.10](options).on(classpath).session { sources.compile() }` — a warm
  compiler session over in-memory sources, with `Notice`s carrying spans.
- `dissonance.evolve(versions, similar): Evolution[element]` — element identity across many
  versions, which the frame animation is built on. Unchanged since Amok.
- `stratiform`: `text.read[Tel].as[Preamble]` decodes a TEL preamble into a case class with
  no `derives` clause; derivation sites must sit in a module compiled without capture checking
  (every Pyrocosm tool keeps one: fume's `relay`/`api`, flair's `records`).
- `pyrocosm.Tool` (`pyrocosm-cli`): the daemon, standard subcommands, config cascade and
  `Tool.Web`; `WebFrontend(port, fallback)`; `TerminalFrontend`; `Block`, `Inline`, `Panel`,
  `Interface`, `Live`.
- Missing, and needed by later milestones: an animated code block in the model (M3); a
  renderer that writes an `Interface` as static HTML files (M6); a TASTy-to-API model (M5);
  languages beyond Scala and Java in harlequin (M6).

## Configuration

`.pyrocosm/fuel/config.tel`, found by walking up from the working directory, under the rules
every Pyrocosm tool shares: a setting's camelCase name is a kebab-case keyword, a bare keyword
is `true`, a repeated keyword's atoms join with `:`. `~/.config/fuel/config.tel` supplies a
user's defaults.

```
tel 1.0

# The classpath embedded code is highlighted and compiled against; one entry per line,
# resolved against the project root.
classpath out/soundness/core/assembly.dest/out.jar
classpath lib/extras.jar

# The directories of Markdown documents `fuel compile` reads, recursively (`*.md`), each
# becoming a section of the site named by its last segment.
document doc
document doc/modules

# Serve the dashboard from the daemon's start, on this port (8060 by default).
serve
port 8060
```

Later keywords, as the layers arrive: `api <jar>` (M5, the jars whose API is documented and
whose TEL overlays are read), `imports <package>` (names elided in signatures), `output <dir>`
(M6, the static site). Each is a `Setting`, so flags, `fuel.*` properties and `FUEL_*`
variables reach it too.

## Roadmap

- **M0 Scaffold** (done). The repository in the shape of fever (the smallest tool): `build.mill` on
  the fume template with `FUEL_RELEASE_VERSION`, modules `core` (published as `fuel-core`,
  capture-checked), `client` (`fuel-client`: the `Tool`, subcommands, dashboard), `launcher`
  (`fuel.fuel`), `test`; `Makefile`, `AGENTS.md`, `.github/readme.md`, `ci.yml`, `release.yml`,
  `etc/{refs,tools,release,shared,github-ref,xek.tsv,header.txt}`, `.dockerignore`,
  `.pyrocosm/{fume,flair}/config.tel`, `mill`. Pins: soundness 0.72.0, pyrocosm 0.7.1. Port
  8060. `fuel about` works. Off-repo: add `propensive/fuel` to `pyrocosm/etc/downstream` and
  `github/settings/repos.tsv`; generate `etc/keys/release.pub` with `bin/release-key`.
- **M1 Markdown with embedded code, compiled and viewed** (done, first cut). This document's
  second half. Found while building it:
  - A chain of `Optional#let` calls with `_` lambdas (`file.parent.let(_.parent).let(_.parent)`
    inside an outer `let`) crashes the fork compiler (`TypeOps.orDominator` assertion) with no
    position; explicitly typed lambdas, one per line, compile. Bisecting a crash needs
    `./mill clean fuel.client` before each attempt: Zinc's incremental state after a crash gives
    false negatives.
  - `flair check` over the whole repository exits silently (status 0 or 2 with nothing printed)
    while each file or directory checks fine on its own; `make check` is run per directory
    until that is understood.
  - `fuel show` (the terminal rendering) slipped to M4, as the plan allowed.
  - Nothing is written to `$XDG_STATE_HOME`: a `fuel quit` forgets the corpus, and the next
    `fuel compile` rebuilds it in well under a second for this repository.
- **M2 The code-block preamble, and compile-checking** (done, first cut). A fenced block whose
  info string is `scala fuel` has a TEL preamble, a `##` line, and code. `Preamble(syntax,
  context, highlight, error, caution, param, errors, follows)` decoded by stratiform in a new
  non-capture-checked `fuel-records` module; notes located by `Selection.findIn` and attached
  as `Block.Note`s. Every Scala fragment in a document — plain fences included — is compiled
  in one warm `Scalac.Session` on the configured classpath, `follows` chaining fragments and
  `errors fail|ignore|highlight|show` deciding what a diagnostic means; a diagnostic becomes an
  `Erroneous` note, and `fuel compile` exits non-zero when a fragment that should compile does
  not. `fuel watch` recompiles on change (surveillance) and the dashboard follows.
  Found while building it:
  - The preamble is walked by hand from `Tel` (`fields`, `primaryAtom`, `atomTexts`), as flair
    reads its configuration, so no unchecked `records` module was needed after all.
  - Fragments are typechecked through harlequin's `highlighting.sessionScala` in one
    `Scalac[3.9].on(classpath).session`: a highlight at typechecked depth is a compile, and
    yields typed tokens and offset-span diagnostics together. `follows` prefixes the followed
    fragments' text and shifts the spans back, by harlequin's term/type wrapper too.
  - `errors` has three values, not Amok's four: `fail`, `show` (errors expected: their absence
    is the problem) and `ignore`.
  - Without a `classpath` nothing is compiled, since not even the standard library would be on
    it; fuel's own fixture compiles against its launcher assembly with the fork's flags as
    `scalac-option`s. A `soundness` shorthand for those flags is worth adding.
  - Not yet: `fuel watch`; a warning as a `Caution` note is shown but never a problem.
  - **Diagnostics** (Pyrocosm M6d, pinned as a snapshot of `cf9a815` until released): every
    problem is a `Block.Diagnostic` at its place in the Markdown file — a fragment's compiler
    error (location from the fence's line, which punctuation's `Layout.CodeBlock` carries, so
    the note the design doc thought Fuel would need upstream was not needed), a preamble that
    is not TEL (`Block.Diagnostic.of(telError)` shifted to the file), an unreadable file, a
    failed session. `fuel compile` prints them through `TerminalRenderer.blocks`; in a document
    a diagnostic follows its fragment, and every fence's `Block.Code` is numbered from the
    file (`start`). A note is one `Block.Note`, across lines or a caret, with its caption as
    phrasing; harlequin's offset-mode spans are placed by Fuel's own `Notes.of`, since
    `Block.Note.of` declines them.
  - All compiler use (tokenizing included) runs on one dedicated thread with a large stack, in
    `Checking.exclusive`: dotc pins a context base to the thread of its first run, so a later
    run from another thread fails with `RecursionOverflow` and no frames. The daemon serves each
    invocation on its own connection thread, so without this the first compile after a highlight
    would fail.
  - **Upstream bug found** (harlequin, with the fork; reported as propensive/soundness#2284
    and propensive/proscala#19): highlighting
    `val sum: Int = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12` at *tokenized* depth
    throws `dotty.tools.dotc.core.RecursionOverflow`, in-process or in the daemon. The fork's
    `TreeAccumulator.foldOver` throws it when `ContextBase.recursiveDepth` reaches
    `recursiveOperations.length`, an array `ContextBase.initialize` sizes from `-Xmax-fuel`
    (default 550) — but harlequin tokenizes on `Contexts.ContextBase().initialCtx.fresh`
    (`harlequin.SourceCode.scala:44`), never initialised, whose array keeps its 10-slot field
    default, so any expression nested ten deep overflows its parse-tree fold. It is not a stack
    overflow (the JVM's exception log shows none) and not the daemon (every probe — classloaders,
    settings state, thread stacks — came back ordinary). Until harlequin initialises that base,
    the fixture keeps its expressions shallow, and `fuel compile` reports a session or
    highlighting failure in a line and shows what it can.
- **M3 Animation** (done, first cut). Repeated `frame`s in the preamble, and Amok's alignment — highlight each
  frame, `dissonance.evolve` lines by similarity, then tokens within aligned lines — kept as
  it is; its *output* and its *rendering* change, so that the front end does the animating
  with a little JavaScript, which is simpler than Amok's CSS and leaves room to change the
  behaviour and to schedule it later.

  Amok's rendering laid out the union of every frame's tokens in every line, gave each an
  inline `width: Nch`, and used a ladder of `.sN:checked ~ … :not(.vN)` selectors (capped at
  fifty-one frames, driven by radio inputs) to collapse what the chosen frame lacked. So a
  token could never move between lines, a line could not be inserted smoothly, and there was
  no notion of time.

  **Model (Pyrocosm).** Animation is data about identity and placement, never geometry:
  `Block.Code` gains `animation: Optional[Animation]`, where `Animation(atoms, frames,
  placements, autoplay, loop)`; an `Atom(id, text, accent, role)` is a token with a stable
  identity across frames; a `Frame(index, label, hold)`; a `Placement(frame, line, atom)`, flat
  because a repeated TEL field cannot nest. `lines` remains the first frame, so a renderer
  that knows nothing of animation (the terminal, a static page, a fragment shown in Flame)
  shows a correct block.

  **Server (Fuel).** Alignment yields atoms with ids and a placement per frame instead of
  presence classes. A token `evolve` sees as the same atom on another line in the next frame
  is placed on that line, which is the cross-line move for free. Each frame compiles on its
  own under M2.

  **Browser (`pyrocosm.js`, generic).** `HtmlRenderer` emits frame 0 as ordinary `pyro-line`
  and `pyro-token` spans carrying `data-atom`, the animation as JSON in a `<script
  type="application/json">` within the block, and controls (`pyro-frames`: previous, next, a
  counter, play). A step is FLIP: record each atom's rect, rebuild the lines of the target
  frame reusing the same nodes keyed by atom id, record the new rects, apply the inverted
  transform, transition to identity; entering atoms fade in, leaving ones fade and collapse
  their width. Geometry is measured, not predicted, so an inserted line, a reflowed line and a
  move across lines all just work, and the behaviour lives in one function. Arrow keys when
  focused, space to play, `prefers-reduced-motion` jumps. No inline `style`, no selector
  ladder, no frame cap; without the script a page shows frame 0, which Pyrocosm's pages
  already assume. `Frame.hold` and `autoplay` time one block; a document-level timeline (one
  clock driving several blocks by `block#frame`, which a presentation needs) is a later
  `Interface`-level addition nothing here precludes.

  **Plan.** (1) A Pyrocosm PR: the model and its TEL codecs, the renderer's output, the FLIP
  behaviour in `pyrocosm.js`, one animated block in the gallery, the terminal showing frame 0
  (stepping is M4's); pinned here as a snapshot. (2) Fuel: `frame` in the preamble,
  `Alignment` (evolve to atoms and placements) with tests for an inserted line, a token moved
  across lines and a rename, each frame compiled. (3) Fixtures from `scalaio.tel`'s sixteen
  blocks (`presentation.tel` uses the older `version` keyword).

  Done as planned, with the alignment in Pyrocosm (`Block.Animation.of`) rather than here, so
  the gallery animates too and any tool can; pinned as a snapshot of Pyrocosm's `animation`
  branch. Fuel reads `frame`s, `autoplay` and `loop`, compiles each frame on its own (a failing
  frame is a problem naming it), and emits one `Block.Code` whose lines are the first frame.

  Then the `frame` became a compound with children, since a TEL compound with a source atom
  can have none: `code` (required; a frame without it is a problem at its line, found by
  counting the preamble's `frame` lines, as a TEL node does not know its own, and dropped),
  `label` (Markdown phrasing, through `Blocks.inline`), `hold`, `at` (a time on the page's one
  clock, which Pyrocosm's script drives), `language` (a Java frame is highlighted by harlequin
  alone; a later `follows` gets the last Scala frame's code), and marks of its own; the
  preamble's marks apply in every frame they match, and every mark's selection may be refined
  by `line` and `occurrence` children. `Selection` is the one object that reads and finds a
  range, so that other ways of naming one can be tried there alone. Each frame goes to
  `Animation.of` as a `Slide.Detail`, and Pyrocosm derives the transition into each slide. Not yet: `scalaio.tel` as fixtures (its decks need the
  presentation layer, M7).
- **M4 Terminal and fragments** (`show` done). `fuel show <file>` sets one document in the
  terminal through `TerminalRenderer(justify = true)`: paragraphs justified and hyphenated at
  the terminal's width, code highlighted, an animated block as its slides in sequence under
  `step n of m` markers (the renderer's own addition, since a terminal cannot step), problems
  after — compiled as `compile` compiles, so the page shows what the dashboard would. An
  interactive view through `TerminalFrontend` (navigation of headings) can follow. `fuel
  blocks <file>` writes the document's blocks as TEL, which is the fragment form other tools
  consume, and the dashboard's `fallback` serves the same at `/blocks/<document>`. An MCP
  server (synesthesia, as fume's) answering `documents`, `document`, `search` over the corpus.
- **M5 API documentation.** The TASTy walker (`Model`, `Declaration`, `Node`) rebuilt on
  `scala3-tasty-inspector` and `stenography.Designator`/`Syntax`; TEL overlays in the `Flux`
  format, their `info`/`detail` Markdown rendered through M1–M3 and typechecked against the
  jar they document (an entry naming a member that does not exist is a failure, as a fragment
  that does not compile is); `Block.Tree` navigation of packages, types and terms;
  signatures as `Inline.Code`; `Criteria` search in the dashboard; import elision as a
  setting. Fuel's own API docs as the fixture.
- **M6 Outputs and languages.** A static site: Pyrocosm's `HtmlRenderer` and `PyrocosmPage`
  written to files, one page per document, with the stylesheet and no socket (a Pyrocosm
  feature: `WebFrontend` has the pieces, a batch writer does not exist). Languages beyond Scala
  and Java: TEL schemas as a hierarchy (records, fields, scalars — the first non-JVM API),
  Java through harlequin as now, other fences highlighted by a pluggable tokenizer or shown
  plain. Pyrocosm's `Language` is open, so nothing in the model changes.
- **M7 Presentations** (optional): Amok's `format presentation` decks as a one-panel interface
  stepping through `Markdown#sections`, with the animations of M3. Only if wanted.

### Decisions

- Blocks, not HTML; what the model lacks is added to Pyrocosm (principle 1).
- The compile step lives in the daemon: `fuel compile` leaves a `Corpus` in memory that `fuel
  serve`, `fuel show` and the dashboard read, as fume's runs and flair's findings do. Writing
  the corpus to `$XDG_STATE_HOME/fuel/` as TEL is deferred until something needs it to survive
  a `fuel quit`.
- The code-block hook is a typeclass in `fuel-core`, not a Pyrocosm change, until M3 forces
  one. Amok's `Formattable` (punctuation's `(info, content) => Optional[Html]`) is the model,
  with `List[Block]` as the result.
- One flat package, `fuel`; files named `fuel.Type.scala` and `fuel_topic.scala`; `import
  soundness.*` with `pyrocosm.Tool` imported by name; capture checking on, separation checking
  off (as flame, flair, fever and pyrocosm; the web module threads handlers it rejects).
- Fluence (`~/work/fluence`) stays as it is, as the reference checkout; nothing is built from it.

### Open questions, to settle as they arrive

- The fence label for an enriched block: ```` ```fuel ```` (Amok's style, language in the
  preamble's `syntax`) or ```` ```scala fuel ```` (language first, so a renderer that knows
  nothing of Fuel still highlights it). The second degrades better; M2 decides.
- Whether `##` as the preamble separator survives TEL's treatment of `#` as a comment leader;
  fever's script headers use the same separator, so there is precedent.
- GFM tables: punctuation is CommonMark only, and documentation wants tables. Either a
  punctuation extension or a `fuel` fence holding a TEL table.

## M1: Markdown with embedded code, compiled and viewed

The deliverable: `fuel compile` reads the configured directories of Markdown, turns every
document into Pyrocosm blocks with its Scala and Java fences highlighted, and reports what it
compiled; `fuel serve` (or a bare `serve` in config) shows the corpus on the Pyrocosm web
interface, a navigation tree of documents beside the chosen document. Nothing is compiled by
scalac yet (M2), and no fence has a preamble yet (M2); but the classpath is already configured
and read, so M2 adds a compiler session without touching the configuration or the model.

### Modules and files

```
src/core/      fuel-core (published; capture-checked)
  fuel_core.scala          package-level givens and the `Embedding` typeclass
  fuel.Document.scala      Document(path, title, blocks)
  fuel.Corpus.scala        Corpus(sections: List[Section]); Section(name, documents); lookup
  fuel.Markdown.scala      Markdown of Layout → List[Block], dispatching fences by info string
  fuel.Embedding.scala     Embedding: (info: List[Text], content: Text) => Optional[List[Block]]
                           instances: scala, java (harlequin → Block.Code), plain fallback
  fuel.Compilation.scala   Compilation.compile(config): Corpus — read directories, parse, convert

src/client/    fuel-client (published)
  fuel_client.scala        val Fuel: Tool; object ui (Compile, Serve, Show; Settings
                           classpath, document, port); object Client (run, dispatch)
  fuel.Workspace.scala     the Settings read into (classpath: List[Path], documents: List[Path])
  fuel.Dashboard.scala     the Tool.Web: Interface of a Navigation panel (Block.Tree of
                           sections and documents, each node carrying an Action) and a Primary
                           panel (the chosen document's blocks); handle(Pressed) swaps the
                           document; the daemon-held Corpus is its source
  fuel.Server.scala        the daemon's state: @volatile corpus: Optional[Corpus], replaced
                           by each compile; Live cells the dashboard watches

src/launcher/
  fuel_launcher.scala      object fuel: main = externalize(Client.run())

src/test/
  fuel_test.scala          Suite: a document with a heading, prose, a scala fence and a plain
                           fence converts to the expected blocks; a corpus from a temp
                           directory tree has the expected sections and titles
  fuel_test_main.scala     @main runTests
```

### Steps

1. **M0 scaffold** as above, from fever's shape and fume's templates; `make run` gives `fuel
   about`. Commit.
2. **`Embedding` and the Markdown conversion.** Copy Pyrocosm's `layout`/`prose` conversion
   (`pyrocosm.Presentable.scala:149-186`) into `fuel.Markdown`, replacing the `CodeBlock` case
   with a fold over the `Embedding` instances in scope, falling back to a plain `Block.Code`
   under `Language(info.prim)`. Instances for Scala and Java call `Scala.highlight` /
   `Java.highlight` at tokenized depth and use `pyrocosm-compiler`'s `modelLines`. Keep both
   emphasis strengths (`Inline.Emphasis` and `Inline.Strong` both exist). Test against a
   fixture document.
3. **`Document` and `Corpus`.** A document's identity is its path relative to the `document`
   directory it was found under; its title is the first level-one heading, else the file's
   name. A section is one `document` directory. `Compilation.compile` walks each directory
   (galilei glob `**/*.md`), reads, parses (`punctuation.Parser.parse`) and converts; a file
   that fails to read or parse is skipped with one line to the console.
4. **Settings and `fuel compile`.** `classpath` and `document` as repeated `Setting[Text]`s,
   split on `:` and resolved against the project root (as flame does), `port` defaulting to
   8060. `fuel compile` builds the corpus, stores it in `Server`, and prints one line per
   section: its name and document count. The classpath is read and validated to exist, and
   otherwise unused until M2.
5. **`Dashboard` and `fuel serve`.** `Tool.Web` on the flair pattern: a `WebFrontend(port)`
   running one shared `Interface`. The navigation panel's `Block.Tree` has a root per section
   and a node per document, each with an `Action`; `handle(Event.Pressed(action))` sets the
   primary panel's `Live[List[Block]]` to that document's blocks. A compile while serving
   rebuilds the tree and re-shows the same document if it still exists. `fuel serve` runs the
   same in the foreground until Ctrl+C; a bare `serve` in config runs it from the daemon's
   start (`Tool` does this given the `web` field).
6. **`fuel show <file>`** (cheap, and proves principle 1): convert one file and run a
   one-panel `Interface` on `TerminalFrontend`. Can slip to M4 if the terminal frontend needs
   anything Fuel does not have.
7. **Readme and fixture.** `.github/readme.md` in fume's shape (what it is, usage, configuration
   files, building); Fuel's own `doc/` as the fixture corpus, so `fuel compile` in this
   repository shows this document.

### Done when

`fuel compile` in a project with a `.pyrocosm/fuel/config.tel` naming `doc` reports its
documents; `fuel serve` at `http://localhost:8060/` lists them in the side panel, shows the
chosen one with its Scala fences highlighted in Pyrocosm's code styling, and the masthead shows
Fuel alongside any other running tool; `make test` passes under the pinned fume; CI is green.
