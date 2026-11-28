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
fuel present doc/slides.tel --transcript doc/slides.md --align talk.wav  # the same, timed to a recording of the transcript
fuel present talk.tel --transcript talk.fr.md --align talk.fr.wav --language fr  # a recording in French
fuel transcript doc/slides.tel doc/slides.md  # a transcript of the presentation, checked against its slides
fuel sync github propensive/fuel  # attach a repository's issues to the API pages, and fetch them
fuel sync          # fetch every attached tracker's issues again
fuel install       # install tab-completions and the manpage
fuel about         # fuel's version, and the daemon serving it
fuel --version     # the version alone
fuel quit          # stop the daemon, and any server it runs
```

A failure of the daemon itself while it carries out a command (running out of memory, say) is
reported on the standard error, where it was thrown, and the command exits with status 6, apart
from a compile's own failures (status 4), from a tracker whose issues could not be fetched
(status 7), from a presentation whose transcript and alignment do not correspond (status 8),
from an aligner that could not be installed or run (status 9), from a presentation whose
transcript does not cue its states, each once and in order (status 10), and from a command line
it did not understand (status 2).

Every error, diagnostic and failure fuel reports is named by a bolt code, the scheme of all of
Propensive's projects: `↯/f.25` to `↯/f.28` are fuel's (its core, API, client and launcher), as
`FuelBolts` declares, and `[↯/f.27.5.7] usage: fuel show <file>` is printed after its code's
label. A code which carries another names the error reported on account of it: a glossary's
fault the TEL schema finds, `↯/f.25.4.1/s.…`, carries the TEL error's own code. The record of
what every code means is `bolt.tel` in
[propensive/pyrocosm](https://github.com/propensive/pyrocosm).

`fuel compile` reads every `*.md` in each configured section's directory, parses it as
CommonMark, and converts it to Pyrocosm blocks: a fenced block whose info string is `scala` or
`java` is highlighted through [Harlequin](https://soundness.dev/harlequin/); any other fence is
shown plain under its own language name. The result is kept in the daemon, and `fuel serve` (or
a bare `serve` in the configuration) shows it: every section's documents, by title, beside the
chosen document. A document's title is its first level-one heading, else its filename.

The documents are configured as *sections*. A `section` is a name and a directory, and holds
what else is found in that directory beneath it: perhaps the file there which says what a
reader is told of the section, each directory of the API's documentation, and each glossary:

```
section guide docs                  # the documents beneath `docs`
  metadata section.tel              # the section's file, `docs/section.tel`
  overlay api                       # `docs/api`
  glossary glossary.tel             # `docs/glossary.tel`
```

Each value on these lines is a member with a keyword of its own, which the configuration's
schema (`fuel/config.schema.tel`, among Fuel's resources) declares in the order they are
written: a section's `name`, `path`, `language` and `metadata`. So a member may be written
beneath its parent instead, and `section guide docs en section.tel` is this section, written
in English:

```
section
  name guide
  path docs
  language en
  metadata section.tel
  overlay api
  glossary glossary.tel
```

Every path is within its parent's: the section's directory is in the project's, and the
section's file, an `overlay` and a `glossary` in the section's. A path which begins with `/`
is where it says.

The documents may be held in an [Orthant](https://github.com/propensive/orthant) store,
which keeps each in every language it is translated into, and at every release. A `store`
says the directory the store is in — the one which has `.orthant` in it — and beneath it an
`at` for the value of each of its axes, other than its languages, which the documents are
read at:

```
store .
  at release=1.2.0
section guide docs
```

Whatever the configuration names within the store's tree — its live tree, where it has one,
and its working tree otherwise; a section, its file, an overlay, a glossary — is then read
from the store, in each of its languages, and not from the files there: see
[Languages](#languages). Anything else is read from the
filesystem, in the one language its section says, if it says one.

The name is the first part of the address of each of the section's documents,
`/doc/guide/intro.md`, whatever its directories are called and whichever language is read. It
is one segment of an address, so it holds no `/` or `#`, and no two sections share one. A
`section` which is not what the schema allows — not a name and a directory — is a failure,
and so are an `original` and a `translation`, which said a section's languages before a store
did, and a `document` line, which a section once was. A directory given by `--document` or in
the environment has nowhere to say more, and is the section named by its last segment, read
in place of the configuration's.

What a reader is told of a section is not in the configuration but among its documents, in
the section's file, its `metadata`, so that it is translated as everything else there is. It
is a TEL file, to the schema `section`, which says the section's `title`:

```
title  User guide
```

The navigation shows the title in place of the name, which is shown where there is none, or
no file.

When a `classpath` is configured, every Scala fence is also *compiled*, in one warm compiler
session on that classpath with the configured `scalac-option`s, so a fragment that does not
typecheck fails the compile (exit status 4). A macro the fragments call is run against that
classpath, never fuel's own copy of a library it uses (Proscala's `-Zisolated-macros`). Every problem — a fragment that does not typecheck,
a preamble that is not TEL, a file that cannot be read — is a Pyrocosm diagnostic: `fuel
compile` prints it as the terminal renders one (`path:line:col: error` and its bolt code, the
message, and the lines concerned numbered from the file, with the place underlined), and in the
served document it follows the fragment, whose lines are numbered from the file too. Every
link is checked too, once every document is compiled: a link to another document (`guide/x.md`, `../x.md`,
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

slide one-value
  syntax scala
      val x = 1

slide a-second-name
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
whose code has `frame`s of its own is an animation alone. A frame which says `errors show` is meant not
to compile, and shows what the compiler says of it, as a fence's frame does; an example of a
mistake is written so, and not as a slide of its own, since a slide joined to its neighbours as
a frame does not keep what its `syntax` says of errors. Code is compiled as `fuel show`
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
`fuel present <file> --transcript talk.md --align talk.wav` aligns the recording against the
transcript (`fuel transcript <file> talk.md` prints it, to read from) with
[Montreal Forced Aligner](https://montreal-forced-aligner.readthedocs.io/), and each state after
the first arrives at the moment the first word of its cue is spoken — or, where the aligner
missed that word, the first it caught from there to the next cue. `--align` also takes an
alignment already made, in any of MFA's formats — a TextGrid (its default), JSON or CSV —
telling a recording from an alignment by its extension or its first bytes. A transcript
must be given, every state after the first must have an id, and the transcript and the
alignment must correspond: a word the aligner did not know (`<unk>`) stands for any one word,
but if more than one word in ten is in the one and not the other, the presentation is not
shown, and the differences are printed as a word diff (`-word` for a word of the transcript not
spoken, `+word` for one spoken that the transcript lacks), with status 8; fewer differences are
shown the same way, and the presentation goes ahead. `--align` implies `--autoplay`.

A recording is taken to be in English. `--language` says otherwise, as `fuel present <file>
--transcript talk.fr.md --align talk.fr.wav --language fr`: the recording is aligned with the
dictionary and the acoustic model Montreal Forced Aligner publishes for that language, which are
downloaded the first time it is aligned. It takes a language's code, whose region is not looked
at — `bg`, `cs`, `de`, `en`, `es`, `fr`, `ha`, `hr`, `pl`, `pt`, `ru`, `sv`, `sw`, `tr` or `uk`,
the languages written with spaces between their words which MFA has both for — or the name of
one of MFA's models, for one of its others (`--language english_us_arpa`); a language there is
no model for fails with status 9. An alignment already made is in whatever language it was made
in, and `--language` says nothing of it.

MFA is published only as conda-forge packages, so fuel installs it itself the first time a
recording is aligned, into `$XDG_CACHE_HOME/fuel/mfa` (`~/.cache/fuel/mfa`): it fetches
micromamba (a single static executable which installs conda-forge packages into a directory,
with no Anaconda, no shell set-up and no activation), makes an environment there with MFA 3.4
(about 2 GB, a few minutes), and downloads the dictionary and acoustic model of the recording's
language (`english_mfa`, for English).
Nothing else on the machine is touched; `rm -rf ~/.cache/fuel/mfa` removes it all. An alignment
is kept in the project's cache by a digest of the recording, the transcript and the model, so a
presentation is aligned again only when one of them changes; MFA itself takes tens of seconds to
start and align. If fuel cannot install MFA — there is no micromamba for the platform, or a
download fails — it says so and prints the three commands to do it by hand (`brew install
micromamba`, `micromamba create -p ~/.cache/fuel/mfa/env -c conda-forge
"montreal-forced-aligner>=3.4,<4"`, `mfa model download …`), and exits with status 9. Record in
one take, as WAV or FLAC for choice (MFA's own sox and ffmpeg convert the other formats); a
whole talk is aligned as one utterance, which MFA manages but is its weakest case, so if it
finds no alignment fuel tries once more with wider beams.

### A stylesheet of its own

A presentation is shown in Pyrocosm's colours and faces unless it names a stylesheet, with
`style`, by a path relative to its own file:

```
title  Effects, gently
style  talk.css
```

The stylesheet is a CSS file, linked by the presentation's page after Pyrocosm's own, so its
rules prevail. It may set Pyrocosm's custom properties (`--pyro-code-bg`, the
`--pyro-code-accent-…` of each kind of token, `--pyro-font-mono` and the rest), give rules to
what is on a slide (`.pyro-stage`, `.pyro-leaf`, `.pyro-codeblock`), and begin with the
`@import` of its fonts. A stylesheet which cannot be read fails the presentation, at the line
which names it.

A presentation may name a script of its own in the same way, with `script`: a JavaScript
file, run by the page after Pyrocosm's own script, for what a stylesheet cannot do, such as a
backdrop which moves.

### Typed changes

A step of an animation moves the tokens which stay to their new places, and fades the others in
and out. That suits a change which rearranges the code. A change which is better seen made is
*typed*: a frame which says `typed` is arrived at by typing the change from the frame before,
a keypress at a time, quickly but not evenly, behind a cursor a character wide.

```
slide
  syntax scala
    frame
      code
          val id = uuid"not-a-uuid"
    frame
      typed
      code
          val id = uuid"7a0c2f5e-1b6d-4c3a-9e8f-2d4b6a8c0e1f"
```

What the two frames share is kept: the lines which are in both, and, on a line which changes,
what it begins and ends with. What is between is erased and typed again, and a line which goes
or comes is erased or typed whole. While it is typed, a character keeps the colour it had, and
one typed into a token which keeps characters of another colour takes theirs: the frame's own
colours arrive when the typing is done, with its marks. `typed` on the `syntax` itself makes
every frame of the animation typed, and on a slide which is a frame of a run, that slide. A
fence's frames in a document may be typed too. Under `--autoplay`, a typed step is started as
long before its time as its typing takes, as any step is.

### The transcript

What is said while a presentation is shown is a transcript: a Markdown file of its own, which
the presentation does not name, and is given with it, as `fuel present <file> --transcript
talk.md` — so one presentation may have a transcript in each language it is given in, and
`--transcript talk.fr.md` shows the same slides to the French. The two are tied by the
presentation's *states*. A presentation is in one state at a time: a slide as it is
first shown, or a later frame of the animation stepped through on it (the frames of a `syntax`
with `frame`s, or of the last fence with frames in its Markdown). A state may have an `id`, on
the line of its `slide` or `frame` or beneath it:

```
slide declare
  syntax scala
    frame
      code
          def twice(n: Int) = n*2
    frame reveal-inferred-type
      highlight : Int
      code
          def twice(n: Int): Int = n*2
```

A slide with frames is first shown in its first frame, which is one state, so the id is the
slide's or that frame's, and not one of each. In the transcript, the words which bring a state
are a link to its id, behind an `@` — a *cue*:

```
We [declare](@declare) a method without saying what it returns, and the compiler will work out
that the return type is [this](@reveal-inferred-type).
```

The state arrives as the first word of the link is spoken; a link with no words, `[](@id)`,
cues the word after it. What is said is the transcript's prose — its paragraphs, and those of
its lists and quotations — and not its headings, which are there to find a way about it by; a
link to anything else is said as its words are. A transcript among a section's documents is
read as any document, its cues' words as any others.

A transcript given must cohere with the presentation, and one which does not is a failure: the
presentation is not shown, each fault is reported at its line in the file it is in, and the
command exits with status 10. Every state with an id must be cued, once, and the cues must
come in the order the states are shown; a cue must name a state, and be in what is said; and
no two states may have one id. A state without an id needs no cue — it is stepped to by hand,
or by the regular schedule — but a presentation timed by `--align` needs an id on every state
after the first, which is shown from the start.

`fuel transcript <file> <transcript>` prints the transcript, set to the terminal's width
without breaking a word across lines, a cue's words as any others, with what the reading of the
presentation reports after it; it makes the same check, and fails the same way.

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
on, each document by its section (its directory relative to the project root) and its
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
one, an `inline` one is sheared, a `transparent` one drawn in outline, an abstract one dotted.
A class is three quarters of two rings, a trait half of them, a type four arcs of a ring, and an
enum five discs; a class, a trait, a type or an enum with a companion object is one glyph, a
whole circle, the object's ring filling the sectors the type leaves open, or threading the
enum's discs.
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
in the same scope is a caution and stays plain. A fence's import brings what it would in code:
`import a.b.*` every member of `a.b`, and `import a.b.C` (or `a.b.{C, D as E}`) those names
alone, which come before a wildcard's, so that naming one member of a library does not set its
others beside an umbrella package's exports of them. The caution is at the line the name is
written on (a table's, at the line its fence opens on) and says which entities answer; an
export and what it exports — a type with parameters included — are one entity, however many
imports reach it. A name is followed through an export in its
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

Every `fuel compile` also publishes the API's one-line descriptions for the other tools on the
machine — Flame shows an entity's `info` beside its completion — as a RaTEL image in the
project's cache, `$XDG_CACHE_HOME/fuel/<project>/share/help.ratel`: each documented entity's
address (`gossamer.Text#length`, or `presentation/…` for another format's) with its `info` as
plain text. The file is replaced whole, and a tool maps it into memory and finds one entry by
its address, so it needs neither Fuel's daemon nor any configuration, and sees a newer index
the next time it looks.

Beside it, `pages.ratel` has every entity's page — documented or not — by the same address, as
its path beneath the site's root (`/api/gossamer.Text~length`). The pages are there only while
Fuel serves them, so for as long as it does, and no longer, `site.ratel` says where: the
`address` of the site's root as a browser on the machine opens it, without a final `/`
(`http://localhost:8090/fuel`, or `http://localhost:<port>` where a `port` is configured), and
the `pid` of the process serving it, by which a tool tells an announcement a crash left behind
from a live one. A tool which finds it may link a name to the address followed by the page's
path. It is the project compiled last whose site is announced, since those are the pages the
daemon serves, and `fuel serve stop` and `fuel quit` withdraw it.

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
variants are members of the type form of theirs (`presentation.Slide#id`,
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
  entry #id   the name the transcript cues the slide by
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

A `glossary` is a TEL file of terms (several may be configured, and are combined), to the schema
`glossary`, each with a Markdown `definition` and, when a page explains it in full, a `link` to
that page, on the site (`/doc/guide/interfaces.md`) or elsewhere:

```
tel 1.0

define  user interface               # a name of several words follows two spaces
  synonym UI                         # another name for it: see below
  keyword  front end                 # found by a search for `front end`; shown nowhere
  link  /doc/guide/interfaces.md
  definition
      Where a person and a program meet and exchange information.

define padding
  unlisted                           # defined, but on no page and found by no search
  definition
      What fills a value out to a width.
```

As TEL reads a line, one space separates one value from the next, so a value with spaces in it —
a term's name, a keyword, a synonym — follows two: `define type class` is a term `type` whose
`term` (which only a glossary in another language says) is `class`, and a failure. What the schema finds wrong
with a glossary is a failure at its line.

A term in brackets in prose — `[interface]`, in any case, or `[interfaces][interface]` for an
inflected form — shows its definition when pointed at, and leads to its link, or to the term's
entry on the glossary page at `/glossary`, which lists every term and is in the navigation. The
brackets are CommonMark's own reference link, since the glossary's definitions are appended to
each document before it is parsed, so an author's own `[interface]: url` wins; a term defined
twice is a failure, as is a fence left open (which would swallow the definitions). The page
lists its terms in the order of the dictionary of the language it is read in.

A `synonym` is another name for a term: a search finds the term by it, `[UI]` in prose is the
term, and the glossary page lists the synonym in its own place, leading to the term. A name — a
term's or a synonym's — which another term has already is a failure. An `unlisted` term is
defined for prose to use, and for every language to say alike, without being listed: it is
not on the glossary page, nor found by a search, and `[padding]` shows its definition but leads
only to its own `link`, where it has one.

A glossary named beneath a section which an Orthant store holds is translated as the
section's documents are: see [Languages](#languages).

## Languages

Documentation is written in one language and translated into others, and changes from one
release to the next. Fuel keeps none of that itself: it reads an
[Orthant](https://github.com/propensive/orthant) store, which holds every document at each
*point* of its axes — a language, a release — as the lines which differ from the point above
it, and derives the plain text of a document at any point. The `orthant` tool writes a store:
it checks a point out into a working tree, shows a translation beside the text it translates,
checks edits back in, and says which passages nobody has translated and which were written
for text which has since changed. None of that is Fuel's, which only reads
(`orthant-reader`), and so has no command which writes a translation.

```
store .                             # the store in `.orthant`, whose tree is `docs`
  at release=1.2.0                  # the release to read; the latest, if nothing says
section guide docs
  metadata section.tel
  overlay api
  glossary glossary.tel
```

The languages are the values of the store's axis of the kind `language`: its root is the
language the documents are written in, and every other value a language they are read in,
`fr/CA` being `fr-CA`. A language reads what nobody has translated as the language above it
has it, and at last as it was written, which is how Orthant derives it; so there is no
fallback to configure, and an unfinished translation is no failure. `fuel compile` says how
far each language is done, as Orthant counts it, and `orthant todo` says where the work is.

Each other axis is read at one value: the one an `at` names, as `at release=1.2.0` or the
value alone where one axis has it (a tag, on an axis which follows a git repository); or
else its last value, where its values are one line, each beneath the one before — the
latest release, or what is not yet released on the branch the axis follows. An axis which
branches has no last value, and wants an `at`.

Fuel reads the store as if it were a tree of its documents with a language checked out: a
document `api/gossamer.tel` is the file of that path in the tree's directory, in every
language, and that is where a problem with it is reported, with the language which reads it
so. The tree is the store's *live tree*, where it has one (`live docs` in the store's
configuration): the directory the documents are written in, which is committed, and which
`orthant tag` stores. Otherwise it is the store's working tree, which a point is checked out
into. Nothing is read from either tree itself, so **an edit is compiled once it is in the
store**: with `orthant tag` for the live tree, and `orthant checkin` for the working tree,
which is still where a translation is written. Where the live tree has edits the store does
not, `fuel compile` says how many documents differ, and that they were read as the store has
them. A store which cannot be opened, an `at` which names no value, and a
document which cannot be derived at a point are failures, each carrying Orthant's own code.

Every file is plain text in every language: a Markdown document is Markdown, and an overlay,
a glossary or a section's file is TEL, with no mark in it of what it translates. Each is held
in every language to what a file of its kind must be — a document's fences compiled, an
overlay or a glossary valid TEL and what its schema allows — and a problem a language's text
has is reported unless the text it was written in has the same one, in a passage nobody has
translated. That a translation says all the text it translates says, and of the text as it
now is, is Orthant's to show.

A glossary in another language is a glossary of the same terms, each named on its `define`
line as the glossary was written names it, so the file itself says which term is which.
Beneath the name, a `term` says what the term is in its own language:

```
define library
  term bibliothèque
  keyword bib
  definition
      Un ensemble de code que d'autres programmes utilisent.
```

An entry with no `term` is called what the original calls it. Its `keyword`s and `synonym`s
are its own language's, in place of the original term's; whether it is `unlisted` is the
original term's, the same in every language. A term whose name is not the one it translates
ends its definition with that name, in the language the glossary was written in where Fuel
knows its name (*En anglais : classloader.*), so that a reader knows the word the original
text uses. Its `link` is the original term's, since a page of the site is the same page in
every language; a `link` is written only for a page elsewhere which another explains in this
language.

A translated document names a term as its own language does, `[bibliothèque]`, and may name
it as the glossary defines it too, `[library]`, as a passage nobody has translated does;
where one name is two terms, it is this language's. The term's anchor on the glossary page is
the original term's in every language, `/glossary#library`. An entry for a term the glossary
does not have, a `term` or a synonym which is a name in its language already, and a `link`
in place of one to a page of the site are reported at their lines.

The dashboard is read in a language. Where the documents are translated, its menu bar offers
the languages they are in, each by its two letters (in full, as `fr-CA`, where two share them),
and a reader's choice is kept for every tool of the hub. Without one, a reader has the
`language` of `~/.config/pyrocosm/config.tel`, then the first their browser asks for which the
documents are in, then the language they are written in.

- A document is shown as the language reads it. One of which nothing is translated is shown
  as it is written, under a note which says so. A translated heading keeps the anchor of the
  heading it translates, so a link to a place in a document means the same in every language.
- An entity's page says what the overlay says of it in the language, and a search's results
  with it. The pages of another language are rendered when they are asked for, their
  examples highlighted and not compiled again: an example is compiled for the language it
  was written in.
- The glossary is the language's own: its page in the order of that language's dictionary,
  each term under its own name, and a search finds a term by its name in either language and
  by its own keywords. A language which has translated none of a glossary reads it as it is
  defined.
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

# The sections of Markdown documents: each a name, which its documents' addresses begin
# with (`/doc/guide/…`), and its directory.
section guide doc

section modules doc/modules

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

# An Orthant store, whose live tree is `docs`: what is configured within that tree is read
# from the store, in each of its languages, at the release named (the latest, if none is).
store .
  at release=1.2.0

# A section may say more: its own file (which says its title), and the overlays and the
# glossaries in its directory. This one is in the store's tree, so it is read in the store's
# languages; one which is not says the language it is written in, if it says one.
section handbook docs/handbook
  metadata section.tel
  overlay api
  glossary glossary.tel

# Serve the documents from the daemon's start, behind the machine's Pyrocosm hub at
# http://localhost:8090/fuel/; `port` would serve them on a port of their own as well.
serve
```

Each setting is also reachable as a flag (`--document doc`), a `fuel.`-prefixed system property
and a `FUEL_`-prefixed environment variable, in that order of priority ahead of the files.

## Modules

- `fuel-code` (`src/code`): the `Embedding` of fenced code alone — a fence's `Preamble`, its
  highlighting and typechecking by a `Checker`, its notes and its animation, as Pyrocosm
  blocks. It knows nothing of documents, of the daemon or of Pyrocosm's tool runtime (it
  depends on Pyrocosm's `model`, `compiler` and `system` modules only), so a build which is
  not a tool's can depend on it to compile samples of code: `Checking.session` opens a
  checker, and an `Embedding.ScalaCode` made with it embeds each fence and collects its
  `problems`. By itself it neither runs a `run` fragment nor links a label's references;
  `Blocks.scala` (in `fuel-core`) makes the embedding the documents use, which does both.
- `fuel-core` (`src/core`): `Doc`, `Corpus`, the `Blocks` conversion of Markdown to Pyrocosm
  blocks (recording every `Linkage` it makes), the `Glossary`, and the `Library` a file is
  read from: the filesystem, or the `Depot` which reads an Orthant store.
- `fuel-api` (`src/api`): the `Api` model of each `Format` (`Apis`), Scala's read from
  compiled code by `Inspection` and a TEL schema's by `TelsFormat`, documented by `Overlay`s,
  with the `Resolver` of references, `Signatures` as tokens, `ApiPages` and the `Links` check.
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
