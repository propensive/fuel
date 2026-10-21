# Writing documentation for Fuel

A document is a CommonMark file. Fuel converts it, node for node, into Pyrocosm's block model,
so what you write is what every renderer shows: headings, paragraphs, lists, quotations, rules
and code, with *emphasis*, **strong emphasis**, `inline code` and [links](https://soundness.dev/).

## Fenced code

A fence whose info string names a language Fuel knows is highlighted:

```scala
val greeting: Text = t"Hello, world"
val shouted: Text = greeting.upper
```

When a `classpath` is configured, every Scala fence is compiled against it, so a fragment that
does not typecheck fails `fuel compile`, and its error is shown in place.

## A preamble

A fence marked ` ```scala fuel ` begins with a TEL preamble, ended by a line holding `##`:

```scala fuel
id numbers
highlight 1..3  the operands
##
val sum: Int = 1 + 2 + 3
```

`id` names a fragment so that another may `follows` it, with the earlier text in scope;
`context term` or `context type` says the fragment is an expression or a type rather than
top-level definitions; `highlight`, `error`, `caution` and `param` mark a range, named by its
first and last characters (`1..3`) or literally, with a caption after it or beneath it as
`caption` — and, when the range occurs more than once, `line 3` beneath it names the line of
the fragment to look on and `occurrence 2` which of its occurrences is meant; `language java`
says what the fragment is highlighted as; and `errors show` says the fragment is meant not to
compile, so its errors are shown rather than failing the compile:

```scala fuel
follows numbers
errors show
##
val wrong: Text = sum
```

Java is highlighted too:

```java
public final class Greeting {
  public static void main(String[] args) { System.out.println("Hello"); }
}
```

Any other fence is shown plain, under the language it names:

```sh
fuel compile && fuel serve
```

## Animation

A fence with `frame`s is one block that steps through them: each token keeps its identity
across the frames, so a step moves what stays, fades in what arrives and fades out what
departs — wherever the lines put them. A `frame`'s `code` holds its text, and beneath it may
be a `label` (Markdown phrasing, shown beside the step counter), a `hold` (the seconds the
frame rests when the animation plays), an `at` (its time on the page's one clock: a bar at the
foot of the page plays every timed block together, and a frame without a time follows the one
before it by that frame's hold), a `language` of its own, and marks of its own — `highlight`,
`error`, `caution` and `param`, as above, which fade out before a step and in after it; a mark
on the fence itself applies in every frame it matches. Step with the buttons, or the arrow
keys when the block is focused; space plays. The code after `##`, if any, is the last frame.
Each Scala frame is compiled on its own; a Java frame is highlighted.

```scala fuel
id greet
frame
  label A stub
  code
      def greet(name: Text): Text = ???
frame
  label A body
  hold 3
  highlight t"H..me"  the greeting
  code
      def greet(name: Text): Text =
        t"Hello, $name"
frame
  label A parameter, and a line before the body
  code
      def greet(name: Text, times: Int): Text =
        val greeting: Text = t"Hello, $name"
        greeting * times
##
```

## Sessions

A tutorial's fences are usually written as one session, as a REPL's are: a fence uses what an
earlier one defined, and a bare expression is a statement. With `session` in the
configuration, Fuel compiles each document that way — every fence follows the ones before it,
in one object, so a definition is in scope from then on — and compiles the document's fixture
(`<fixtures>/<document>.scala`, when a `fixtures` directory is configured) first. A fence whose
preamble says `restart` begins the session afresh.

A fence may say how far the compiler takes it: `parse` (syntax alone), `typecheck` (the
default when nothing is said) or `compile` (every phase, which is where capture checking
reports); a fence which says a mode is checked by Fuel but never run by a checker which runs
fences, so `typecheck` on its own marks one which opens a socket or a browser. `feature
experimental.captureChecking` turns a language feature on from that fence onward. A fence
joins the session when it compiled as its preamble asked; one marked `errors show` never does,
and one which failed leaves only its imports.

A line ending in `// does not compile` is left out of its fence and compiled alone, in its
place: it must fail, and its error is shown as a note on it.

```scala
val greeting: Text = t"Hello"
```

```scala
greeting.upper
val shouted: Int = greeting // does not compile
```

## What comes next

- the documents in the terminal, and as fragments for other tools;
- the project's API, from its TASTy, with these pages in its tree.
