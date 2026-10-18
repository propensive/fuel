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
`caption`; and `errors show` says the fragment is meant not to compile, so its errors are shown
rather than failing the compile:

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

## What comes next

- a sequence of frames, animated step by step;
- the documents in the terminal, and as fragments for other tools;
- the project's API, from its TASTy, with these pages in its tree.
