# Writing documentation for Fuel

A document is a CommonMark file. Fuel converts it, node for node, into Pyrocosm's block model,
so what you write is what every renderer shows: headings, paragraphs, lists, quotations, rules
and code, with *emphasis*, **strong emphasis**, `inline code` and [links](https://soundness.dev/).

## Fenced code

A fence whose info string names a language Fuel knows is highlighted:

```scala
import soundness.*

@main
def run(): Unit =
  val greeting: Text = t"Hello, world"
  Out.println(greeting)
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

- a TEL preamble on a fence, naming the fragment's context and the ranges to annotate;
- every Scala fragment compiled against the project's classpath;
- a sequence of frames, animated step by step.
