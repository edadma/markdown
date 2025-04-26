# Scala CommonMark

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/markdown_sjs1_3)
![GitHub](https://img.shields.io/github/license/edadma/markdown)
![Scala Version](https://img.shields.io/badge/scala-3.6.4-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.18.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-5.7-blue.svg)
![CommonMark Version](https://img.shields.io/badge/CommonMark-0.31.2-purple.svg)

A fast, minimal **Scala 3** library for parsing and rendering [CommonMark 0.29] Markdown.  
Cross‑platform support: **JVM**, **Scala.js**, and **Scala Native**.

## Features

- **Core CommonMark 0.29 compliance**: ATX & Setext headings, lists, block quotes, fenced & indented code, HTML blocks, thematic breaks, reference definitions
- **Inline parsing**: emphasis, strong, code spans, inline & reference links/images, autolinks, raw HTML, entity references
- **HTML entity decoding** outside of code, with literal preservation inside code spans/blocks
- **HTML rendering**: safe escaping for `<`, `>`, `&`, and `"`; outputs standard tags (`<p>`, `<h1–6>`, `<ul>`, `<ol>`, `<pre><code>`, `<blockquote>`, `<a>`, `<img>`, etc.)
- **Zero runtime dependencies** and lightweight API

## Quickstart

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %% "markdown" % "0.0.1"
```

```scala
import io.github.edadma.markdown._

val md = """
# Hello, CommonMark!

This is **bold**, *italic*, and `code`.
"""

val (doc, _) = parseDocumentContentWithRefs(md)
val html      = renderToHTML(doc)
println(html)
```

## AST Access

The core AST is defined by:

```scala
sealed trait Node
case class Document(children: List[Block]) extends Node
sealed trait Block extends Node
case class Paragraph(inlines: List[Inline]) extends Block
case class Heading(level: Int, inlines: List[Inline]) extends Block
case class Code(content: String, info: Option[String]) extends Block
case class BlockQuote(children: List[Block]) extends Block
case class ListBlock(data: ListData, items: List[ListItem]) extends Block
// … Inline types: Text, Emphasis, Strong, CodeSpan, Link, Image, AutoLink, RawHTML, etc.
```

## Roadmap

- Multi-line link reference definitions (v0.0.2)
- Extensions: footnotes, tables, task lists

## Contributing

1. Fork this repository
2. Create a branch (`git checkout -b feat/awesome`)
3. Commit changes (`git commit -m "Add awesome feature"`)
4. Push and open a Pull Request

Please run `sbt test` and add tests for any new functionality.

## License

This project is licensed under the **ISC License**. See [LICENSE](LICENSE) for details.  
