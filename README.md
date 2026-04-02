# markdown

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/markdown_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/markdown)](https://github.com/edadma/markdown/commits)
![GitHub](https://img.shields.io/github/license/edadma/markdown)
![Scala Version](https://img.shields.io/badge/Scala-3.8.2-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)
![CommonMark Version](https://img.shields.io/badge/CommonMark-0.31.2-purple.svg)

A fast, minimal **Scala 3** library for parsing [CommonMark 0.31.2](https://spec.commonmark.org/0.31.2/) Markdown.
Cross‑platform support: **JVM**, **Scala.js**, and **Scala Native**.

**Full CommonMark 0.31.2 spec compliance** — all 877 spec tests passing across all 25 sections.

## Features

- **100% CommonMark 0.31.2 compliance**: all 877 spec tests passing — ATX & Setext headings, lists (tight/loose), block quotes, fenced & indented code, HTML blocks (types 1–7), thematic breaks, link reference definitions, images, emphasis/strong emphasis, code spans, autolinks, raw HTML, entity references, backslash escapes, hard/soft line breaks
- **HTML entity decoding** outside of code, with literal preservation inside code spans/blocks
- **HTML rendering**: safe escaping for `<`, `>`, `&`, and `"`; outputs standard tags (`<p>`, `<h1–6>`, `<ul>`, `<ol>`, `<pre><code>`, `<blockquote>`, `<a>`, `<img>`, etc.)
- **Zero runtime dependencies** and lightweight API
- **Optional syntax highlighting** for fenced and indented code blocks via a pluggable highlighter function (works with [highlighter](https://github.com/edadma/highlighter) or any custom implementation)

## Online Demo

Try out the Markdown parser in your browser using the [Dingus](https://edadma.github.io/dingus/).

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %% "markdown" % "0.2.0"
```

```scala
import io.github.edadma.markdown._

val md = """
# Hello, CommonMark!

This is **bold**, *italic*, and `code`.
"""

val html = renderToHTML(md)

println(html)
```

## Code Highlighting

Fenced and indented code blocks can be syntax-highlighted by providing a highlighter function. Works with [highlighter](https://github.com/edadma/highlighter) or any custom implementation.

```scala
import io.github.edadma.markdown.*
import io.github.edadma.highlighter.*

// Parse grammars once, cache per language
val mode = ClassMode("hl-")
val highlighters = Map(
  "scala" -> Highlighter.fromJson(scalaGrammarJson, mode).toOption.get,
  "js"    -> Highlighter.fromJson(jsGrammarJson, mode).toOption.get,
)

val config = MarkdownConfig.all.copy(
  codeHighlighter = Some((code, lang) => highlighters.get(lang).map(_.highlight(code))),
  indentedCodeLanguage = Some("scala"), // optional: assume indented blocks are Scala
)

val html = renderToHTML("```scala\nval x = 42\n```", config)
```

## AST Access

The core AST is defined by:

```scala
sealed trait Node
case class Document(children: List[Block]) extends Node
sealed trait Block extends Node
case class Paragraph(inlines: List[Inline]) extends Block
case class Heading(level: Int, inlines: List[Inline]) extends Block
case class Code(content: String, info: Option[String], indented: Boolean) extends Block
case class BlockQuote(children: List[Block]) extends Block
case class ListBlock(data: ListData, items: List[ListItem]) extends Block
// … Inline types: Text, Emphasis, Strong, CodeSpan, Link, Image, AutoLink, RawHTML, etc.
```

## Extensions

Beyond CommonMark, the library also supports:

- Tables (GFM-style)
- Definition lists
- Math blocks and inline math
- Callout blocks
- Collapsible blocks
- Emoji shortcodes

## Contributing

1. Fork this repository
2. Create a branch (`git checkout -b feat/awesome`)
3. Commit changes (`git commit -m "Add awesome feature"`)
4. Push and open a Pull Request

Please run `sbt test` and add tests for any new functionality.

## License

This project is licensed under the **ISC License**. See [LICENSE](LICENSE) for details.  
