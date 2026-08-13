# markdown

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/markdown_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/markdown)](https://github.com/edadma/markdown/commits)
![GitHub](https://img.shields.io/github/license/edadma/markdown)
![Scala Version](https://img.shields.io/badge/Scala-3.8.4-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.22.0-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.12-blue.svg)
![npm](https://img.shields.io/npm/v/@edadma/markdown)
![CommonMark Version](https://img.shields.io/badge/CommonMark-0.31.2-purple.svg)

A fast, minimal **Scala 3** library for parsing [CommonMark 0.31.2](https://spec.commonmark.org/0.31.2/) Markdown.
Cross‑platform support: **JVM**, **Scala.js**, and **Scala Native**.

**Full CommonMark 0.31.2 spec compliance** — all 652 spec tests passing across all 26 sections.

## Features

- **100% CommonMark 0.31.2 compliance**: all 652 spec tests passing — ATX & Setext headings, lists (tight/loose), block quotes, fenced & indented code, HTML blocks (types 1–7), thematic breaks, link reference definitions, images, emphasis/strong emphasis, code spans, autolinks, raw HTML, entity references, backslash escapes, hard/soft line breaks
- **HTML entity decoding** outside of code, with literal preservation inside code spans/blocks
- **HTML rendering**: safe escaping for `<`, `>`, `&`, and `"`; outputs standard tags (`<p>`, `<h1–6>`, `<ul>`, `<ol>`, `<pre><code>`, `<blockquote>`, `<a>`, `<img>`, etc.)
- **Zero runtime dependencies** and lightweight API
- **Optional syntax highlighting** for fenced and indented code blocks via a pluggable highlighter function (works with [highlighter](https://github.com/edadma/highlighter) or any custom implementation)

## Documentation

Full reference, AST guide, and configuration index:
**https://edadma.github.io/markdown/**

## Online Demo

Try out the Markdown parser in your browser using the [Dingus](https://edadma.github.io/dingus/).

## Installation

### Scala (sbt)

```scala
libraryDependencies += "io.github.edadma" %%% "markdown" % "0.4.7"
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

### JavaScript / TypeScript (npm)

The library also ships as a published npm package — the linked Scala.js
output, plus TypeScript typings:

```bash
npm install @edadma/markdown
```

```js
import { renderToHTML, extractHeadings, plainText } from "@edadma/markdown"

renderToHTML("# Hello", { autoHeadingIds: true })
// → '<h1 id="hello">Hello</h1>'

extractHeadings("# Intro\n## Setup")
// → [{ level: 1, text: "Intro", id: "intro" },
//    { level: 2, text: "Setup", id: "setup" }]
```

See [`npm/README.md`](npm/README.md) for the full JS/TS API. Maintainers:
`./npm/build.sh` re-links the bundle and copies it into `npm/`; `cd npm &&
npm publish` from there.

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
case class Heading(level: Int, inlines: List[Inline], attrs: Option[Attributes]) extends Block
case class Code(content: String, info: Option[String], indented: Boolean) extends Block
case class BlockQuote(children: List[Block]) extends Block
case class ListBlock(data: ListData, items: List[ListItem]) extends Block
// … Inline types: Text, Emphasis, Strong, CodeSpan, Link, Image, AutoLink, RawHTML, etc.
```

## Helpers for AST consumers

A few public helpers for code that walks the AST (TOC builders, anchor-text
generators, search-index excerpt builders, etc.):

```scala
val doc: Document = parseDocumentContent(md)

// All top-level Heading blocks, in source order.
val hs: List[Heading] = doc.headings

// Plain-text projection of a list of inlines (strips formatting).
val title: String = plainText(hs.head.inlines)

// Render inlines directly to HTML (no surrounding `<p>` wrapper).
val html: String = renderInlines(hs.head.inlines)
```

## Auto-generated heading IDs

Set `autoHeadingIds = true` to have the parser populate every heading's `id`
attribute from its plain-text content. Slugs are pluggable.

```scala
val cfg = MarkdownConfig.default.copy(autoHeadingIds = true)
renderToHTML("## Hello, World!", cfg)
// → <h2 id="hello-world">Hello, World!</h2>

// Custom slug function:
val cfg2 = MarkdownConfig.default.copy(
  autoHeadingIds = true,
  slugify = s => s.toLowerCase.replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_"),
)
```

Explicit ids set via the `attributes` extension (`## Heading {#explicit}`)
always win.

## Configuration

`MarkdownConfig` controls optional features. Use `MarkdownConfig.default` for spec-only behavior or `MarkdownConfig.all` for everything enabled.

| Option | Default | Description |
|--------|---------|-------------|
| `tables` | `false` | GFM-style tables |
| `definitionLists` | `false` | Definition lists |
| `math` | `false` | Math blocks and inline math |
| `callouts` | `false` | Callout blocks |
| `emoji` | `Disabled` | Emoji shortcodes (`Unicode` or `Image(baseURL)`) |
| `strikethrough` | `false` | GFM `~~strikethrough~~` |
| `taskListItems` | `false` | GFM task list items (`- [ ]` / `- [x]`) |
| `extendedAutolinks` | `false` | GFM extended autolinks (bare URLs) |
| `footnotes` | `false` | `[^label]` references with `[^label]: ...` definitions |
| `smartPunctuation` | `false` | Curly quotes, en/em dashes, ellipsis |
| `attributes` | `false` | `{#id .class key=value}` on headings, fenced blocks, images |
| `autoHeadingIds` | `false` | Auto-generate `<hN id="…">` from heading text. Explicit ids (via `attributes`) win. |
| `slugify` | `defaultSlugify` | Pluggable slug function used by `autoHeadingIds` |
| `docTags` | `DocTagConfig.disabled` | Opt-in API doc-tag extension (`@name [target] — body`) |
| `codeHighlighter` | `None` | Pluggable syntax highlighting function |
| `indentedCodeLanguage` | `None` | Default language for indented code blocks |
| `indentedCodeBreaksList` | `false` | Indented code block after a blank line ends the enclosing list item instead of being absorbed into it |

### Doc-tag extension

Enable `docTags` with a caller-supplied `TagRegistry` to parse `@name [target] — body` lines as block-level
`DocTagBlock` AST nodes. Designed for documentation tools built on top of the AST — the processor only parses the
syntax and renders a sensible default (`<dl class="doc-tag doc-tag-{name}">`); binding tags to code declarations,
extracting reference pages, and resolving cross-references happen in downstream tools.

```scala
import io.github.edadma.markdown.*

val registry = TagRegistry(
  TagDefinition("api",    acceptsTarget = false, ContentMode.InlineMarkdown),
  TagDefinition("param",  acceptsTarget = true,  ContentMode.InlineMarkdown),
  TagDefinition("example", acceptsTarget = false, ContentMode.BlockMarkdown),
)

val config = MarkdownConfig(
  docTags = DocTagConfig(enabled = true, registry = registry),
)

renderToHTML("@param msg — the error message\n", config)
```

Each `TagDefinition` specifies whether the tag takes an identifier target and how its body is parsed
(`Opaque`, `InlineMarkdown`, or `BlockMarkdown`). Unknown tags are emitted as lenient `DocTagBlock` nodes by default,
or fall back to plain text with `strictUnknownTags = true`.

## Contributing

1. Fork this repository
2. Create a branch (`git checkout -b feat/awesome`)
3. Commit changes (`git commit -m "Add awesome feature"`)
4. Push and open a Pull Request

Please run `sbt test` and add tests for any new functionality.

## License

This project is licensed under the **ISC License**. See [LICENSE](LICENSE) for details.  
