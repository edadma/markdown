---
title: Quickstart
summary: Render markdown to HTML, walk the AST, enable extensions — the three things every caller does.
weight: 20
---

This walkthrough shows the three things you'll do with the library 95 % of the time: render to HTML, parse to an AST, and turn on extensions.

## 1. Render markdown to HTML

The one-line case:

```scala
import io.github.edadma.markdown.*

renderToHTML(
  """
    |# Hello, CommonMark!
    |
    |This is **bold**, *italic*, and `code`.
    |""".stripMargin
)
```

Output:

```html
<h1>Hello, CommonMark!</h1>
<p>This is <strong>bold</strong>, <em>italic</em>, and <code>code</code>.</p>
```

The default config follows the CommonMark spec exactly — no GFM tables, no math, no emoji. Predictable output for any input.

## 2. Parse to an AST

When you want the structured form rather than HTML:

```scala
import io.github.edadma.markdown.*

val doc: Document = parseDocumentContent("# Hello\n\nWorld.")

// All top-level Heading blocks, in source order.
val titles: List[Heading] = doc.headings

// Plain-text projection of a list of inlines (strips formatting).
val title: String = plainText(titles.head.inlines)
// → "Hello"
```

`Document.children` is a `List[Block]`; each `Heading`, `Paragraph`, `ListBlock`, etc. carries its own typed children. See [AST](/concepts/ast/) for the full hierarchy.

## 3. Turn on extensions

`MarkdownConfig.default` is pure CommonMark. `MarkdownConfig.all` flips on every extension at once:

```scala
val cfg = MarkdownConfig.all

renderToHTML("| a | b |\n|---|---|\n| 1 | 2 |", cfg)
// → "<table>...</table>"
```

Or pick the ones you actually want:

```scala
val cfg = MarkdownConfig.default.copy(
  tables          = true,
  strikethrough   = true,
  taskListItems   = true,
  autoHeadingIds  = true,
)

renderToHTML("## Hello, World!", cfg)
// → <h2 id="hello-world">Hello, World!</h2>
```

The full list of flags lives on the [Options](/reference/options/) page.

## Where to go next

- [Concepts → AST](/concepts/ast/) — the node hierarchy you walk after parsing.
- [Concepts → Rendering](/concepts/rendering/) — escape rules, raw-HTML pass-through, custom highlighters.
- [Reference → API](/reference/api/) — every public function, signature, and return type.
- [Recipes → Custom renderer](/recipes/custom-renderer/) — turn the AST into your own format.
