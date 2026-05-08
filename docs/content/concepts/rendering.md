---
title: Rendering
summary: How blocks and inlines become HTML — escaping, raw HTML, code highlighters, and the XML alternative.
weight: 30
---

`renderToHTML` is the default emitter, but the AST is not bound to it. There's also an XML emitter, and the public helpers (`renderBlockToHTML`, `renderInlines`, `plainText`) let you compose your own pipeline.

## Two entry points

```scala
def renderToHTML(md: String,    config: MarkdownConfig = MarkdownConfig.default): String
def renderToHTML(node: Node,    config: MarkdownConfig = MarkdownConfig.default): String
```

The string overload parses then renders. The node overload skips parsing — useful when you've built or transformed the AST yourself.

## Escape rules

Inline `Text` is escaped via `escapeXml`, which encodes `&`, `<`, `>`, and `"`. Single quotes are not escaped (they're safe in attribute values when those values are double-quoted, which the renderer guarantees).

Code spans and code blocks are escaped the same way — never raw-passed. This is deliberate: an `<code>` span containing `<script>` should render as visible text, not execute.

URLs in `<a href="…">` and `<img src="…">` go through `percentEncode` first. The function intentionally treats `%` as already-safe, so a hand-written `https://example.com/foo%20bar` stays as `%20` rather than getting double-encoded to `%2520`.

## Raw HTML

`HTMLBlock` (block-level) and `RawHTML` (inline) pass through verbatim — no escaping, no transformation. This is the standard CommonMark behavior. If you're rendering untrusted input, sanitize the output yourself; this library does not pretend to be a sanitizer.

## Custom code highlighting

Set `codeHighlighter` on the config to plug in syntax highlighting:

```scala
codeHighlighter: Option[(String, String) => Option[String]]
//                       ^^ code     ^^ language
//                                              ^^ highlighted HTML or None
```

When the function returns `None`, the renderer falls back to plain `<pre><code>…</code></pre>`. When it returns `Some(html)`, that html is dropped in directly — your highlighter is responsible for any escaping.

```scala
import io.github.edadma.markdown.*
import io.github.edadma.highlighter.*

val mode = ClassMode("hl-")
val scalaHl = Highlighter.fromJson(scalaGrammarJson, mode).toOption.get

val cfg = MarkdownConfig.all.copy(
  codeHighlighter      = Some((code, lang) =>
                           if lang == "scala" then Some(scalaHl.highlight(code)) else None
                         ),
  indentedCodeLanguage = Some("scala"), // assume indented blocks are Scala
)

renderToHTML("```scala\nval x = 42\n```", cfg)
```

Works with [highlighter](https://github.com/edadma/highlighter) or any function with the right shape.

## Helpers for AST consumers

The renderer exposes its block- and inline-level workers as public functions:

```scala
def renderBlockToHTML(node: Block, config: MarkdownConfig = MarkdownConfig.default): String
def renderInlines(inlines: List[Inline]): String
def plainText(inlines: List[Inline], escape: Boolean = false): String
```

`renderInlines` is what you reach for to render heading or link-text content as HTML *without* a surrounding `<p>` wrapper — the building block for table-of-contents items, anchor labels, summary excerpts.

`plainText` flattens an inline list to its textual content, stripping all formatting. `escape = true` runs the result through `escapeXml`, which is what you want when emitting into `alt="…"`, `title="…"`, or a `<title>` tag.

## XML output

```scala
def renderToXML(doc: Document, system: String = "document"): String
```

Emits CommonMark XML format with `<?xml … ?>` declaration and a DTD reference. Heading rendering is simplified to plain text — the XML form is intended for spec conformance and AST-diffing tools, not for downstream HTML.

## Heading IDs and slugs

When `autoHeadingIds = true`, the parser populates each heading's `id` from its plain-text content via the `slugify` callback. The renderer just emits whatever's already on the node — it doesn't slug-ify at render time. This means slug behaviour is controlled at *parse* time, and the same `Document` value will always render with the same ids.

Explicit ids set via the `attributes` extension always win:

```markdown
## Hello, World! {#my-explicit-id}
```

renders to `<h2 id="my-explicit-id">Hello, World!</h2>` regardless of whether `autoHeadingIds` is on.

## Render to your own format

The renderers are not magic — they're pattern matches on `Block` and `Inline`. To target a different format, write your own walker. See [Recipes → Custom renderer](/recipes/custom-renderer/) for a worked example.
