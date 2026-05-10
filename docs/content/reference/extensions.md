---
title: Extensions
summary: Every opt-in syntax extension — tables, math, callouts, footnotes, emoji, doc-tags, and more.
weight: 20
---

CommonMark itself is conservative: anything not in the spec is off by default. The library ships every popular extension as a flag on `MarkdownConfig`. Set `MarkdownConfig.all` to turn them all on at once, or copy the default and enable just what you need.

## Tables (`tables`)

GFM-style pipe tables. Header row, separator row, body rows. Per-column alignment via `:---`, `:---:`, `---:`.

```markdown
| Name     | Status |
|----------|:------:|
| ready    |   ✓    |
| pending  |   …    |
```

## Strikethrough (`strikethrough`)

GFM `~~text~~` renders to `<del>text</del>`. Two-tilde delimiters; a single `~` does nothing.

```markdown
This is ~~deleted~~ text.
```

## Task list items (`taskListItems`)

GFM `- [ ]` and `- [x]` (or `- [X]`) at the start of a list item render with a disabled checkbox. Works in both ordered and unordered lists.

```markdown
- [x] write docs
- [ ] ship docs
```

## Extended autolinks (`extendedAutolinks`)

Bare URLs and `www.`-prefixed hostnames become links without `<>` brackets:

```markdown
visit https://example.com or www.example.com
```

## Footnotes (`footnotes`)

`[^label]` references with `[^label]: …` definitions. References are renumbered in document order; definitions are collected and emitted as a `<section class="footnotes">` at the end of the document, with backrefs.

```markdown
Here is a claim.[^src]

[^src]: Source: *The Big Book of Claims*, p. 42.
```

## Math (`math`)

Block math with `$$…$$` fences and inline math with `$…$`. Renders to `<div class="math display">\[…\]</div>` and `<span class="math inline">\(…\)</span>` — pair with KaTeX or MathJax client-side.

````markdown
The Pythagorean theorem: $a^2 + b^2 = c^2$.

$$
\int_0^\infty e^{-x^2}\,dx = \frac{\sqrt{\pi}}{2}
$$
````

This site has KaTeX wired up (juicerdocs theme, `[juicerdocs] math = true` in `site.toml`), so the same source above renders as:

The Pythagorean theorem: $a^2 + b^2 = c^2$.

$$
\int_0^\infty e^{-x^2}\,dx = \frac{\sqrt{\pi}}{2}
$$

A few more shapes — quadratic formula, a matrix, a sum, and a limit:

$$
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}
$$

$$
A = \begin{pmatrix} a_{11} & a_{12} \\ a_{21} & a_{22} \end{pmatrix}
$$

$$
\sum_{n=1}^{\infty} \frac{1}{n^2} = \frac{\pi^2}{6}
$$

$$
\lim_{x \to 0} \frac{\sin x}{x} = 1
$$

## Callouts (`callouts`)

Obsidian / GFM admonition syntax: a blockquote whose first line is `[!TYPE]` or `[!TYPE] Title`. Common types: `note`, `tip`, `info`, `warning`, `danger`.

```markdown
> [!warning] Heads up
> This kills the build.
```

Renders as `<div class="callout callout-warning">…</div>` with a `<div class="callout-title">` for the title and `<div class="callout-content">` for the body.

## Smart punctuation (`smartPunctuation`)

`"..."` becomes `“…”`, `'...'` becomes `‘…’`, `--` becomes `–` (en-dash), `---` becomes `—` (em-dash), `...` becomes `…` (ellipsis). Off by default to keep CommonMark output character-for-character predictable.

## Attributes (`attributes`)

`{#id .class key=value}` after a heading, fenced code block, or image populates the node's `Attributes` value. Multiple classes and arbitrary key-value pairs are supported.

```markdown
## Hello {#hello-id .lead key="some value"}

![alt](/img.png){.figure width=400}

```scala {#example1 .runnable}
val x = 42
```
```

## Auto heading IDs (`autoHeadingIds`)

Every heading gets an `id` attribute generated from its plain-text content via `slugify`. Default algorithm: lowercase, runs of non-alphanumerics → `-`, strip leading/trailing `-`. Same algorithm Hugo, mkdocs, and GitHub use.

```scala
val cfg = MarkdownConfig.default.copy(autoHeadingIds = true)
renderToHTML("## Hello, World!", cfg)
// → <h2 id="hello-world">Hello, World!</h2>
```

Plug in a custom slug function:

```scala
val cfg = MarkdownConfig.default.copy(
  autoHeadingIds = true,
  slugify = s => s.toLowerCase.replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_"),
)
```

Explicit ids set via the `attributes` extension (`## Heading {#explicit}`) always win.

## Emoji (`emoji`)

Three modes:

| Config                        | Behavior |
|-------------------------------|----------|
| `EmojiConfig.Disabled`        | `:smile:` is left as text. |
| `EmojiConfig.Unicode`         | `:smile:` becomes `😄`. |
| `EmojiConfig.Image(baseURL)`  | `:smile:` becomes `<img src="{baseURL}/smile.svg" />`. |

The shortcode set is the standard GitHub / Slack list.

## Definition lists (`definitionLists`)

A term followed by one or more `: definition` lines:

```markdown
Term
: A succinct definition.
: Another sense of the term.
```

Renders to `<dl><dt>Term</dt><dd>…</dd><dd>…</dd></dl>`.

## Indented code language (`indentedCodeLanguage`)

Indented code blocks (4-space) carry no language tag in CommonMark. Set `indentedCodeLanguage = Some("scala")` (or any other) to default them to a language for highlighting:

```scala
val cfg = MarkdownConfig.default.copy(
  indentedCodeLanguage = Some("scala"),
  codeHighlighter      = Some(myHighlighter),
)
```

## Indented code breaks list (`indentedCodeBreaksList`)

A subtle one. CommonMark says an indented code block following a blank line *inside a list item* is absorbed into the list item. Set `indentedCodeBreaksList = true` to make the indented code block instead **end** the enclosing list item. Useful when you're using indented blocks as section breaks rather than list-internal code.

## Doc-tags (`docTags`)

Block-level `@name [target] — body` annotations. Designed for documentation tools built on top of the AST. The processor parses the syntax and renders a sensible default (`<dl class="doc-tag doc-tag-{name}">`); binding tags to code declarations and resolving cross-references is the downstream tool's job.

```scala
import io.github.edadma.markdown.*

val registry = TagRegistry(
  TagDefinition("api",     acceptsTarget = false, ContentMode.InlineMarkdown),
  TagDefinition("param",   acceptsTarget = true,  ContentMode.InlineMarkdown),
  TagDefinition("example", acceptsTarget = false, ContentMode.BlockMarkdown),
)

val cfg = MarkdownConfig.default.copy(
  docTags = DocTagConfig(enabled = true, registry = registry),
)

renderToHTML("@param msg — the error message\n", cfg)
```

`ContentMode` controls how each tag's body is parsed:

| Mode                | Body is                                                       |
|---------------------|---------------------------------------------------------------|
| `Opaque`            | a single `Text` node, not parsed                              |
| `InlineMarkdown`    | parsed by the inline parser only (becomes one `Paragraph`)    |
| `BlockMarkdown`     | parsed by the block parser (arbitrary block content)          |

`strictUnknownTags = true` causes unknown tag names to fall back to ordinary text. With the default (`false`), unknown tags are still emitted as `DocTagBlock` with `acceptsTarget = true` and `InlineMarkdown` mode — convenient for forward-compatible doc pipelines.

## Combined: everything on

```scala
val cfg = MarkdownConfig.all
//                              ^ tables, math, callouts, strikethrough, taskListItems,
//                                extendedAutolinks, footnotes, smartPunctuation,
//                                attributes, definitionLists, autoHeadingIds, emoji=Unicode
```

Note that `MarkdownConfig.all` does **not** turn on `docTags` or set `codeHighlighter` / `indentedCodeLanguage` / `indentedCodeBreaksList` — those need explicit configuration values, not just a boolean.
