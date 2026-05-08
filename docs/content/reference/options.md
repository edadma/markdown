---
title: Options
summary: The full MarkdownConfig surface — every flag, callback, and prefab — in one table.
weight: 30
---

`MarkdownConfig` is the single source of truth for parser and renderer behaviour. Construct one explicitly, or copy `MarkdownConfig.default` (pure CommonMark) or `MarkdownConfig.all` (every extension on).

## Prefabs

| Value                       | Meaning                                                              |
|-----------------------------|----------------------------------------------------------------------|
| `MarkdownConfig.default`    | Pure CommonMark. No extensions. Predictable spec-only output.        |
| `MarkdownConfig.all`        | Every boolean extension on; `emoji = Unicode`; `autoHeadingIds = true`. |

## Full reference

| Option                     | Type                                              | Default              | Notes                                                                  |
|----------------------------|---------------------------------------------------|----------------------|------------------------------------------------------------------------|
| `tables`                   | `Boolean`                                         | `false`              | GFM-style pipe tables.                                                 |
| `definitionLists`          | `Boolean`                                         | `false`              | `Term\n: defn` style definition lists.                                 |
| `math`                     | `Boolean`                                         | `false`              | `$$…$$` block math, `$…$` inline math.                                 |
| `callouts`                 | `Boolean`                                         | `false`              | `> [!TYPE]` admonition blocks.                                         |
| `strikethrough`            | `Boolean`                                         | `false`              | GFM `~~strikethrough~~`.                                               |
| `taskListItems`            | `Boolean`                                         | `false`              | GFM `- [ ]` / `- [x]` checkbox list items.                             |
| `extendedAutolinks`        | `Boolean`                                         | `false`              | Bare URL / `www.` autolinks (no `<>` brackets needed).                 |
| `footnotes`                | `Boolean`                                         | `false`              | `[^label]` references with `[^label]: …` definitions.                  |
| `smartPunctuation`         | `Boolean`                                         | `false`              | Curly quotes, en/em-dashes, ellipsis.                                  |
| `attributes`               | `Boolean`                                         | `false`              | `{#id .class key=value}` on headings, fenced blocks, images.           |
| `autoHeadingIds`           | `Boolean`                                         | `false`              | Auto-populate heading `id`s. Explicit ids (via `attributes`) win.      |
| `slugify`                  | `String => String`                                | `defaultSlugify`     | Slug function used by `autoHeadingIds`.                                |
| `emoji`                    | `EmojiConfig`                                     | `Disabled`           | `Disabled` / `Unicode` / `Image(baseURL)`.                             |
| `docTags`                  | `DocTagConfig`                                    | `disabled`           | `@name [target] — body` annotation parsing.                            |
| `codeHighlighter`          | `Option[(String, String) => Option[String]]`      | `None`               | Pluggable code-block syntax highlighter.                               |
| `indentedCodeLanguage`     | `Option[String]`                                  | `None`               | Default language tag applied to indented code blocks.                  |
| `indentedCodeBreaksList`   | `Boolean`                                         | `false`              | If true, an indented code block after a blank line ends the enclosing list item instead of being absorbed. |

## `EmojiConfig`

```scala
enum EmojiConfig:
  case Disabled
  case Unicode
  case Image(baseURL: String)
```

`Unicode` substitutes the Unicode glyph (e.g. `:smile:` → `😄`). `Image("/emoji")` substitutes `<img src="/emoji/smile.svg" alt=":smile:" />` — the `baseURL` is prefixed verbatim.

## `slugify`

Default behaviour (`MarkdownConfig.defaultSlugify`):

- lowercase the input
- collapse runs of non-alphanumeric characters into a single `-`
- strip leading and trailing `-`
- return `""` if the input has no letter/digit content

Same algorithm Hugo, mkdocs, and GitHub use. Override when your URL scheme differs:

```scala
val cfg = MarkdownConfig.default.copy(
  autoHeadingIds = true,
  slugify = s => s.toLowerCase.replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_"),
)
```

## `codeHighlighter`

```scala
codeHighlighter: Option[(String, String) => Option[String]]
```

The function receives the raw code body and the language tag (from the info string, or from `indentedCodeLanguage` for indented blocks). Return `Some(html)` to inject custom HTML inside `<code>…</code>`; return `None` to fall back to the default `<pre><code>{escaped}</code></pre>` output.

The renderer does **not** escape your highlighter's output — it's assumed to be valid HTML already.

## `DocTagConfig`

```scala
case class DocTagConfig(
  enabled:           Boolean      = false,
  registry:          TagRegistry  = TagRegistry.empty,
  strictUnknownTags: Boolean      = false,
)
```

See [Extensions → Doc-tags](/reference/extensions/#doc-tags) for the integration model and a full example.

## Worked configurations

### "Hugo-flavoured": tables, autolinks, smart quotes, heading ids

```scala
val cfg = MarkdownConfig.default.copy(
  tables             = true,
  extendedAutolinks  = true,
  smartPunctuation   = true,
  autoHeadingIds     = true,
)
```

### "Obsidian-flavoured": callouts, footnotes, math, tables

```scala
val cfg = MarkdownConfig.default.copy(
  tables    = true,
  callouts  = true,
  footnotes = true,
  math      = true,
)
```

### "GFM-flavoured": tables, strikethrough, task lists, autolinks

```scala
val cfg = MarkdownConfig.default.copy(
  tables             = true,
  strikethrough      = true,
  taskListItems      = true,
  extendedAutolinks  = true,
)
```

### Everything

```scala
val cfg = MarkdownConfig.all
```
