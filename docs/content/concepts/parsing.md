---
title: Parsing
summary: The pipeline from source string to Document — InputReader, block parsers, inline parsers.
weight: 20
---

The parser is structured as a pipeline. Each stage has one job and produces input for the next:

```
String
   │
   ▼  InputReader
LazyList[C]                    cursors carrying char + position + isLiteral
   │
   ▼  groupIntoLines
LazyList[List[C]]              one list per line, with leading tabs expanded
   │
   ▼  block parsers (priority-ordered registry)
List[Block]                    blocks containing raw List[Inline] of cursors
   │
   ▼  processInlines
Document                       inlines resolved, entities decoded, link refs bound
```

## 1. Input normalization

`new InputReader(input)` produces the cursor stream `LazyList[C]`. Each `C` carries a single character plus its source position, line, column, and an `isLiteral` flag (true if the character was backslash-escaped). The reader normalizes line endings (CR / CRLF → LF) and replaces null with U+FFFD. Backslash escapes of ASCII punctuation are resolved here — the escaped character gets `isLiteral = true` so downstream parsers know not to treat it as syntax.

You won't usually call `InputReader` directly; `parseDocumentContent` wraps it.

## 2. Line grouping

`groupIntoLines` chunks the cursor stream into lines. It uses `LazyList.unfold` to stay lazy, and `expandLeadingTabs` to expand tabs to spaces in leading whitespace only (CommonMark requires column-based indent semantics).

## 3. Block parsing

Block parsers form a priority-ordered registry. For each line, the parser walks the registry calling `canStart()` then `parse()` on each entry. Whichever parser claims the line consumes one or more lines and returns a `Block`. Returning zero lines means "not me, try the next parser." Returning a `null` block means "I consumed lines but produced no output" — the only parser that does this is `LinkReferenceDefinitionParser`, which accumulates link reference definitions into a side table for later resolution.

Priority order (top wins):

1. `LinkReferenceDefinitionParser` — `[label]: dest "title"`
2. `ThematicBreakBlockParser` — `***`, `---`, `___`
3. `ListBlockParser` — unordered (`-`, `+`, `*`) and ordered (`1.`, `1)`)
4. `IndentedCodeBlockParser` — 4+ space indent
5. `SetextHeadingBlockParser` — looks ahead for `===` or `---` underline
6. `ATXHeadingBlockParser` — `#`–`######`
7. `HTMLBlockParser` — types 1–7 per CommonMark §4.6
8. `TableBlockParser` — GFM tables (requires `tables`)
9. `CollapsibleBlockParser` — `::: title` … `:::`
10. `DefinitionListBlockParser` — term + `: definition` (requires `definitionLists`)
11. `FencedCodeBlockParser` — `` ``` `` or `~~~`
12. `MathBlockParser` — `$$` fences (requires `math`)
13. `CalloutBlockParser` — `> [!TYPE]` admonitions (requires `callouts`)
14. `BlockQuoteParser` — `> ` prefixed blocks with lazy continuation
15. `ParagraphBlockParser` — fallback, absorbs continuation lines until interrupted

The fallback at the end is critical: anything not claimed by an earlier parser becomes a `Paragraph`. `ParagraphBlockParser`'s notion of when it gets *interrupted* by a later block construct (ATX heading, thematic break, HTML block 1–6, fence, blockquote, list-that-can-interrupt, math block) is what makes the spec's "no blank line needed" rules work.

## 4. Inline parsing

After the block tree is built, `processInlines(linkRefs, config)` is called on the `Document`. It walks every block recursively and calls `parseInline` on the `List[Inline]` it finds.

`parseInline` is a single-pass algorithm operating on a doubly-linked list. Its main loop checks each `C` cursor against a fixed set of trigger characters in spec-defined precedence order:

| Char       | Triggers                                        | Guarded by                |
|------------|-------------------------------------------------|---------------------------|
| `` ` ``    | code spans (highest precedence)                 | always on                 |
| `:`        | emoji shortcodes                                | `emoji != Disabled`       |
| `$`        | inline math                                     | `math`                    |
| `<`        | autolinks and raw HTML tags                     | always on                 |
| `*`, `_`   | emphasis / strong (deferred via delimiter stack)| always on                 |
| `~`        | strikethrough (delimiter stack)                 | `strikethrough`           |
| `[`        | link / image opener                             | always on                 |
| `]`        | link / image closer                             | always on                 |
| `\n`       | hard or soft line break                         | always on                 |
| `h`, `w`   | extended autolinks (bare URLs)                  | `extendedAutolinks`       |

After the main loop, `processEmphasis` walks the delimiter stack and resolves emphasis pairs using the CommonMark "rule of 3" (`isValidEmphasisPair`). Then `consolidateCharacters` merges adjacent `C` cursors into `Text` nodes, and `decodeHtmlEntities` resolves named, decimal, and hex entity references.

## 5. Link references

Link reference definitions are accumulated during block parsing into an `immutable.Map[String, LinkReference]`. They're passed into `processInlines`, where `lookForLinkOrImage` consults them when resolving full reference (`[text][label]`), collapsed reference (`[text][]`), and shortcut reference (`[text]`) link forms — in addition to inline `[text](url)`.

Labels are normalized via Unicode case-folding (`toLowerCase` plus `ß → ss`) and whitespace collapsing before comparison.

## What you actually call

```scala
import io.github.edadma.markdown.*

// Most common:
val html: String     = renderToHTML(md, config)
val doc:  Document   = parseDocumentContent(md, config)

// Need the link reference table back? (e.g. for cross-document analysis)
val (doc2, refs) = parseDocumentContentWithRefs(md, config)
```

See [Reference → API](/reference/api/) for every entry point with full signatures.
