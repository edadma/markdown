---
title: AST
summary: Document, Block, and Inline — the three families of nodes that come out of parseDocumentContent.
weight: 10
---

The AST is a small set of `case class`es organized into three families. `Document` is the root; it holds a list of `Block`s; each block holds either child blocks or a list of `Inline`s.

## The hierarchy

[= filetree =]
Node (trait)
├── Document
│   └── children: List[Block]
│
├── Block (trait) — anything that can appear at document or list-item level
│   ├── Paragraph(inlines)
│   ├── Heading(level, inlines, attrs?)
│   ├── Code(content, infoString?, indented, attrs?)
│   ├── BlockQuote(children)
│   ├── ThematicBreak
│   ├── HTMLBlock(content)
│   ├── ListBlock(data: ListData, items: List[ListItem])
│   │   └── ListItem(content: List[Block])
│   ├── Table(headerRow, rows, alignments)
│   │   ├── TableRow(cells)
│   │   └── TableCell(content)
│   ├── DefinitionListBlock(items: List[(term, defs)])
│   ├── MathBlock(content)
│   ├── CalloutBlock(calloutType, title?, children)
│   ├── CollapsibleBlock(title, isOpen, children)
│   ├── FootnoteDefinition(label, content)
│   └── DocTagBlock(name, target?, body, contentMode)
│
└── Inline (trait) — anything that can appear inside a paragraph / heading
    ├── Text(content)
    ├── SoftLineBreak
    ├── HardLineBreak
    ├── CodeSpan(content)
    ├── Emphasis(inlines)
    ├── Strong(inlines)
    ├── Strikethrough(inlines)
    ├── Link(destination, title?, inlines)
    ├── Image(destination, title?, inlines, attrs?)
    ├── AutoLink(destination, text)
    ├── RawHTML(content)
    ├── MathExpr(content)
    ├── Emoji(name)
    ├── FootnoteReference(label)
    └── C(char, pos, line, column, isLiteral)
[= /filetree =]

`C` is the cursor type used during parsing — you'll only see it inside `Inline` lists *before* the inline parser has run. After `parseDocumentContent`, every `C` has been folded into `Text`. You won't encounter one in normal use.

## `Document`

```scala
case class Document(children: List[Block]) extends Node {
  def headings: List[Heading]
}
```

`headings` is a convenience accessor — `children.collect { case h: Heading => h }`. Useful for table-of-contents builders, page-title extraction, anchor generators.

## Block nodes

| Node                  | Carries                                                | Renders to                              |
|-----------------------|--------------------------------------------------------|-----------------------------------------|
| `Paragraph`           | `inlines: List[Inline]`                                | `<p>…</p>`                              |
| `Heading`             | `level: Int`, `inlines`, `attrs: Option[Attributes]`   | `<h1>` … `<h6>`                         |
| `Code`                | `content: String`, `infoString: Option[String]`, `indented: Boolean`, `attrs` | `<pre><code class="language-…">` |
| `BlockQuote`          | `children: List[Block]`                                | `<blockquote>…</blockquote>`            |
| `ThematicBreak`       | —                                                      | `<hr />`                                |
| `HTMLBlock`           | `content: String`                                      | passed through verbatim                 |
| `ListBlock`           | `data: ListData`, `items: List[ListItem]`              | `<ul>` / `<ol>` with `<li>` children    |
| `Table`               | `headerRow`, `rows`, `alignments`                      | `<table>` (when `tables` is on)         |
| `DefinitionListBlock` | `items: List[(term, defs)]`                            | `<dl><dt>…<dd>…`                        |
| `MathBlock`           | `content: String`                                      | `<div class="math display">\\[…\\]</div>` |
| `CalloutBlock`        | `calloutType`, `title?`, `children`                    | `<div class="callout callout-…">`       |
| `CollapsibleBlock`    | `title`, `isOpen`, `children`                          | `<details><summary>…</summary>…`        |
| `FootnoteDefinition`  | `label`, `content`                                     | renumbered, emitted at end of document  |
| `DocTagBlock`         | `name`, `target?`, `body`, `contentMode`               | `<dl class="doc-tag doc-tag-…">`        |

`ListData` carries `isOrdered`, `bulletChar`, `startNumber`, `delimiter`, `isTight`, and `indent`. It's how the renderer knows whether to emit `<ul>` or `<ol start="N">` and whether items are tight (no `<p>` wrappers) or loose.

## Inline nodes

| Node                | Carries                                            | Renders to                              |
|---------------------|----------------------------------------------------|-----------------------------------------|
| `Text`              | `content: String`                                  | escaped text                            |
| `SoftLineBreak`     | —                                                  | a literal newline                       |
| `HardLineBreak`     | —                                                  | `<br />`                                |
| `CodeSpan`          | `content: String`                                  | `<code>…</code>`                        |
| `Emphasis`          | `inlines: List[Inline]`                            | `<em>…</em>`                            |
| `Strong`            | `inlines: List[Inline]`                            | `<strong>…</strong>`                    |
| `Strikethrough`     | `inlines: List[Inline]`                            | `<del>…</del>`                          |
| `Link`              | `destination`, `title?`, `inlines`                 | `<a href="…">…</a>`                     |
| `Image`             | `destination`, `title?`, `inlines`, `attrs?`       | `<img src="…" alt="…" />`               |
| `AutoLink`          | `destination`, `text`                              | `<a href="…">…</a>`                     |
| `RawHTML`           | `content: String`                                  | passed through verbatim                 |
| `MathExpr`          | `content: String`                                  | `<span class="math inline">\\(…\\)</span>` |
| `Emoji`             | `name: String`                                     | Unicode glyph or `<img>` (per `EmojiConfig`) |
| `FootnoteReference` | `label: String`                                    | `<sup class="footnote-ref"><a>N</a></sup>` |

## Auxiliary types

```scala
case class Attributes(
  id:       Option[String]      = None,
  classes:  List[String]        = Nil,
  kvPairs:  Map[String, String] = Map.empty,
)

case class LinkReference(destination: String, title: Option[String])

case class ListData(
  isOrdered:   Boolean,
  bulletChar:  Option[Char] = None,
  startNumber: Option[Int]  = None,
  delimiter:   Option[Char] = None,
  isTight:     Boolean      = true,
  indent:      Int,
)

enum TableAlignment:
  case Left, Center, Right, None

enum ContentMode:
  case Opaque         // body stored as a single Text node, no inline parsing
  case InlineMarkdown // body parsed with the inline parser
  case BlockMarkdown  // body parsed with the block parser
```

`Attributes` is populated from the `attributes` extension (`{#id .class key=value}` after a heading, image, or fenced block) and from `autoHeadingIds`.

## The `processInlines` step

Block parsing produces blocks whose inline content is still a raw `List[Inline]` of `C` cursors and the `Inline`s the block-level parsers were able to identify (e.g. raw HTML in HTML blocks). After all blocks are parsed, the parser walks the document calling `node.processInlines(linkRefs, config)` on every node, which:

- runs the inline parser to build `Emphasis` / `Strong` / `Link` / etc. trees;
- consolidates runs of `C` cursors into single `Text` nodes;
- decodes HTML entities and resolves link references against the accumulated `linkRefs` table;
- assigns `id` attributes to headings if `autoHeadingIds` is on.

`parseDocumentContent` wraps this for you. You only need to know it exists if you're building blocks programmatically and want them re-resolved. See [Parsing](/concepts/parsing/) for the pipeline.
