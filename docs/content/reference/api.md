---
title: API
summary: Every public top-level function in io.github.edadma.markdown — parsing, rendering, helpers.
weight: 10
---

All public functions live in the package object — `import io.github.edadma.markdown.*` and you have everything below.

## Parsing

| Function                            | Returns                                           | Notes                                          |
|-------------------------------------|---------------------------------------------------|------------------------------------------------|
| `parseDocumentContent(input, config?)`        | `Document`                              | Most common entry point. Throws away the link reference table. |
| `parseDocumentContentWithRefs(input, config?)`| `(Document, Map[String, LinkReference])`| Use when you need the link references for cross-document analysis. |
| `parseDocument(stream, config?)`              | `(Document, Map[String, LinkReference])`| Lower level — takes a `LazyList[C]` from `InputReader`. |

```scala
def parseDocumentContent(
    input:  String,
    config: MarkdownConfig = MarkdownConfig.default,
): Document

def parseDocumentContentWithRefs(
    input:  String,
    config: MarkdownConfig = MarkdownConfig.default,
): (Document, Map[String, LinkReference])

def parseDocument(
    stream: LazyList[C],
    config: MarkdownConfig = MarkdownConfig.default,
): (Document, Map[String, LinkReference])
```

## HTML rendering

| Function                                      | Returns  | Notes                                          |
|-----------------------------------------------|----------|------------------------------------------------|
| `renderToHTML(md: String, config?)`           | `String` | Parse + render. The one-line case.             |
| `renderToHTML(node: Node, config?)`           | `String` | Render an existing AST. Errors if `node` is an inline. |
| `renderBlockToHTML(node: Block, config?)`     | `String` | Render a single block — without the trailing newline `renderToHTML` adds. |
| `renderInlines(inlines: List[Inline])`        | `String` | Render inline content with **no** surrounding `<p>`. |
| `plainText(inlines, escape: Boolean = false)` | `String` | Strip all formatting. `escape=true` runs the result through `escapeXml`. |

```scala
def renderToHTML(md:   String, config: MarkdownConfig = MarkdownConfig.default): String
def renderToHTML(node: Node):                                                      String
def renderToHTML(node: Node,   config: MarkdownConfig):                            String

def renderBlockToHTML(node: Block, config: MarkdownConfig = MarkdownConfig.default): String

def renderInlines(inlines: List[Inline]):                              String
def plainText   (inlines: List[Inline], escape: Boolean = false):      String
```

## XML rendering

| Function                                  | Returns  | Notes                                          |
|-------------------------------------------|----------|------------------------------------------------|
| `renderToXML(doc: Document, system?)`     | `String` | Emits CommonMark XML format with `<?xml … ?>` declaration and DTD. Headings rendered as plain text. |
| `escapeXml(s: String)`                    | `String` | Encodes `&`, `<`, `>`, `"`. Used internally; exposed for renderer authors. |

```scala
def renderToXML(doc: Document, system: String = "document"): String
def escapeXml  (s: String):                                  String
```

## URL helpers

| Function                          | Returns  | Notes                                          |
|-----------------------------------|----------|------------------------------------------------|
| `percentEncode(input: String)`    | `String` | URI percent-encoding. Treats `%` as already-safe to avoid double-encoding pre-encoded URLs. |

## AST walking helpers

| Function                                  | Returns                  | Notes                                          |
|-------------------------------------------|--------------------------|------------------------------------------------|
| `Document.headings`                       | `List[Heading]`          | All top-level `Heading` blocks, in source order. |
| `extractHeaders(doc: Document)`           | `List[(Int, String)]`    | `(level, plain-text-content)` pairs.           |
| `inlinesToPlainText(inlines: List[Inline])` | `String`               | Lighter alternative to `plainText` — drops only `Text`, `CodeSpan`, `Emphasis`, `Strong`, and bare `C` cursors. |
| `plainText(inlines, escape?)`             | `String`                 | Recommended. Handles every inline type and strips images, links, math, etc., to their text content. |

```scala
def extractHeaders(document: Document):       List[(Int, String)]
def inlinesToPlainText(inlines: List[Inline]): String
```

## Input layer

You'll rarely call these directly — `parseDocumentContent` wraps them — but they're public for renderer / formatter authors:

| Function                                  | Returns           | Notes                                          |
|-------------------------------------------|-------------------|------------------------------------------------|
| `new InputReader(input).stream`           | `LazyList[C]`     | The cursor stream. Normalizes line endings and resolves backslash escapes. |
| `rawText(cursors: List[C])`               | `String`          | Reconstruct source text, restoring backslashes for literal-flagged characters. Used by code-block parsers. |
| `expandLeadingTabs(line, startCol = 0)`   | `List[C]`         | Expand tabs to spaces in leading whitespace only. |
| `virtualIndent(line: List[C])`            | `Int`             | Tab-aware indent count of a line.              |
| `dropIndent(line, n, startCol = 0)`       | `List[C]`         | Remove `n` columns of (tab-aware) leading indent. |

## Config

`MarkdownConfig` is one big `case class` with one `copy` constructor. Two prefab values:

| Value                       | Meaning                                  |
|-----------------------------|------------------------------------------|
| `MarkdownConfig.default`    | Pure CommonMark — no extensions enabled. |
| `MarkdownConfig.all`        | Every extension enabled.                 |

See [Options](/reference/options/) for the per-flag table.

## Doc-tag extension

```scala
case class TagDefinition(
  name:          String,
  acceptsTarget: Boolean,
  contentMode:   ContentMode,
)

final class TagRegistry:
  def lookup(name: String): Option[TagDefinition]
  def isEmpty:  Boolean
  def nonEmpty: Boolean
  def all:      Iterable[TagDefinition]

object TagRegistry:
  val empty: TagRegistry
  def apply(defs: TagDefinition*):           TagRegistry
  def fromList(defs: Iterable[TagDefinition]): TagRegistry

enum ContentMode:
  case Opaque, InlineMarkdown, BlockMarkdown

case class DocTagConfig(
  enabled:           Boolean      = false,
  registry:          TagRegistry  = TagRegistry.empty,
  strictUnknownTags: Boolean      = false,
)

object DocTagConfig:
  val disabled: DocTagConfig
```

See [Extensions → Doc-tags](/reference/extensions/#doc-tags) for the integration model.
