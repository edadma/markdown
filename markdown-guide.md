# Markdown Library Programmer's Guide

This guide provides an overview and usage instructions for the `io.github.edadma.markdown` library, a Scala implementation of the CommonMark specification with extensions like tables.

## Installation

Add the following dependency to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "markdown" % "0.0.1"
```

The library is cross-built for Scala JVM, Scala.js, and Scala Native platforms.

## Basic Usage

### Parsing Markdown

The simplest way to parse Markdown to an AST:

```scala
import io.github.edadma.markdown._

// Parse Markdown into a Document AST
val document = parseDocumentContent("# Hello, *world*!")
```

If you need link references:

```scala
// Get both document and link references
val (doc, linkRefs) = parseDocumentContentWithRefs("# Hello\n\n[ref]: /url")
```

### Rendering to HTML

To render Markdown as HTML:

```scala
import io.github.edadma.markdown._

// Parse and render in one step
val html = renderToHTML("# Hello, *world*!")

// Or render an existing Document
val document = parseDocumentContent("# Hello, *world*!")
val html = renderToHTML(document)
```

### Extracting Headers

Get all headings for a table of contents:

```scala
val document = parseDocumentContent("# Title\n## Section\n### Subsection")
val headers = extractHeaders(document)  // List[(Int, String)] of (level, text)

// Output the headers
headers.foreach { case (level, text) =>
  println(s"${"\t" * (level - 1)}$text")
}
```

## Core API

### Main Functions

| Function | Description |
|----------|-------------|
| `parseDocumentContent(input: String): Document` | Parse Markdown to a Document AST |
| `parseDocumentContentWithRefs(input: String): (Document, Map[String, LinkReference])` | Parse Markdown, returning Document and link references |
| `renderToHTML(md: String): String` | Parse and render Markdown to HTML |
| `renderToHTML(document: Document): String` | Render a Document AST to HTML |
| `renderToXML(doc: Document, system: String = "document"): String` | Render a Document AST to XML (useful for debugging) |
| `extractHeaders(document: Document): List[(Int, String)]` | Extract all headings with their levels |
| `inlinesToPlainText(inlines: List[Inline]): String` | Convert inline elements to plain text |
| `escapeXml(s: String): String` | Escape special characters for XML/HTML output |

### Document Model

The AST is structured hierarchically around these core types:

- `Document(children: List[Block])` - Container for all blocks in a document
- `Block` - Base trait for all block-level elements
- `Inline` - Base trait for all inline-level elements

#### Block-Level Elements

| Class | Description |
|-------|-------------|
| `Paragraph(inlines: List[Inline])` | A paragraph of text |
| `Heading(level: Int, inlines: List[Inline])` | A heading with level 1-6 |
| `BlockQuote(children: List[Block])` | A block quote containing other blocks |
| `Code(content: String, infoString: Option[String])` | A code block with optional language info |
| `ThematicBreak()` | A horizontal rule (HR) |
| `HTMLBlock(content: String)` | A raw HTML block |

#### List-Related Elements

| Class | Description |
|-------|-------------|
| `ListBlock(data: ListData, items: List[ListItem])` | A list (ordered or unordered) |
| `ListItem(content: List[Block])` | An item in a list |
| `ListData(isOrdered, bulletChar, startNumber, delimiter, isTight, indent)` | Metadata about a list's structure |

#### Table-Related Elements

| Class | Description |
|-------|-------------|
| `Table(headerRow, rows, alignments)` | A table with headers, rows, and column alignments |
| `TableRow(cells: List[TableCell])` | A row in a table |
| `TableCell(content: List[Inline])` | A cell in a table |
| `TableAlignment` (enum) | Alignment types: Left, Center, Right, None |

#### Inline-Level Elements

| Class | Description |
|-------|-------------|
| `Text(content: String)` | Plain text |
| `Emphasis(inlines: List[Inline])` | Emphasized text (*text*) |
| `Strong(inlines: List[Inline])` | Strongly emphasized text (**text**) |
| `CodeSpan(content: String)` | Inline code (`code`) |
| `Link(destination, title, inlines)` | A hyperlink |
| `Image(destination, title, inlines)` | An image |
| `AutoLink(destination, text)` | An autolink (<url>) |
| `SoftLineBreak()` | A newline in source rendered as space |
| `HardLineBreak()` | A forced line break |
| `RawHTML(content: String)` | Inline HTML |

#### Other Types

| Type | Description |
|------|-------------|
| `LinkReference(destination, title)` | Reference-style link definition |
| `C(char, pos, line, column, isLiteral)` | Character cursor for tracking position in source |

## Working with the AST

### Walking the Document Tree

You can traverse the document structure recursively:

```scala
def walkDocument(doc: Document): Unit = {
  def walkNode(node: Node): Unit = node match {
    case Document(children) => 
      println("Document:")
      children.foreach(walkNode)
    
    case Heading(level, inlines) =>
      println(s"Heading (level $level): ${inlinesToPlainText(inlines)}")
    
    case Paragraph(inlines) =>
      println(s"Paragraph: ${inlinesToPlainText(inlines)}")
    
    case BlockQuote(children) =>
      println("BlockQuote:")
      children.foreach(walkNode)
    
    case ListBlock(data, items) =>
      println(s"${if (data.isOrdered) "Ordered" else "Unordered"} List:")
      items.foreach { item =>
        println("  - ListItem:")
        item.content.foreach(b => walkNode(b))
      }
    
    case Code(content, infoString) =>
      println(s"Code block${infoString.map(s => s" ($s)").getOrElse("")}: $content")
    
    case other => println(s"Other node: $other")
  }
  
  walkNode(doc)
}
```

### Finding Specific Content

Extract specific content from the document:

```scala
// Find all links in a document
def findLinks(doc: Document): List[Link] = {
  val links = collection.mutable.ListBuffer[Link]()
  
  def searchNode(node: Node): Unit = node match {
    case Link(dest, title, inlines) => 
      links += Link(dest, title, inlines)
    
    case Document(children) => children.foreach(searchNode)
    case Paragraph(inlines) => inlines.foreach(searchNode)
    case Heading(_, inlines) => inlines.foreach(searchNode)
    case Emphasis(inlines) => inlines.foreach(searchNode)
    case Strong(inlines) => inlines.foreach(searchNode)
    case b: BlockQuote => b.children.foreach(searchNode)
    case lb: ListBlock => lb.items.foreach(item => item.content.foreach(searchNode))
    
    // Handle other nodes as needed
    case _ => // Skip other node types
  }
  
  searchNode(doc)
  links.toList
}
```

### Transforming the AST

The AST is immutable, so transformations create new nodes:

```scala
// Add a custom class to all code blocks
def addClassToCodeBlocks(doc: Document, className: String): Document = {
  def transformBlock(block: Block): Block = block match {
    case c: Code =>
      val newInfoString = c.infoString match {
        case Some(info) => Some(s"$info $className")
        case None => Some(className)
      }
      c.copy(infoString = newInfoString)
    
    case bq: BlockQuote =>
      BlockQuote(bq.children.map(transformBlock))
    
    case lb: ListBlock =>
      val newItems = lb.items.map(item => 
        ListItem(item.content.map(transformBlock))
      )
      ListBlock(lb.data, newItems)
    
    // Transform other block types as needed
    case other => other
  }
  
  Document(doc.children.map(transformBlock))
}
```

## Common Tasks

### Adding a Custom Header ID

```scala
// Add a custom ID attribute to a heading when rendering
def customRenderHeading(heading: Heading): String = {
  val text = inlinesToPlainText(heading.inlines)
  val id = text.toLowerCase.replaceAll("[^a-z0-9]+", "-").trim('-')
  s"""<h${heading.level} id="$id">${renderInlines(heading.inlines)}</h${heading.level}>"""
}
```

### Syntax Highlighting for Code Blocks

```scala
// Custom renderer with syntax highlighting for code blocks
def renderCodeBlockWithHighlighting(code: Code): String = {
  val content = escapeXml(code.content)
  val langClass = code.infoString
    .map(info => s""" class="language-${info.split(' ').head}"""")
    .getOrElse("")
  
  s"""<pre><code$langClass>$content</code></pre>"""
}
```

### Custom Table Rendering

```scala
// Custom HTML table renderer with additional classes
def renderCustomTable(table: Table): String = {
  val alignAttrs = table.alignments.map {
    case TableAlignment.Left => """ align="left""""
    case TableAlignment.Right => """ align="right""""
    case TableAlignment.Center => """ align="center""""
    case TableAlignment.None => ""
  }
  
  val headerCells = table.headerRow.cells.zip(alignAttrs).map { case (cell, align) =>
    s"""<th$align>${renderInlines(cell.content)}</th>"""
  }.mkString

  val rows = table.rows.map { row =>
    val cells = row.cells.zip(alignAttrs).map { case (cell, align) =>
      s"""<td$align>${renderInlines(cell.content)}</td>"""
    }.mkString
    s"<tr>$cells</tr>"
  }.mkString
  
  s"""<table class="markdown-table">
     |  <thead>
     |    <tr>$headerCells</tr>
     |  </thead>
     |  <tbody>
     |    $rows
     |  </tbody>
     |</table>""".stripMargin
}
```

## Advanced Usage

### Custom Block Parser

To extend the library with a custom block parser:

```scala
import scala.collection.mutable

object CustomDivBlockParser extends BlockParser {
  val name: String = "custom divs"
  
  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.isEmpty) return false
    
    val line = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    line.trim.startsWith(":::") 
  }
  
  def parse(
    lines: List[LazyList[C]], 
    linkRefs: mutable.Map[String, LinkReference]
  ): (Block, Int) = {
    var currentLine = 0
    val content = new StringBuilder
    var foundClosing = false
    
    // Get div type
    val firstLine = lines(currentLine)
      .takeWhile(_.char != '\n').map(_.char).mkString.trim
    val divType = firstLine.stripPrefix(":::")
    currentLine += 1
    
    // Collect all lines until closing marker
    while (currentLine < lines.size && !foundClosing) {
      val line = lines(currentLine)
        .takeWhile(_.char != '\n').map(_.char).mkString
      
      if (line.trim == ":::") {
        foundClosing = true
      } else {
        content.append(line).append('\n')
      }
      currentLine += 1
    }
    
    // Process the content recursively
    val reader = new InputReader(content.toString)
    val (innerDoc, _) = parseDocument(reader.stream)
    
    // Return a custom HTML block with div wrapper
    val html = s"""<div class="$divType">
                  |${renderToHTML(innerDoc)}
                  |</div>""".stripMargin
                  
    (HTMLBlock(html), currentLine)
  }
}

// Add the custom parser to the list of block parsers
blockParsers.prepend(CustomDivBlockParser)
```

### Custom Inline Renderer

To customize how inline elements are rendered:

```scala
def customRenderInlines(inlines: List[Inline]): String = {
  inlines.map {
    case Text(content) => escapeXml(content)
    case Emphasis(children) => s"<em class='custom-em'>${customRenderInlines(children)}</em>"
    case Strong(children) => s"<strong class='custom-strong'>${customRenderInlines(children)}</strong>"
    case Link(dest, title, children) =>
      val titleAttr = title.map(t => s""" title="${escapeXml(t)}"""").getOrElse("")
      val classes = if (dest.startsWith("http")) "external-link" else "internal-link"
      s"""<a href="${escapeXml(dest)}"$titleAttr class="$classes">${customRenderInlines(children)}</a>"""
    case other => renderToHTML(other) // Fall back to default rendering
  }.mkString
}
```

## Performance Considerations

- The parser uses lazy lists to avoid loading the entire document into memory at once
- For large documents, consider processing in chunks if possible
- If parsing multiple documents, reuse the same parsers to avoid initialization overhead

## Error Handling

The parser is designed to handle malformed input gracefully:

```scala
def safeParseMarkdown(input: String): Document = {
  try {
    parseDocumentContent(input)
  } catch {
    case e: Exception =>
      // Log the error
      println(s"Error parsing markdown: ${e.getMessage}")
      // Return an empty document or error document
      Document(List(Paragraph(List(Text(s"Error parsing content: ${e.getMessage}")))))
  }
}
```

## Debugging

For debugging purposes, you can render the document as XML:

```scala
val doc = parseDocumentContent("# Test\n\nParagraph")
val xml = renderToXML(doc)
println(xml)
```

This will produce XML output showing the full structure of the AST:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE document SYSTEM "CommonMark.dtd">
<document xmlns="http://commonmark.org/xml/1.0">
  <h1>Test</h1>
  <paragraph>Paragraph</paragraph>
</document>
```