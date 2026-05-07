package io.github.edadma.markdown

case class Attributes(id: Option[String] = None, classes: List[String] = Nil, kvPairs: Map[String, String] = Map.empty)

trait Node {
  def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Node = this
}

// Document delegates to its children
case class Document(children: List[Block]) extends Node {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Document =
    Document(children.map(_.processInlines(linkRefs, config)))

  /** All top-level [[Heading]] blocks, in source order. Convenience accessor
    * for table-of-contents builders, page-title extraction, etc. — the same
    * `children.collect { case h: Heading => h }` repeated everywhere.
    */
  def headings: List[Heading] = children.collect { case h: Heading => h }
}

trait Block extends Node {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = this
}

case class Paragraph(inlines: List[Inline]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Paragraph =
    Paragraph(parseInline(inlines, linkRefs, config))
}

case class Heading(level: Int, inlines: List[Inline], attrs: Option[Attributes] = None) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Heading = {
    val parsed = parseInline(inlines, linkRefs, config)

    // Auto-generate an `id` attribute from the heading's plain-text content,
    // unless the heading already has one (set by the `attributes` extension
    // — `## Heading {#explicit-anchor}`). Explicit ids always win.
    val finalAttrs =
      if (config.autoHeadingIds && !attrs.exists(_.id.isDefined)) {
        val text = plainText(parsed)
        val slug = config.slugify(text)
        if (slug.isEmpty) attrs
        else Some(attrs.getOrElse(Attributes()).copy(id = Some(slug)))
      } else attrs

    Heading(level, parsed, finalAttrs)
  }
}

case class BlockQuote(children: List[Block]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): BlockQuote =
    BlockQuote(children.map(_.processInlines(linkRefs, config)))
}

case class Code(content: String, infoString: Option[String] = None, indented: Boolean = false, attrs: Option[Attributes] = None) extends Block
case class ThematicBreak()                                          extends Block
case class HTMLBlock(content: String)                               extends Block

enum TableAlignment:
  case Left, Center, Right, None

case class TableCell(content: List[Inline]) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    TableCell(parseInline(content, linkRefs, config))
  }

case class TableRow(cells: List[TableCell]) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block =
    TableRow(cells.map(_.processInlines(linkRefs, config).asInstanceOf[TableCell]))

case class Table(
    headerRow: TableRow,
    rows: List[TableRow],
    alignments: List[TableAlignment],
) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block =
    Table(
      headerRow.processInlines(linkRefs, config).asInstanceOf[TableRow],
      rows.map(_.processInlines(linkRefs, config).asInstanceOf[TableRow]),
      alignments,
    )

case class ListItem(content: List[Block]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    ListItem(content.map(_.processInlines(linkRefs, config)))
  }
}

case class ListBlock(data: ListData, items: List[ListItem]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    ListBlock(
      data,
      items.map(item => ListItem(item.content.map(_.processInlines(linkRefs, config)))),
    )
  }
}

case class DefinitionListBlock(items: List[(List[Inline], List[Block])]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    DefinitionListBlock(items.map { case (term, defs) =>
      (parseInline(term, linkRefs, config), defs.map(_.processInlines(linkRefs, config)))
    })
  }
}

case class MathBlock(content: String) extends Block

/** Block-level doc-tag annotation (e.g. `@param name — description`).
  *
  * The tag is produced by the doc-tag extension (see `DocTagConfig`). Downstream tools walk these nodes to drive
  * documentation generators, linters, and indexers — this processor only parses the syntax.
  *
  * @param name
  *   the tag name (the identifier following `@`)
  * @param target
  *   optional identifier target; only populated when the registry entry has `acceptsTarget = true`
  * @param body
  *   body content, parsed per the tag's `ContentMode`. For `Opaque`, a single `Paragraph(List(Text(...)))`.
  *   For `InlineMarkdown`, a single `Paragraph` with parsed inlines. For `BlockMarkdown`, arbitrary blocks.
  * @param contentMode
  *   the content mode used to parse the body (retained so renderers / downstream tools can distinguish modes)
  */
case class DocTagBlock(
    name: String,
    target: Option[String],
    body: List[Block],
    contentMode: ContentMode,
) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block =
    DocTagBlock(name, target, body.map(_.processInlines(linkRefs, config)), contentMode)
}

case class FootnoteDefinition(label: String, content: List[Block]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block =
    FootnoteDefinition(label, content.map(_.processInlines(linkRefs, config)))
}

/** Represents a callout block in the document.
  *
  * A callout block is a specialized form of block quote with additional metadata for type and optional title to create
  * visually distinct "callout" sections.
  *
  * @param calloutType
  *   The type of callout (e.g., "note", "warning", "info", "tip", "danger")
  * @param title
  *   Optional title for the callout block
  * @param children
  *   Child blocks within the callout
  */
case class CalloutBlock(
    calloutType: String,   // Type of callout (e.g., "note", "warning", "info", etc.)
    title: Option[String], // Optional custom title for the callout
    children: List[Block], // Child blocks (could be paragraphs, lists, code blocks, etc.)
) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    CalloutBlock(
      calloutType,
      title,
      children.map(_.processInlines(linkRefs, config)),
    )
  }
}

case class CollapsibleBlock(
    title: List[Inline],
    isOpen: Boolean,
    children: List[Block],
) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference], config: MarkdownConfig): Block = {
    CollapsibleBlock(
      parseInline(title, linkRefs, config),
      isOpen,
      children.map(_.processInlines(linkRefs, config)),
    )
  }
}

sealed trait Inline                                                                 extends Node
case class Text(content: String)                                                    extends Inline
case class SoftLineBreak()                                                          extends Inline
case class HardLineBreak()                                                          extends Inline
case class CodeSpan(content: String)                                                extends Inline
case class Emphasis(inlines: List[Inline])                                          extends Inline
case class Strong(inlines: List[Inline])                                            extends Inline
case class Link(destination: String, title: Option[String], inlines: List[Inline])  extends Inline
case class Image(destination: String, title: Option[String], inlines: List[Inline], attrs: Option[Attributes] = None) extends Inline
case class AutoLink(destination: String, text: String)                              extends Inline
case class RawHTML(content: String)                                                 extends Inline
case class MathExpr(content: String)                                                extends Inline
case class Emoji(name: String)                                                      extends Inline
case class Strikethrough(inlines: List[Inline])                                     extends Inline
case class FootnoteReference(label: String)                                         extends Inline

case class C(
    char: Char,        // The character (possibly transformed)
    pos: Int,          // Position in original input
    line: Int,         // Line number (0-based)
    column: Int,       // Column number (0-based)
    isLiteral: Boolean, // Whether this should be treated literally (not as syntax)
) extends Inline

// Sentinel value for end of input
object EndOfInput extends C('\u0000', -1, -1, -1, false)
