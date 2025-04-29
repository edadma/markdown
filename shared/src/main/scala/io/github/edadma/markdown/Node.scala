package io.github.edadma.markdown

import pprint.pprintln

trait Node {
  def processInlines(linkRefs: Map[String, LinkReference]): Node = this
}

// Document delegates to its children
case class Document(children: List[Block]) extends Node {
  override def processInlines(linkRefs: Map[String, LinkReference]): Document =
    Document(children.map(_.processInlines(linkRefs)))
}

trait Block extends Node {
  override def processInlines(linkRefs: Map[String, LinkReference]): Block = this
}

case class Paragraph(inlines: List[Inline]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): Paragraph =
    Paragraph(parseInline(inlines, linkRefs))
}

case class Heading(level: Int, inlines: List[Inline]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): Heading =
    Heading(level, parseInline(inlines, linkRefs))
}

case class BlockQuote(children: List[Block]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): BlockQuote =
    BlockQuote(children.map(_.processInlines(linkRefs)))
}

case class Code(content: String, infoString: Option[String] = None) extends Block
case class ThematicBreak()                                          extends Block
case class HTMLBlock(content: String)                               extends Block

enum TableAlignment:
  case Left, Center, Right, None

case class TableCell(content: List[Inline]) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference]): Block = {
    TableCell(parseInline(content, linkRefs))
  }

case class TableRow(cells: List[TableCell]) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference]): Block =
    TableRow(cells.map(_.processInlines(linkRefs).asInstanceOf[TableCell]))

case class Table(
    headerRow: TableRow,
    rows: List[TableRow],
    alignments: List[TableAlignment],
) extends Block:
  override def processInlines(linkRefs: Map[String, LinkReference]): Block =
    Table(
      headerRow.processInlines(linkRefs).asInstanceOf[TableRow],
      rows.map(_.processInlines(linkRefs).asInstanceOf[TableRow]),
      alignments,
    )

case class ListItem(content: List[Block]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): Block = {
    ListItem(content.map(_.processInlines(linkRefs)))
  }
}

case class ListBlock(data: ListData, items: List[ListItem]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): Block = {

    pprintln(linkRefs)
    pprintln(items.head.content.head.processInlines(linkRefs))
    ListBlock(
      data,
      items.map(item => ListItem(item.content.map(_.processInlines(linkRefs)))),
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
case class Image(destination: String, title: Option[String], inlines: List[Inline]) extends Inline
case class AutoLink(destination: String, text: String)                              extends Inline
case class RawHTML(content: String)                                                 extends Inline

case class C(
    char: Char,        // The character (possibly transformed)
    pos: Int,          // Position in original input
    line: Int,         // Line number (0-based)
    column: Int,       // Column number (0-based)
    isLiteral: Boolean, // Whether this should be treated literally (not as syntax)
) extends Inline

// Sentinel value for end of input
object EndOfInput extends C('\u0000', -1, -1, -1, false)
