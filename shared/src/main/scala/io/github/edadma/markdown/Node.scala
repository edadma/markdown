package io.github.edadma.markdown

trait Node {
  def processInlines: Node = this
}

// Document delegates to its children
case class Document(children: List[Block]) extends Node {
  override def processInlines: Document = Document(children.map(_.processInlines))
}

trait Block extends Node {
  override def processInlines: Block = this
}

case class Paragraph(inlines: List[Inline]) extends Block {
  override def processInlines: Paragraph = Paragraph(parseInline(inlines))
}

case class Heading(level: Int, inlines: List[Inline]) extends Block {
  override def processInlines: Heading = Heading(level, parseInline(inlines))
}

case class BlockQuote(children: List[Block]) extends Block {
  override def processInlines: BlockQuote =
    BlockQuote(children.map(_.processInlines))
}

case class Code(content: String, infoString: Option[String] = None) extends Block
case class ThematicBreak()                                          extends Block

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

case class Cursor(
    char: Char,        // The character (possibly transformed)
    pos: Int,          // Position in original input
    line: Int,         // Line number (0-based)
    column: Int,       // Column number (0-based)
    isLiteral: Boolean, // Whether this should be treated literally (not as syntax)
) extends Inline

// Sentinel value for end of input
object EndOfInput extends Cursor('\u0000', -1, -1, -1, false)
