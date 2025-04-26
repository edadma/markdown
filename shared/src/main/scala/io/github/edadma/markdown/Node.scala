package io.github.edadma.markdown

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
