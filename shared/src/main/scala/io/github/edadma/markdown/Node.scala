package io.github.edadma.markdown

sealed trait Node
case class Document(children: List[Block]) extends Node

sealed trait Block                                                  extends Node
case class Paragraph(inlines: List[Inline])                         extends Block
case class Heading(level: Int, inlines: List[Inline])               extends Block
case class Code(content: String, infoString: Option[String] = None) extends Block
case class BlockQuote(children: List[Block])                        extends Block
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
