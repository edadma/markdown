package io.github.edadma.markdown

sealed trait Node
case class Document(children: List[Block]) extends Node

sealed trait Block                          extends Node
case class Paragraph(inlines: List[Inline]) extends Block

sealed trait Inline              extends Node
case class Text(content: String) extends Inline
