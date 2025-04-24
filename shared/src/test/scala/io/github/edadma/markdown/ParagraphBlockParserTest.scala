package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParagraphBlockParserTest extends AnyFlatSpec with Matchers {

  "The paragraph block parser" should "parse a simple paragraph" in {
    val input    = "This is a paragraph."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(Paragraph(List(Text("This is a paragraph.")))))
  }

  it should "parse multiple paragraphs separated by blank lines" in {
    val input    = """First paragraph.

Second paragraph.

Third paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(Text("First paragraph."), SoftLineBreak())),
      Paragraph(List(Text("Second paragraph."), SoftLineBreak())),
      Paragraph(List(Text("Third paragraph."))),
    ))
  }

  it should "parse a multi-line paragraph as a single paragraph" in {
    val input    = """This is a paragraph
that spans multiple
lines."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(Paragraph(List(
      Text("This is a paragraph"),
      SoftLineBreak(),
      Text("that spans multiple"),
      SoftLineBreak(),
      Text("lines."),
    ))))
  }

  it should "handle empty documents" in {
    val input    = ""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List())
  }

  it should "handle documents with only blank lines" in {
    val input    = """

"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List())
  }

  it should "ignore leading and trailing blank lines" in {
    val input    = """

First paragraph.

Last paragraph.

"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(Text("First paragraph."), SoftLineBreak())),
      Paragraph(List(Text("Last paragraph."), SoftLineBreak())),
    ))
  }

  it should "handle paragraphs with different line endings" in {
    val input    = "First line.\r\nSecond line.\r\n\r\nNew paragraph."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("First line."),
        SoftLineBreak(),
        Text("Second line."),
        SoftLineBreak(),
      )),
      Paragraph(List(Text("New paragraph."))),
    ))
  }

  it should "handle multiple consecutive blank lines between paragraphs" in {
    val input    = """First paragraph.


Second paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(Text("First paragraph."), SoftLineBreak())),
      Paragraph(List(Text("Second paragraph."))),
    ))
  }

  it should "handle paragraphs with different indentation" in {
    val input    = """  Indented paragraph.

Not indented paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(Text("  Indented paragraph."), SoftLineBreak())),
      Paragraph(List(Text("Not indented paragraph."))),
    ))
  }
}
