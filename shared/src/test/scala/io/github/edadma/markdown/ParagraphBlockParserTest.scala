package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParagraphBlockParserTest extends AnyFlatSpec with Matchers {

  "The paragraph block parser" should "parse a simple paragraph" in {
    val input    = "This is a paragraph."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 1
    document.children.head shouldBe a[Paragraph]
    val para = document.children.head.asInstanceOf[Paragraph]
    para.inlines should have length 1
    para.inlines.head shouldBe a[Text]
    para.inlines.head.asInstanceOf[Text].content should be("This is a paragraph.")
  }

  it should "parse multiple paragraphs separated by blank lines" in {
    val input    = """First paragraph.

Second paragraph.

Third paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 3
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should include("First paragraph")
    paras(1).inlines.head.asInstanceOf[Text].content should include("Second paragraph")
    paras(2).inlines.head.asInstanceOf[Text].content should include("Third paragraph")
  }

  it should "parse a multi-line paragraph as a single paragraph" in {
    val input    = """This is a paragraph
that spans multiple
lines."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 1
    document.children.head shouldBe a[Paragraph]

    val text = document.children.head.asInstanceOf[Paragraph].inlines.head.asInstanceOf[Text].content
    text should include("This is a paragraph")
    text should include("that spans multiple")
    text should include("lines")
  }

  it should "handle empty documents" in {
    val input    = ""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should be(empty)
  }

  it should "handle documents with only blank lines" in {
    val input    = """

"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should be(empty)
  }

  it should "ignore leading and trailing blank lines" in {
    val input    = """

First paragraph.

Last paragraph.

"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should include("First paragraph")
    paras(1).inlines.head.asInstanceOf[Text].content should include("Last paragraph")
  }

  it should "handle paragraphs with different line endings" in {
    // Test with CRLF line endings (will be normalized by InputReader)
    val input    = "First line.\r\nSecond line.\r\n\r\nNew paragraph."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val firstPara = document.children(0).asInstanceOf[Paragraph]
    firstPara.inlines.head.asInstanceOf[Text].content should include("First line")
    firstPara.inlines.head.asInstanceOf[Text].content should include("Second line")

    val secondPara = document.children(1).asInstanceOf[Paragraph]
    secondPara.inlines.head.asInstanceOf[Text].content should include("New paragraph")
  }

  it should "handle multiple consecutive blank lines between paragraphs" in {
    val input    = """First paragraph.


Second paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should include("First paragraph")
    paras(1).inlines.head.asInstanceOf[Text].content should include("Second paragraph")
  }

  it should "handle paragraphs with different indentation" in {
    val input    = """  Indented paragraph.

Not indented paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should include("  Indented paragraph")
    paras(1).inlines.head.asInstanceOf[Text].content should include("Not indented paragraph")
  }

  it should "preserve newlines in the paragraph content" in {
    val input    = """Line one
Line two
Line three"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 1
    document.children.head shouldBe a[Paragraph]

    val content = document.children.head.asInstanceOf[Paragraph].inlines.head.asInstanceOf[Text].content
    content should include("\n")
    content.count(_ == '\n') should be(2) // Two newlines in the paragraph
  }
}
