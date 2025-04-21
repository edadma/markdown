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
    paras(0).inlines.head.asInstanceOf[Text].content should be("First paragraph.")
    paras(1).inlines.head.asInstanceOf[Text].content should be("Second paragraph.")
    paras(2).inlines.head.asInstanceOf[Text].content should be("Third paragraph.")
  }

  it should "parse a multi-line paragraph as a single paragraph with line breaks" in {
    val input    = """This is a paragraph
that spans multiple
lines."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 1
    document.children.head shouldBe a[Paragraph]

    val para = document.children.head.asInstanceOf[Paragraph]

    // The paragraph should have alternating Text and SoftLineBreak nodes
    para.inlines.length should be >= 5 // At least 3 text nodes and 2 line breaks

    // Check that the text content is spread across multiple Text nodes
    val textNodes = para.inlines.collect { case t: Text => t.content }
    textNodes should contain("This is a paragraph")
    textNodes should contain("that spans multiple")
    textNodes should contain("lines.")

    // Verify the structure has alternating Text and SoftLineBreak nodes
    para.inlines.zipWithIndex.foreach { case (node, idx) =>
      if (idx % 2 == 0) node shouldBe a[Text]
      else node shouldBe a[SoftLineBreak]
    }
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
    paras(0).inlines.head.asInstanceOf[Text].content should be("First paragraph.")
    paras(1).inlines.head.asInstanceOf[Text].content should be("Last paragraph.")
  }

  it should "handle paragraphs with different line endings" in {
    // Test with CRLF line endings (will be normalized by InputReader)
    val input    = "First line.\r\nSecond line.\r\n\r\nNew paragraph."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val firstPara = document.children(0).asInstanceOf[Paragraph]
    firstPara.inlines(0).asInstanceOf[Text].content should be("First line.")
    firstPara.inlines(1) shouldBe a[SoftLineBreak]
    firstPara.inlines(2).asInstanceOf[Text].content should be("Second line.")

    val secondPara = document.children(1).asInstanceOf[Paragraph]
    secondPara.inlines.head.asInstanceOf[Text].content should be("New paragraph.")
  }

  it should "handle multiple consecutive blank lines between paragraphs" in {
    val input    = """First paragraph.


Second paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should be("First paragraph.")
    paras(1).inlines.head.asInstanceOf[Text].content should be("Second paragraph.")
  }

  it should "handle paragraphs with different indentation" in {
    val input    = """  Indented paragraph.

Not indented paragraph."""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 2
    document.children.foreach(_ shouldBe a[Paragraph])

    val paras = document.children.map(_.asInstanceOf[Paragraph])
    paras(0).inlines.head.asInstanceOf[Text].content should be("  Indented paragraph.")
    paras(1).inlines.head.asInstanceOf[Text].content should be("Not indented paragraph.")
  }

  it should "correctly structure line breaks within a paragraph" in {
    val input    = """Line one
Line two
Line three"""
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document.children should have length 1
    document.children.head shouldBe a[Paragraph]

    val para = document.children.head.asInstanceOf[Paragraph]
    para.inlines.length should be(5) // 3 text nodes + 2 line breaks

    para.inlines(0) shouldBe a[Text]
    para.inlines(0).asInstanceOf[Text].content should be("Line one")

    para.inlines(1) shouldBe a[SoftLineBreak]

    para.inlines(2) shouldBe a[Text]
    para.inlines(2).asInstanceOf[Text].content should be("Line two")

    para.inlines(3) shouldBe a[SoftLineBreak]

    para.inlines(4) shouldBe a[Text]
    para.inlines(4).asInstanceOf[Text].content should be("Line three")
  }
}
