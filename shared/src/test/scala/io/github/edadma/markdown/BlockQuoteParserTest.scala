package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockQuoteParserTest extends AnyFlatSpec with Matchers {

  "The block quote parser" should "parse a simple block quote" in {
    val input    = "> This is a block quote."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("This is a block quote."))),
      )),
    ))
  }

  it should "parse a block quote with multiple paragraphs" in {
    val input = """
                  |> First paragraph.
                  |>
                  |> Second paragraph.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("First paragraph."), SoftLineBreak())),
        Paragraph(List(Text("Second paragraph."))),
      )),
    ))
  }

  it should "parse nested block quotes" in {
    val input = """
                  |> Outer quote.
                  |>
                  |> > Nested quote.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("Outer quote."), SoftLineBreak())),
        BlockQuote(List(
          Paragraph(List(Text("Nested quote."))),
        )),
      )),
    ))
  }

  it should "parse block quotes with other block elements" in {
    val input = """
                  |> # Heading in a block quote
                  |>
                  |> ```
                  |> Code block in a block quote
                  |> ```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Heading(1, List(Text("Heading in a block quote"))),
        Code("Code block in a block quote", None),
      )),
    ))
  }

  it should "handle lazy continuation lines" in {
    val input = """
                  |> This is a paragraph
                  |that continues on the next line.
                  |> This is another paragraph.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(
          Text("This is a paragraph"),
          SoftLineBreak(),
          Text("that continues on the next line."),
          SoftLineBreak(),
          Text("This is another paragraph."),
        )),
      )),
    ))
  }

  it should "handle indented block quotes" in {
    val input    = "   > This block quote is indented with 3 spaces."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("This block quote is indented with 3 spaces."))),
      )),
    ))
  }

  it should "handle block quotes with blank lines" in {
    val input = """
                  |> First paragraph.
                  |
                  |> Second paragraph.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("First paragraph."), SoftLineBreak())),
      )),
      BlockQuote(List(
        Paragraph(List(Text("Second paragraph."))),
      )),
    ))
  }

  it should "handle multiple > markers on a single line" in {
    val input    = "> > Nested quote on a single line."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        BlockQuote(List(
          Paragraph(List(Text("Nested quote on a single line."))),
        )),
      )),
    ))
  }

  it should "handle a mixture of block quote styles" in {
    val input = """
                  |> First line
                  |> > Nested line
                  |continued without a marker
                  |> Last line of inner block quote""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(
          Text("First line"),
          SoftLineBreak(),
        )),
        BlockQuote(List(
          Paragraph(List(
            Text("Nested line"),
            SoftLineBreak(),
            Text("continued without a marker"),
            SoftLineBreak(),
            Text("Last line of inner block quote"),
          )),
        )),
      )),
    ))
  }

  it should "correctly process thematic breaks in block quotes" in {
    val input = """
                  |> Above a horizontal rule
                  |>
                  |> ---
                  |>
                  |> Below a horizontal rule""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      BlockQuote(List(
        Paragraph(List(Text("Above a horizontal rule"), SoftLineBreak())),
        ThematicBreak(),
        Paragraph(List(Text("Below a horizontal rule"))),
      )),
    ))
  }
}
