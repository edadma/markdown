package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLEntityProcessingTest extends AnyFlatSpec with Matchers {

  "The HTML entity processor" should "handle common named entities" in {
    val input    = "This is a &copy; symbol and a &reg; symbol."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("This is a © symbol and a ® symbol."))),
    ))
  }

  it should "handle decimal numeric entities" in {
    val input    = "ASCII A is &#65; and Z is &#90;."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("ASCII A is A and Z is Z."))),
    ))
  }

  it should "handle hexadecimal numeric entities" in {
    val input    = "Hex A is &#x41; and Z is &#x5A;."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("Hex A is A and Z is Z."))),
    ))
  }

  it should "preserve invalid or incomplete entities" in {
    val input    = "Invalid: &invalid; and incomplete: &#; and &#x;"
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("Invalid: &invalid; and incomplete: &#; and &#x;"))),
    ))
  }

  it should "handle entities in headings and other block elements" in {
    val input = """# Heading with &copy; symbol
                  |
                  |> Blockquote with &reg; symbol
                  |
                  |```
                  |Code with &amp; symbol
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading with © symbol"))),
      BlockQuote(List(
        Paragraph(List(Text("Blockquote with ® symbol"))),
      )),
      Code("Code with &amp; symbol", None),
    ))
  }

  it should "handle entities in emphasis and other inline elements" in {
    val input    = "This is *emphasized with &hearts;* and **strong with &spades;**."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(
        Text("This is "),
        Emphasis(List(Text("emphasized with ♥"))),
        Text(" and "),
        Strong(List(Text("strong with ♠"))),
        Text("."),
      )),
    ))
  }

  it should "handle entities in links and images" in {
    val input =
      "[Link with &copy;](https://example.com/?param=value) and ![Image with &reg;](image.jpg \"Title with &trade;\")"
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(
        Link("https://example.com/?param=value", None, List(Text("Link with ©"))),
        Text(" and "),
        Image("image.jpg", Some("Title with ™"), List(Text("Image with ®"))),
      )),
    ))
  }
}
