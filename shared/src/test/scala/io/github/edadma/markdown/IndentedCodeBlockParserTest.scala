package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IndentedCodeBlockParserTest extends AnyFlatSpec with Matchers {

  "The indented code block parser" should "parse a simple indented code block" in {
    val input    = "    This is a code block."
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This is a code block.", None),
    ))
  }

  it should "parse a multi-line indented code block" in {
    val input = """
                  |    Line 1
                  |    Line 2
                  |    Line 3""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("Line 1\nLine 2\nLine 3", None),
    ))
  }

  it should "handle blank lines within indented code blocks" in {
    val input = """
                  |    Line 1
                  |
                  |    Line 3""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("Line 1\n\nLine 3", None),
    ))
  }

  it should "preserve indentation beyond the required 4 spaces" in {
    val input = """
                  |    Code line
                  |        Indented line
                  |            More indented""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("Code line\n    Indented line\n        More indented", None),
    ))
  }

  it should "terminate code blocks at non-indented, non-blank lines" in {
    val input = """
                  |    Code block
                  |    Still code
                  |Not code
                  |
                  |    Another code block""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("Code block\nStill code", None),
      Paragraph(List(Text("Not code"))),
      Code("Another code block", None),
    ))
  }

  it should "handle code blocks following paragraphs" in {
    val input = """
                  |This is a paragraph.
                  |
                  |    This is a code block.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("This is a paragraph."))),
      Code("This is a code block.", None),
    ))
  }

  it should "handle tabs for indentation" in {
    val input    = """	This is indented with a tab."""
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This is indented with a tab.", None),
    ))
  }

  it should "handle fewer than 4 spaces as regular paragraph" in {
    val input = """   Only 3 spaces
                  |  Only 2 spaces""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Only 3 spaces"),
        SoftLineBreak(),
        Text("Only 2 spaces"),
      )),
    ))
  }

  it should "handle mixed tabs and spaces correctly" in {
    val input = """
                  |	  Code indented with tab and spaces
                  | 	 Also code with space-tab-space""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("  Code indented with tab and spaces\n Also code with space-tab-space", None),
    ))
  }

  it should "handle code blocks with special characters" in {
    val input = """
                  |    def hello() {
                  |      println("Hello, world!")
                  |    }""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code(
        """def hello() {
  println("Hello, world!")
}""",
        None,
      ),
    ))
  }

  it should "ignore trailing blank lines after code blocks" in {
    val input = """
                  |    Code block line.
                  |
                  |
                  |Regular paragraph.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("Code block line.", None),
      Paragraph(List(Text("Regular paragraph."))),
    ))
  }
}
