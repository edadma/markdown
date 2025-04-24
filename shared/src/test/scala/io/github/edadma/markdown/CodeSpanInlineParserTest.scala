package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CodeSpanInlineParserTest extends AnyFlatSpec with Matchers {

  "The inline parser" should "process basic code spans correctly" in {
    val input    = "This is a `code span` example."
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("This is a "),
        CodeSpan("code span"),
        Text(" example."),
      )),
    ))
  }

  it should "handle code spans with backticks inside using double backticks" in {
    val input    = "Example: ``code with `backtick` inside``"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Example: "),
        CodeSpan("code with `backtick` inside"),
      )),
    ))
  }

  it should "strip one space from both ends when spaces surround content" in {
    val input    = "Space stripping: ` code with spaces `"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Space stripping: "),
        CodeSpan("code with spaces"),
      )),
    ))
  }

  it should "only strip one space from each end" in {
    val input    = "Double spaces: `  multiple spaces  `"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Double spaces: "),
        CodeSpan(" multiple spaces "),
      )),
    ))
  }

  it should "not strip spaces if content only has leading or trailing space" in {
    val input    = "Leading space only: ` code`\nTrailing space only: `code `"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Leading space only: "),
        CodeSpan(" code"),
        Text("\nTrailing space only: "),
        CodeSpan("code "),
      )),
    ))
  }

  it should "not strip spaces if code span contains only spaces" in {
    val input    = "Just spaces: ` ` and `  `"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Just spaces: "),
        CodeSpan(" "),
        Text(" and "),
        CodeSpan("  "),
      )),
    ))
  }

  it should "convert line endings in code spans to spaces" in {
    val input    = "Line breaks: `code\nwith\nline\nbreaks`"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Line breaks: "),
        CodeSpan("code with line breaks"),
      )),
    ))
  }

  it should "preserve internal spaces in code spans" in {
    val input    = "Internal spaces: `code   with   multiple   spaces`"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Internal spaces: "),
        CodeSpan("code   with   multiple   spaces"),
      )),
    ))
  }

  it should "handle backtick escapes as literal inside code spans" in {
    val input    = "No escaping inside: `\\`\\``"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("No escaping inside: "),
        CodeSpan(content = "\\"),
        Text(content = "``"),
      )),
    ))
  }

  it should "treat unmatched code spans as literal backticks" in {
    val input    = "Unmatched: `not closed"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Unmatched: `not closed"),
      )),
    ))
  }

  it should "handle multi-line code spans correctly" in {
    val input    = "Multi-line: `code\nspan\ncontinues`"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Multi-line: "),
        CodeSpan("code span continues"),
      )),
    ))
  }

  it should "handle code spans with different length backtick sequences" in {
    val input    = "Multiple sequences: ``double`` and ```triple```"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("Multiple sequences: "),
        CodeSpan("double"),
        Text(" and "),
        CodeSpan("triple"),
      )),
    ))
  }

  it should "have precedence over emphasis" in {
    val input    = "*emphasized `code* span`"
    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)

    document shouldBe Document(List(
      Paragraph(List(
        Text("*emphasized "),
        CodeSpan("code* span"),
      )),
    ))
  }
}
