package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CollapsibleBlockParserTest extends AnyFlatSpec with Matchers {

  "The collapsible section parser" should "parse a basic collapsible section" in {
    val input = """
                  |::: Title
                  |This is a basic collapsible section.
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Title")),
        isOpen = false,
        children = List(
          Paragraph(List(Text("This is a basic collapsible section."))),
        ),
      ),
    ))
  }

  it should "parse a collapsible section with a title" in {
    val input = """
                  |::: Click to expand
                  |Content inside the collapsible section.
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Click to expand")),
        isOpen = false,
        children = List(
          Paragraph(List(Text("Content inside the collapsible section."))),
        ),
      ),
    ))
  }

  it should "parse an open collapsible section" in {
    val input = """
                  |::: open Show less
                  |This section starts expanded.
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Show less")),
        isOpen = true,
        children = List(
          Paragraph(List(Text("This section starts expanded."))),
        ),
      ),
    ))
  }

  it should "handle multiple blocks inside a collapsible section" in {
    val input = """
                  |::: Multiple blocks
                  |First paragraph.
                  |
                  |Second paragraph.
                  |
                  |- List item 1
                  |- List item 2
                  |
                  |```
                  |Code block
                  |```
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Multiple blocks")),
        isOpen = false,
        children = List(
          Paragraph(List(Text("First paragraph."))),
          Paragraph(List(Text("Second paragraph."))),
          ListBlock(
            ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
            List(
              ListItem(List(Paragraph(List(Text("List item 1"))))),
              ListItem(List(Paragraph(List(Text("List item 2"))))),
            ),
          ),
          Code("Code block", None),
        ),
      ),
    ))
  }

  it should "handle a collapsible section that spans to the end without explicit closing" in {
    val input = """
                  |::: Title
                  |Unclosed section.""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Title")),
        isOpen = false,
        children = List(
          Paragraph(List(Text("Unclosed section."))),
        ),
      ),
    ))
  }

  it should "handle nested collapsible sections" in {
    val input = """
                  |::: Outer section
                  |Outer content.
                  |
                  |::: Inner section
                  |Inner content.
                  |:::
                  |
                  |More outer content.
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List(Text("Outer section")),
        isOpen = false,
        children = List(
          Paragraph(List(Text("Outer content."))),
          CollapsibleBlock(
            title = List(Text("Inner section")),
            isOpen = false,
            children = List(
              Paragraph(List(Text("Inner content."))),
            ),
          ),
          Paragraph(List(Text("More outer content."))),
        ),
      ),
    ))
  }

  it should "properly handle the title when 'open' is specified" in {
    val input = """
                  |::: open
                  |Expanded content with no title.
                  |:::""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      CollapsibleBlock(
        title = List.empty,
        isOpen = true,
        children = List(
          Paragraph(List(Text("Expanded content with no title."))),
        ),
      ),
    ))
  }

  it should "properly render to HTML" in {
    val input = """
                  |::: Click me
                  |Hidden content
                  |:::""".stripMargin
    val html = renderToHTML(input)

    html should include("<details>")
    html should include("<summary>Click me</summary>")
    html should include("<p>Hidden content</p>")
    html should include("</details>")
  }

  it should "render open sections with the 'open' attribute" in {
    val input = """
                  |::: open Always visible
                  |This starts expanded
                  |:::""".stripMargin
    val html = renderToHTML(input)

    html should include("<details open>")
    html should include("<summary>Always visible</summary>")
  }
}
