package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CalloutBlockParserTest extends AnyFlatSpec with Matchers {

  // Helper method to parse markdown with callouts enabled
  private def parseWithCalloutsEnabled(input: String): Document = {
    val config = MarkdownConfig.all // Ensures callouts are enabled
    parseDocumentContent(input, config)
  }

  // Helper method to parse markdown with callouts disabled
  private def parseWithCalloutsDisabled(input: String): Document = {
    val config = MarkdownConfig.default // Callouts disabled by default
    parseDocumentContent(input, config)
  }

  "The callout block parser" should "parse a basic note callout" in {
    val input    = "> [!note]\n> This is a simple note."
    val document = parseWithCalloutsEnabled(input)

    document shouldBe Document(List(
      CalloutBlock(
        calloutType = "note",
        title = None,
        children = List(
          Paragraph(List(Text("This is a simple note."))),
        ),
      ),
    ))
  }

  it should "parse a callout with custom title" in {
    val input    = "> [!warning]: Important Warning\n> This is a warning with a custom title."
    val document = parseWithCalloutsEnabled(input)

    document shouldBe Document(List(
      CalloutBlock(
        calloutType = "warning",
        title = Some("Important Warning"),
        children = List(
          Paragraph(List(Text("This is a warning with a custom title."))),
        ),
      ),
    ))
  }

  it should "parse a callout with multiple paragraphs" in {
    val input = """
                  |> [!info]
                  |> First paragraph.
                  |>
                  |> Second paragraph.
                  |""".stripMargin
    val document = parseWithCalloutsEnabled(input)

    document shouldBe Document(List(
      CalloutBlock(
        calloutType = "info",
        title = None,
        children = List(
          Paragraph(List(Text("First paragraph."))),
          Paragraph(List(Text("Second paragraph."))),
        ),
      ),
    ))
  }

  it should "handle multiple adjacent callouts" in {
    val input = """
                  |> [!note]
                  |> Note content.
                  |
                  |> [!warning]
                  |> Warning content.
                  |""".stripMargin
    val document = parseWithCalloutsEnabled(input)

    document shouldBe Document(List(
      CalloutBlock(
        calloutType = "note",
        title = None,
        children = List(
          Paragraph(List(Text("Note content."))),
        ),
      ),
      CalloutBlock(
        calloutType = "warning",
        title = None,
        children = List(
          Paragraph(List(Text("Warning content."))),
        ),
      ),
    ))
  }

  it should "handle callouts with complex content including lists" in {
    val input = """
                  |> [!tip]
                  |> Tip with a list:
                  |> 
                  |> - Item 1
                  |> - Item 2
                  |""".stripMargin
    val document = parseWithCalloutsEnabled(input)

    // Extract the callout block for easier assertion
    val calloutBlock = document.children.head.asInstanceOf[CalloutBlock]

    calloutBlock.calloutType shouldBe "tip"
    calloutBlock.title shouldBe None
    calloutBlock.children.size shouldBe 2
    calloutBlock.children.head shouldBe a[Paragraph]
    calloutBlock.children(1) shouldBe a[ListBlock]
  }

  it should "handle callouts with code blocks" in {
    val input = """
                  |> [!note]
                  |> Example with code:
                  |> 
                  |> ```scala
                  |> val x = 42
                  |> ```
                  |""".stripMargin
    val document = parseWithCalloutsEnabled(input)

    // Extract the callout block for easier assertion
    val calloutBlock = document.children.head.asInstanceOf[CalloutBlock]

    calloutBlock.calloutType shouldBe "note"
    calloutBlock.title shouldBe None
    calloutBlock.children.size shouldBe 2
    calloutBlock.children.head shouldBe a[Paragraph]
    calloutBlock.children(1) shouldBe a[Code]
    calloutBlock.children(1).asInstanceOf[Code].infoString shouldBe Some("scala")
  }

  it should "normalize unsupported callout types to 'note'" in {
    val input    = "> [!custom-type]\n> Custom type callout."
    val document = parseWithCalloutsEnabled(input)

    document.children.head.asInstanceOf[CalloutBlock].calloutType shouldBe "note"
  }

  it should "parse callouts only when enabled in config" in {
    val input = "> [!note]\n> This is a note."

    // With callouts enabled
    val documentWithCallouts = parseWithCalloutsEnabled(input)
    documentWithCallouts.children.head shouldBe a[CalloutBlock]

    // With callouts disabled
    val documentWithoutCallouts = parseWithCalloutsDisabled(input)
    documentWithoutCallouts.children.head shouldBe a[BlockQuote]
  }

  it should "handle empty callouts" in {
    val input    = "> [!note]"
    val document = parseWithCalloutsEnabled(input)

    document shouldBe Document(List(
      CalloutBlock(
        calloutType = "note",
        title = None,
        children = List(),
      ),
    ))
  }

  it should "preserve inline formatting in callout content" in {
    val input    = "> [!important]\n> Text with **bold** and *italic*."
    val document = parseWithCalloutsEnabled(input)

    val paragraph = document.children.head.asInstanceOf[CalloutBlock].children.head.asInstanceOf[Paragraph]
    paragraph.inlines should contain(Strong(List(Text("bold"))))
    paragraph.inlines should contain(Emphasis(List(Text("italic"))))
  }

  it should "handle callouts with special characters in titles" in {
    val input    = "> [!warning]: Title with: colon & special chars!\n> Content."
    val document = parseWithCalloutsEnabled(input)

    document.children.head.asInstanceOf[CalloutBlock].title shouldBe Some("Title with: colon & special chars!")
  }
}
