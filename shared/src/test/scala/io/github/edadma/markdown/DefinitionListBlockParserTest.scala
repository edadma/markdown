package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DefinitionListBlockParserTest extends AnyFlatSpec with Matchers {

  private val config = MarkdownConfig().copy(definitionLists = true)

  "The definition list parser" should "parse a simple definition list" in {
    val input = """Term 1
                  |: Definition 1
                  |
                  |Term 2
                  |: Definition 2""".stripMargin

    // Create a configuration that enables definition lists
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      DefinitionListBlock(List(
        (List(Text("Term 1")), List(Paragraph(List(Text("Definition 1"))))),
        (List(Text("Term 2")), List(Paragraph(List(Text("Definition 2"))))),
      )),
    ))
  }

  it should "parse a definition list with multiple definitions for one term" in {
    val input = """Term 1
                  |: First definition
                  |: Second definition
                  |: Third definition""".stripMargin

    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      DefinitionListBlock(List(
        (
          List(Text("Term 1")),
          List(
            Paragraph(List(Text("First definition"))),
            Paragraph(List(Text("Second definition"))),
            Paragraph(List(Text("Third definition"))),
          ),
        ),
      )),
    ))
  }

  it should "ignore definition lists when the feature is disabled" in {
    val input = """Term 1
                  |: Definition 1""".stripMargin

    // Use default config (definition lists disabled)
    val config   = MarkdownConfig().copy(definitionLists = false)
    val document = parseDocumentContent(input, config)

    // Should be parsed as a regular paragraph
    document shouldBe Document(List(
      Paragraph(List(
        Text("Term 1"),
        SoftLineBreak(),
        Text(": Definition 1"),
      )),
    ))
  }

  it should "handle blank lines correctly" in {
    val input = """Term 1
                  |: Definition 1
                  |
                  |: Definition 2
                  |
                  |Term 2
                  |: Another definition""".stripMargin

    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      DefinitionListBlock(List(
        (
          List(Text("Term 1")),
          List(
            Paragraph(List(Text("Definition 1"))),
            Paragraph(List(Text("Definition 2"))),
          ),
        ),
        (
          List(Text("Term 2")),
          List(
            Paragraph(List(Text("Another definition"))),
          ),
        ),
      )),
    ))
  }

  it should "handle inline formatting in terms and definitions" in {
    val input = """*Emphasized Term*
                  |: Definition with **strong** text""".stripMargin

    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      DefinitionListBlock(List(
        (
          List(Emphasis(List(Text("Emphasized Term")))),
          List(
            Paragraph(List(
              Text("Definition with "),
              Strong(List(Text("strong"))),
              Text(" text"),
            )),
          ),
        ),
      )),
    ))
  }

  it should "handle definition lists adjacent to other block elements" in {
    val input = """# Heading

                  |Term 1
                  |: Definition 1
                  |
                  |> This is a blockquote""".stripMargin

    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading"))),
      DefinitionListBlock(List(
        (List(Text("Term 1")), List(Paragraph(List(Text("Definition 1"))))),
      )),
      BlockQuote(List(
        Paragraph(List(Text("This is a blockquote"))),
      )),
    ))
  }

  // Regression: renderToHTML used to call the public `renderToHTML(node, config)`
  // entry point on each definition body, but that pattern-matches only on
  // Document, throwing `MatchError: Paragraph(...)` for the Paragraph that
  // every parsed definition body actually contains. Now goes through
  // renderBlockToHTML directly.
  it should "render a definition list to HTML without throwing MatchError" in {
    val input = """Apple
                  |: A round fruit, typically red or green.
                  |
                  |Orange
                  |: A citrus fruit. Also a color.""".stripMargin

    val doc  = parseDocumentContent(input, config)
    val html = renderToHTML(doc, config)
    html should include("<dl>")
    html should include("<dt>Apple</dt>")
    html should include("A round fruit, typically red or green.")
    html should include("<dt>Orange</dt>")
    html should include("A citrus fruit. Also a color.")
    html should include("</dl>")
  }
}
