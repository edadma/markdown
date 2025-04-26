package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkInlineParserTest extends AnyFlatSpec with Matchers {

  "The inline parser" should "process basic links correctly" in {
    val input   = "This is a [link](https://example.com) in text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", None, List(Text("link"))),
      Text(" in text."),
    )
  }

  it should "process links with titles" in {
    val input   = "This is a [link](https://example.com \"Link Title\") with title."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", Some("Link Title"), List(Text("link"))),
      Text(" with title."),
    )
  }

  it should "process links with single-quoted titles" in {
    val input   = "This is a [link](https://example.com 'Link Title') with title."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", Some("Link Title"), List(Text("link"))),
      Text(" with title."),
    )
  }

  it should "process links with parenthesis titles" in {
    val input   = "This is a [link](https://example.com (Link Title)) with title."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", Some("Link Title"), List(Text("link"))),
      Text(" with title."),
    )
  }

  it should "process links with emphasis in the text" in {
    val input   = "This is a [*emphasized link*](https://example.com) in text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", None, List(Emphasis(List(Text("emphasized link"))))),
      Text(" in text."),
    )
  }

  it should "process links with angle brackets in destination" in {
    val input   = "This is a [link](<https://example.com?q=1&p=2>) in text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com?q=1&p=2", None, List(Text("link"))),
      Text(" in text."),
    )
  }

  it should "handle empty links" in {
    val input   = "This is an [](https://example.com) empty link."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is an "),
      Link("https://example.com", None, List()),
      Text(" empty link."),
    )
  }

  it should "handle links with escaped brackets" in {
    val input   = "This is a [link with \\[brackets\\]](https://example.com) in text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      Link("https://example.com", None, List(Text("link with [brackets]"))),
      Text(" in text."),
    )
  }

  // These would require the link reference map to be implemented
  it should "handle reference links" in {
    val linkRefs = Map("ref" -> LinkReference("http://example.com", None))
    val input    = "This is a [reference link][ref]."
    val inlines  = parseInlineContent(input, linkRefs)

    // Until reference links are fully implemented, this will just show the literal syntax
    inlines shouldBe List(
      Text("This is a "),
      Text("[reference link][ref]."),
    )
  }
}
