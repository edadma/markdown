package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImageInlineParserTest extends AnyFlatSpec with Matchers {

  "The inline parser" should "process basic images correctly" in {
    val input   = "Here's an image: ![alt text](image.jpg)"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Here's an image: "),
      Image("image.jpg", None, List(Text("alt text"))),
    )
  }

  it should "process images with titles" in {
    val input   = "Image with title: ![alt text](image.jpg \"Image Title\")"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Image with title: "),
      Image("image.jpg", Some("Image Title"), List(Text("alt text"))),
    )
  }

  it should "process reference-style images" in {
    val linkRefs = Map("ref" -> LinkReference("image.jpg", Some("Image Title")))
    val input    = "Reference image: ![alt text][ref]"
    val inlines  = parseInlineContent(input, linkRefs)

    inlines shouldBe List(
      Text("Reference image: "),
      Image("image.jpg", Some("Image Title"), List(Text("alt text"))),
    )
  }

  it should "process images with nested emphasis in alt text" in {
    val input   = "Image with emphasis: ![*emphasized* alt text](image.jpg)"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Image with emphasis: "),
      Image(
        "image.jpg",
        None,
        List(
          Emphasis(List(Text("emphasized"))),
          Text(" alt text"),
        ),
      ),
    )
  }

  it should "allow images inside links" in {
    val input   = "[![alt text](image.jpg)](https://example.com)"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Link(
        "https://example.com",
        None,
        List(
          Image("image.jpg", None, List(Text("alt text"))),
        ),
      ),
    )
  }

  it should "handle shortcut reference images" in {
    val linkRefs = Map("alt text" -> LinkReference("image.jpg", None))
    val input    = "Shortcut image: ![alt text]"
    val inlines  = parseInlineContent(input, linkRefs)

    inlines shouldBe List(
      Text("Shortcut image: "),
      Image("image.jpg", None, List(Text("alt text"))),
    )
  }
}
