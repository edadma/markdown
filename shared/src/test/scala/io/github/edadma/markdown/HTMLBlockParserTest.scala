package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLBlockParserTest extends AnyFlatSpec with Matchers {

  "The HTML block parser" should "parse HTML comments" in {
    val input = "<!-- this is a comment -->"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<!-- this is a comment -->\n"),
    )
  }

  it should "parse processing instructions" in {
    val input = "<?xml version=\"1.0\"?>"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<?xml version=\"1.0\"?>\n"),
    )
  }

  it should "parse CDATA blocks" in {
    val input = "<![CDATA[<tag>some data</tag>]]>"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<![CDATA[<tag>some data</tag>]]>\n"),
    )
  }

  it should "parse DOCTYPE declarations" in {
    val input = "<!DOCTYPE html>"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<!DOCTYPE html>\n"),
    )
  }

  it should "parse <script> blocks" in {
    val input =
      """<script>
        |console.log(\"hello\");
        |</script>""".stripMargin
    val doc = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock(
        "<script>\n" +
          "console.log(\\\"hello\\\");\n" +
          "</script>\n",
      ),
    )
  }

  it should "parse <style> blocks" in {
    val input =
      """<style>
        |body { color: red; }
        |</style>""".stripMargin
    val doc = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock(
        "<style>\n" +
          "body { color: red; }\n" +
          "</style>\n",
      ),
    )
  }

  it should "parse <pre> blocks" in {
    val input =
      """<pre>
        |some    preformatted
        |text
        |</pre>""".stripMargin
    val doc = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock(
        "<pre>\n" +
          "some    preformatted\n" +
          "text\n" +
          "</pre>\n",
      ),
    )
  }

  it should "parse single-line block tags like <div>" in {
    val input = "<div class=\"foo\">"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<div class=\"foo\">\n"),
    )
  }

  it should "parse closing tags like </footer>" in {
    val input = "</footer>"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("</footer>\n"),
    )
  }

  it should "parse generic tags like <span>" in {
    val input = "<span>"
    val doc   = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock("<span>\n"),
    )
  }

  it should "stop HTML block at closing tag and parse subsequent paragraph" in {
    val input =
      """<div>
        |Hello
        |</div>
        |
        |This is a paragraph.""".stripMargin
    val doc = parseDocumentContent(input)
    doc.children shouldBe List(
      HTMLBlock(
        "<div>\n" +
          "Hello\n" +
          "</div>\n",
      ),
      Paragraph(List(Text("This is a paragraph."))),
    )
  }
}
