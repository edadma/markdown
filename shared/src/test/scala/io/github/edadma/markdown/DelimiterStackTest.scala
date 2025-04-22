package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DelimiterStackTest extends AnyFlatSpec with Matchers {

  "The delimiter stack" should "handle basic emphasis" in {
    val inputs = List(
      "*foo*"   -> "<p><em>foo</em></p>",
      "_foo_"   -> "<p><em>foo</em></p>",
      "**foo**" -> "<p><strong>foo</strong></p>",
      "__foo__" -> "<p><strong>foo</strong></p>",
    )

    for ((input, expected) <- inputs) {
      val reader   = new InputReader(input)
      val document = parseDocument(reader.stream)
      renderToHTML(document) should be(expected)
    }
  }

  it should "handle nested emphasis" in {
    val inputs = List(
      "*foo **bar** baz*" -> "<p><em>foo <strong>bar</strong> baz</em></p>",
      "**foo *bar* baz**" -> "<p><strong>foo <em>bar</em> baz</strong></p>",
      "***foo bar***"     -> "<p><em><strong>foo bar</strong></em></p>",
    )

    for ((input, expected) <- inputs) {
      val reader   = new InputReader(input)
      val document = parseDocument(reader.stream)
      renderToHTML(document) should be(expected)
    }
  }

  it should "handle emphasis with links" in {
    val input    = "*[foo](url)*"
    val expected = "<p><em><a href=\"url\">foo</a></em></p>"

    val reader   = new InputReader(input)
    val document = parseDocument(reader.stream)
    renderToHTML(document) should be(expected)
  }

  it should "handle complex rules for underscore emphasis" in {
    val inputs = List(
      "foo_bar_baz"   -> "<p>foo_bar_baz</p>", // No intraword emphasis with _
      "foo_bar _baz_" -> "<p>foo_bar <em>baz</em></p>",
    )

    for ((input, expected) <- inputs) {
      val reader   = new InputReader(input)
      val document = parseDocument(reader.stream)
      renderToHTML(document) should be(expected)
    }
  }

  it should "handle unmatched delimiters" in {
    val inputs = List(
      "*foo"   -> "<p>*foo</p>",
      "*foo**" -> "<p><em>foo</em>*</p>",
      "**foo*" -> "<p>*<em>foo</em></p>",
    )

    for ((input, expected) <- inputs) {
      val reader   = new InputReader(input)
      val document = parseDocument(reader.stream)
      renderToHTML(document) should be(expected)
    }
  }

  it should "follow precedence rules" in {
    val inputs = List(
      "*foo `bar*` baz" -> "<p>*foo <code>bar*</code> baz</p>",
      "*foo <bar>* baz" -> "<p>*foo <bar>* baz</p>",
    )

    for ((input, expected) <- inputs) {
      val reader   = new InputReader(input)
      val document = parseDocument(reader.stream)
      renderToHTML(document) should be(expected)
    }
  }
}
