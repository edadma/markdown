package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AutolinkAndHtmlTagInlineParserTest extends AnyFlatSpec with Matchers {

  "The inline parser" should "process basic URI autolinks correctly" in {
    val input   = "Visit <https://example.com> for more information."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Visit "),
      AutoLink("https://example.com", "https://example.com"),
      Text(" for more information."),
    )
  }

  it should "process URI autolinks with query parameters" in {
    val input   = "Check out <https://example.com?foo=bar&id=123>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Check out "),
      AutoLink("https://example.com?foo=bar&id=123", "https://example.com?foo=bar&id=123"),
    )
  }

  it should "process URI autolinks with different schemes" in {
    val input   = "Various schemes: <http://test.com> <https://secure.org> <ftp://files.net> <mailto:user@example.com>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Various schemes: "),
      AutoLink("http://test.com", "http://test.com"),
      Text(" "),
      AutoLink("https://secure.org", "https://secure.org"),
      Text(" "),
      AutoLink("ftp://files.net", "ftp://files.net"),
      Text(" "),
      AutoLink("mailto:user@example.com", "mailto:user@example.com"),
    )
  }

  it should "process uppercase scheme autolinks" in {
    val input   = "Uppercase scheme: <MAILTO:USER@EXAMPLE.COM>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Uppercase scheme: "),
      AutoLink("MAILTO:USER@EXAMPLE.COM", "MAILTO:USER@EXAMPLE.COM"),
    )
  }

  it should "process email autolinks correctly" in {
    val input   = "Contact <user@example.com> for more info."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Contact "),
      AutoLink("mailto:user@example.com", "user@example.com"),
      Text(" for more info."),
    )
  }

  it should "process email autolinks with complex local parts" in {
    val input   = "Email with symbols: <foo+special@bar.baz-site.com>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Email with symbols: "),
      AutoLink("mailto:foo+special@bar.baz-site.com", "foo+special@bar.baz-site.com"),
    )
  }

  it should "not treat invalid URL-like constructs as autolinks" in {
    val input   = "Not autolinks: < https://foo.bar> <foo bar> <https://example.com space>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Not autolinks: < https://foo.bar> "),
      RawHTML("<foo bar>"),
      Text(" <https://example.com space>"),
    )
  }

  it should "not treat backslash-escaped brackets as autolinks" in {
    val input   = "Escaped: \\<https://example.com>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Escaped: <https://example.com>"),
    )
  }

  it should "treat incomplete autolinks as literal text" in {
    val input   = "Incomplete: <https://example.com"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Incomplete: <https://example.com"),
    )
  }

  it should "process simple HTML tags correctly" in {
    val input   = "This is a <strong>test</strong> of HTML tags."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is a "),
      RawHTML("<strong>"),
      Text("test"),
      RawHTML("</strong>"),
      Text(" of HTML tags."),
    )
  }

  it should "process self-closing HTML tags correctly" in {
    val input   = "Here's an image: <img src=\"test.jpg\" alt=\"Test\" />"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Here's an image: "),
      RawHTML("<img src=\"test.jpg\" alt=\"Test\" />"),
    )
  }

  it should "process HTML tags with attributes correctly" in {
    val input   = "Custom tag: <responsive-image src=\"foo.jpg\" class=\"large\" id='my-img' data-test=value />"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Custom tag: "),
      RawHTML("<responsive-image src=\"foo.jpg\" class=\"large\" id='my-img' data-test=value />"),
    )
  }

  it should "process HTML comments correctly" in {
    val input   = "Text <!-- this is a comment --> more text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Text "),
      RawHTML("<!-- this is a comment -->"),
      Text(" more text."),
    )
  }

  it should "process HTML processing instructions correctly" in {
    val input   = "PHP: <?php echo $var; ?> end."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("PHP: "),
      RawHTML("<?php echo $var; ?>"),
      Text(" end."),
    )
  }

  it should "process CDATA sections correctly" in {
    val input   = "CDATA: <![CDATA[foo & bar]]> end."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("CDATA: "),
      RawHTML("<![CDATA[foo & bar]]>"),
      Text(" end."),
    )
  }

  it should "process declarations correctly" in {
    val input   = "Declaration: <!DOCTYPE html> text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Declaration: "),
      RawHTML("<!DOCTYPE html>"),
      Text(" text."),
    )
  }

  it should "not treat invalid HTML tags as HTML" in {
    val input   = "Not HTML: <<foo> <42> <foo<bar> <foo bar"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Not HTML: <"),
      RawHTML("<foo>"),
      Text(" <42> <foo"),
      RawHTML("<bar>"),
      Text(" <foo bar"),
    )
  }

  it should "handle tag names with hyphens and digits" in {
    val input   = "Custom elements: <custom-element> and <h2-title> and <n2-tag>"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Custom elements: "),
      RawHTML("<custom-element>"),
      Text(" and "),
      RawHTML("<h2-title>"),
      Text(" and "),
      RawHTML("<n2-tag>"),
    )
  }

  it should "handle precedence correctly between code spans and autolinks" in {
    val input   = "`<https://example.com>`"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      CodeSpan("<https://example.com>"),
    )
  }

  it should "handle autolinks and code spans in the same paragraph" in {
    val input   = "Visit <https://example.com> and use `code` and <strong>HTML</strong>."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("Visit "),
      AutoLink("https://example.com", "https://example.com"),
      Text(" and use "),
      CodeSpan("code"),
      Text(" and "),
      RawHTML("<strong>"),
      Text("HTML"),
      RawHTML("</strong>"),
      Text("."),
    )
  }
}
