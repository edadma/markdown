package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DelimiterStackTest extends AnyFlatSpec with Matchers {

  "The delimiter stack" should "handle basic emphasis" in {
    val inputs = List(
      "*foo*"   -> List(Emphasis(List(Text("foo")))),
      "_foo_"   -> List(Emphasis(List(Text("foo")))),
      "**foo**" -> List(Strong(List(Text("foo")))),
      "__foo__" -> List(Strong(List(Text("foo")))),
    )

    for ((input, expected) <- inputs) {
      val result = parseInlineContent(input)
      result should be(expected)
    }
  }

  it should "handle nested emphasis" in {
    val inputs = List(
      "*foo **bar** baz*" -> List(
        Emphasis(List(
          Text("foo "),
          Strong(List(Text("bar"))),
          Text(" baz"),
        )),
      ),
      "**foo *bar* baz**" -> List(
        Strong(List(
          Text("foo "),
          Emphasis(List(Text("bar"))),
          Text(" baz"),
        )),
      ),
      "***foo bar***" -> List(
        Emphasis(List(
          Strong(List(Text("foo bar"))),
        )),
      ),
    )

    for ((input, expected) <- inputs) {
      val result = parseInlineContent(input)
      result should be(expected)
    }
  }

  it should "handle emphasis with links" in {
    val input = "*[foo](url)*"
    val expected = List(
      Emphasis(List(
        Link("url", None, List(Text("foo"))),
      )),
    )

    val result = parseInlineContent(input)
    result should be(expected)
  }

  it should "handle complex rules for underscore emphasis" in {
    val inputs = List(
      "foo_bar_baz" -> List(Text("foo_bar_baz")), // No intraword emphasis with _
      "foo_bar _baz_" -> List(
        Text("foo_bar "),
        Emphasis(List(Text("baz"))),
      ),
    )

    for ((input, expected) <- inputs) {
      val result = parseInlineContent(input)
      result should be(expected)
    }
  }

  it should "handle unmatched delimiters" in {
    val inputs = List(
      "*foo" -> List(Text("*foo")),
      "*foo**" -> List(
        Emphasis(List(Text("foo"))),
        Text("*"),
      ),
      "**foo*" -> List(
        Text("*"),
        Emphasis(List(Text("foo"))),
      ),
    )

    for ((input, expected) <- inputs) {
      val result = parseInlineContent(input)
      result should be(expected)
    }
  }

  it should "follow precedence rules" in {
    val inputs = List(
      "*foo `bar*` baz" -> List(
        Text("*foo "),
        CodeSpan("bar*"),
        Text(" baz"),
      ),
      "*foo <bar>* baz" -> List(
        Text("*foo "),
        RawHTML("<bar>"),
        Text("* baz"),
      ),
    )

    for ((input, expected) <- inputs) {
      val result = parseInlineContent(input)
      result should be(expected)
    }
  }
}
