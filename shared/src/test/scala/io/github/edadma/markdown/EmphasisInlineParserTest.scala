package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EmphasisInlineParserTest extends AnyFlatSpec with Matchers {

  "The inline parser" should "process basic emphasis correctly" in {
    val input   = "This is *emphasized* text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is "),
      Emphasis(List(Text("emphasized"))),
      Text(" text."),
    )
  }

  it should "process basic strong emphasis correctly" in {
    val input   = "This is **strongly emphasized** text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is "),
      Strong(List(Text("strongly emphasized"))),
      Text(" text."),
    )
  }

  it should "process emphasis with underscores" in {
    val input   = "This is _emphasized_ text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is "),
      Emphasis(List(Text("emphasized"))),
      Text(" text."),
    )
  }

  it should "process strong emphasis with underscores" in {
    val input   = "This is __strongly emphasized__ text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is "),
      Strong(List(Text("strongly emphasized"))),
      Text(" text."),
    )
  }

  it should "handle nested emphasis" in {
    val input   = "This is *emphasized with **strong** inside* text."
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("This is "),
      Emphasis(List(
        Text("emphasized with "),
        Strong(List(Text("strong"))),
        Text(" inside"),
      )),
      Text(" text."),
    )
  }

  it should "handle nested emphasis with mixed delimiters" in {
    val input   = "_This has *mixed* delimiters_"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Emphasis(List(
        Text("This has "),
        Emphasis(List(Text("mixed"))),
        Text(" delimiters"),
      )),
    )
  }

  it should "handle emphasis within strong emphasis" in {
    val input   = "**Strong with *emphasis* inside**"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Strong(List(
        Text("Strong with "),
        Emphasis(List(Text("emphasis"))),
        Text(" inside"),
      )),
    )
  }

  it should "handle complex nesting according to rule 14" in {
    val input   = "*Emphasis containing **strong** emphasis*"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Emphasis(List(
        Text("Emphasis containing "),
        Strong(List(Text("strong"))),
        Text(" emphasis"),
      )),
    )
  }

  it should "respect rule 15 for overlapping emphasis" in {
    val input   = "*foo _bar* baz_"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Emphasis(List(
        Text("foo _bar"),
      )),
      Text(" baz_"),
    )
  }

  it should "handle intraword emphasis with asterisks" in {
    val input   = "intra*word*emphasis"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("intra"),
      Emphasis(List(Text("word"))),
      Text("emphasis"),
    )
  }

  it should "not apply intraword emphasis with underscores" in {
    val input   = "intra_word_emphasis"
    val inlines = parseInlineContent(input)

    inlines shouldBe List(
      Text("intra_word_emphasis"),
    )
  }

//  it should "apply the special rules for emphasis with multiple of 3 delimiters" in {
//    val input   = "***triple emphasis***"
//    val inlines = parseInlineContent(input)
//
//    inlines shouldBe List(
//      Emphasis(List(
//        Strong(List(Text("triple emphasis"))),
//      )),
//    )
//  }
}
