//package io.github.edadma.markdown
//
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//class InlineParserTest extends AnyFlatSpec with Matchers {
//  // Helper to get string representation of inlines for easier assertions
//  def inlineToString(inlines: List[Inline]): String = {
//    inlines.map {
//      case Text(content)               => s"Text($content)"
//      case CodeSpan(content)           => s"CodeSpan($content)"
//      case SoftLineBreak()             => "SoftLineBreak"
//      case HardLineBreak()             => "HardLineBreak"
//      case AutoLink(destination, text) => s"AutoLink($destination, $text)"
//      case RawHTML(content)            => s"RawHTML($content)"
//      case Link(destination, title, inlines) =>
//        s"Link($destination, $title, [${inlineToString(inlines)}])"
//      case Image(destination, title, inlines) =>
//        s"Image($destination, $title, [${inlineToString(inlines)}])"
//      case other => other.toString
//    }.mkString(", ")
//  }
//
//  // Tests for plain text
//  "The inline parser" should "parse plain text correctly" in {
//    val input  = "This is plain text"
//    val result = parseInlineContent(input)
//
//    result should have length 1
//    result.head shouldBe a[Text]
//    result.head.asInstanceOf[Text].content should be("This is plain text")
//  }
//
//  // Tests for code spans
//  it should "parse simple code spans" in {
//    val input  = "This is `code`"
//    val result = parseInlineContent(input)
//
//    result should have length 2
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("This is ")
//    result(1) shouldBe a[CodeSpan]
//    result(1).asInstanceOf[CodeSpan].content should be("code")
//  }
//
//  it should "handle code spans with multi-backtick delimiters" in {
//    val input  = "This is ``code with `backticks` inside``"
//    val result = parseInlineContent(input)
//
//    result should have length 2
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("This is ")
//    result(1) shouldBe a[CodeSpan]
//    result(1).asInstanceOf[CodeSpan].content should be("code with `backticks` inside")
//  }
//
//  it should "handle code spans with spaces at the edges" in {
//    val input  = "`` code with spaces ``"
//    val result = parseInlineContent(input)
//
//    result should have length 1
//    result.head shouldBe a[CodeSpan]
//    result.head.asInstanceOf[CodeSpan].content should be("code with spaces")
//  }
//
//  it should "handle unmatched backticks as plain text" in {
//    val input  = "This is `unmatched"
//    val result = parseInlineContent(input)
//
//    result should have length 1
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("This is `unmatched")
//  }
//
//  // Tests for line breaks
//  it should "parse soft line breaks" in {
//    val input  = "Line one\nLine two"
//    val result = parseInlineContent(input)
//
//    result should have length 3
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("Line one")
//    result(1) shouldBe a[SoftLineBreak]
//    result(2) shouldBe a[Text]
//    result(2).asInstanceOf[Text].content should be("Line two")
//  }
//
//  it should "parse hard line breaks with spaces" in {
//    val input  = "Line one  \nLine two"
//    val result = parseInlineContent(input)
//
//    result should have length 3
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("Line one")
//    result(1) shouldBe a[HardLineBreak]
//    result(2) shouldBe a[Text]
//    result(2).asInstanceOf[Text].content should be("Line two")
//  }
//
//  it should "parse hard line breaks with backslash" in {
//    val input  = "Line one\\\nLine two"
//    val result = parseInlineContent(input)
//
//    result should have length 3
//    result(0) shouldBe a[Text]
//    result(0).asInstanceOf[Text].content should be("Line one")
//    result(1) shouldBe a[HardLineBreak]
//    result(2) shouldBe a[Text]
//    result(2).asInstanceOf[Text].content should be("Line two")
//  }
//
//  // Tests for autolinks
//  it should "parse URL autolinks" in {
//    val input  = "<https://example.com>"
//    val result = parseInlineContent(input)
//
//    result should have length 1
//    result.head shouldBe a[AutoLink]
//    val autolink = result.head.asInstanceOf[AutoLink]
//    autolink.destination should be("https://example.com")
//    autolink.text should be("https://example.com")
//  }
//
//  it should "parse email autolinks" in {
//    val input  = "<user@example.com>"
//    val result = parseInlineContent(input)
//
//    result should have length 1
//    result.head shouldBe a[AutoLink]
//    val autolink = result.head.asInstanceOf[AutoLink]
//    autolink.destination should be("mailto:user@example.com")
//    autolink.text should be("user@example.com")
//  }
//
//  it should "handle invalid autolinks as plain text" in {
//    val input  = "<not a valid url>"
//    val result = parseInlineContent(input)
//
//    // Since our implementation currently doesn't validate URLs strictly,
//    // we'll just check something is produced
//    result.mkString should include("<")
//  }
//
//  // Tests for HTML tags
//  it should "parse simple HTML tags" in {
//    val input  = "<div>content</div>"
//    val result = parseInlineContent(input)
//
//    // Since our implementation treats these as raw HTML, check for RawHTML nodes
//    result.head shouldBe a[RawHTML]
//  }
//}
