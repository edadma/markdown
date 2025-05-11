package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Autolinks_Test extends AnyFreeSpec with Matchers:
  "594 - 8780 - 8784" in {
    renderToHTML("<http://foo.bar.baz>\n") shouldBe "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>\n"
  }
  "595 - 8787 - 8791" in {
    renderToHTML("<https://foo.bar.baz/test?q=hello&id=22&boolean>\n") shouldBe "<p><a href=\"https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean\">https://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>\n"
  }
  "596 - 8794 - 8798" in {
    renderToHTML("<irc://foo.bar:2233/baz>\n") shouldBe "<p><a href=\"irc://foo.bar:2233/baz\">irc://foo.bar:2233/baz</a></p>\n"
  }
  "597 - 8803 - 8807" in {
    renderToHTML("<MAILTO:FOO@BAR.BAZ>\n") shouldBe "<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>\n"
  }
  "598 - 8815 - 8819" in {
    renderToHTML("<a+b+c:d>\n") shouldBe "<p><a href=\"a+b+c:d\">a+b+c:d</a></p>\n"
  }
  "599 - 8822 - 8826" in {
    renderToHTML("<made-up-scheme://foo,bar>\n") shouldBe "<p><a href=\"made-up-scheme://foo,bar\">made-up-scheme://foo,bar</a></p>\n"
  }
  "600 - 8829 - 8833" in {
    renderToHTML("<https://../>\n") shouldBe "<p><a href=\"https://../\">https://../</a></p>\n"
  }
  "601 - 8836 - 8840" in {
    renderToHTML("<localhost:5001/foo>\n") shouldBe "<p><a href=\"localhost:5001/foo\">localhost:5001/foo</a></p>\n"
  }
  "602 - 8845 - 8849" in {
    renderToHTML("<https://foo.bar/baz bim>\n") shouldBe "<p>&lt;https://foo.bar/baz bim&gt;</p>\n"
  }
  "603 - 8854 - 8858" in {
    renderToHTML("<https://example.com/\\[\\>\n") shouldBe "<p><a href=\"https://example.com/%5C%5B%5C\">https://example.com/\\[\\</a></p>\n"
  }
  "604 - 8876 - 8880" in {
    renderToHTML("<foo@bar.example.com>\n") shouldBe "<p><a href=\"mailto:foo@bar.example.com\">foo@bar.example.com</a></p>\n"
  }
  "605 - 8883 - 8887" in {
    renderToHTML("<foo+special@Bar.baz-bar0.com>\n") shouldBe "<p><a href=\"mailto:foo+special@Bar.baz-bar0.com\">foo+special@Bar.baz-bar0.com</a></p>\n"
  }
  "606 - 8892 - 8896" in {
    renderToHTML("<foo\\+@bar.example.com>\n") shouldBe "<p>&lt;foo+@bar.example.com&gt;</p>\n"
  }
  "607 - 8901 - 8905" in {
    renderToHTML("<>\n") shouldBe "<p>&lt;&gt;</p>\n"
  }
  "608 - 8908 - 8912" in {
    renderToHTML("< https://foo.bar >\n") shouldBe "<p>&lt; https://foo.bar &gt;</p>\n"
  }
  "609 - 8915 - 8919" in {
    renderToHTML("<m:abc>\n") shouldBe "<p>&lt;m:abc&gt;</p>\n"
  }
  "610 - 8922 - 8926" in {
    renderToHTML("<foo.bar.baz>\n") shouldBe "<p>&lt;foo.bar.baz&gt;</p>\n"
  }
  "611 - 8929 - 8933" in {
    renderToHTML("https://example.com\n") shouldBe "<p>https://example.com</p>\n"
  }
  "612 - 8936 - 8940" in {
    renderToHTML("foo@bar.example.com\n") shouldBe "<p>foo@bar.example.com</p>\n"
  }
