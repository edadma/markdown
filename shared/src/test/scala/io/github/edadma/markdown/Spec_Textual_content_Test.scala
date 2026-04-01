package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Textual_content_Test extends AnyFreeSpec with Matchers:
  "650 - 9395 - 9399" in {
    renderToHTML("hello $.;'there\n") shouldBe "<p>hello $.;'there</p>\n"
  }
  "651 - 9402 - 9406" in {
    renderToHTML("Foo χρῆν\n") shouldBe "<p>Foo χρῆν</p>\n"
  }
  "652 - 9411 - 9415" in {
    renderToHTML("Multiple     spaces\n") shouldBe "<p>Multiple     spaces</p>\n"
  }
