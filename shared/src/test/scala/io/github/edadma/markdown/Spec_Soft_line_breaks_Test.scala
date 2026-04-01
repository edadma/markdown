package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Soft_line_breaks_Test extends AnyFreeSpec with Matchers:
  "648 - 9363 - 9369" in {
    renderToHTML("foo\nbaz\n") shouldBe "<p>foo\nbaz</p>\n"
  }
  "649 - 9375 - 9381" in {
    renderToHTML("foo \n baz\n") shouldBe "<p>foo\nbaz</p>\n"
  }
