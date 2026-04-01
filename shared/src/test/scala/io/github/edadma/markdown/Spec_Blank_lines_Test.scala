package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Blank_lines_Test extends AnyFreeSpec with Matchers:
  "227 - 3632 - 3644" in {
    renderToHTML("  \n\naaa\n  \n\n# aaa\n\n  \n") shouldBe "<p>aaa</p>\n<h1>aaa</h1>\n"
  }
