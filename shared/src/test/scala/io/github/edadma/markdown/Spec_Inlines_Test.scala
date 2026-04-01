package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Inlines_Test extends AnyFreeSpec with Matchers:
  "327 - 5854 - 5858" in {
    renderToHTML("`hi`lo`\n") shouldBe "<p><code>hi</code>lo`</p>\n"
  }
