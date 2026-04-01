package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Precedence_Test extends AnyFreeSpec with Matchers:
  "42 - 840 - 848" in {
    renderToHTML("- `one\n- two`\n") shouldBe "<ul>\n<li>`one</li>\n<li>two`</li>\n</ul>\n"
  }
