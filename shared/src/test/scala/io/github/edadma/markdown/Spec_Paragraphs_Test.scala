package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Paragraphs_Test extends AnyFreeSpec with Matchers:
  "219 - 3525 - 3532" in {
    renderToHTML("aaa\n\nbbb\n") shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }
  "220 - 3537 - 3548" in {
    renderToHTML("aaa\nbbb\n\nccc\nddd\n") shouldBe "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
  }
  "221 - 3553 - 3561" in {
    renderToHTML("aaa\n\n\nbbb\n") shouldBe "<p>aaa</p>\n<p>bbb</p>\n"
  }
  "222 - 3566 - 3572" in {
    renderToHTML("  aaa\n bbb\n") shouldBe "<p>aaa\nbbb</p>\n"
  }
  "223 - 3578 - 3586" in {
    renderToHTML("aaa\n             bbb\n                                       ccc\n") shouldBe "<p>aaa\nbbb\nccc</p>\n"
  }
  "224 - 3592 - 3598" in {
    renderToHTML("   aaa\nbbb\n") shouldBe "<p>aaa\nbbb</p>\n"
  }
  "225 - 3601 - 3608" in {
    renderToHTML("    aaa\nbbb\n") shouldBe "<pre><code>aaa\n</code></pre>\n<p>bbb</p>\n"
  }
  "226 - 3615 - 3621" in {
    renderToHTML("aaa     \nbbb     \n") shouldBe "<p>aaa<br />\nbbb</p>\n"
  }
