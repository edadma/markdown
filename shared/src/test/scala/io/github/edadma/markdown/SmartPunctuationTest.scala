package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SmartPunctuationTest extends AnyFreeSpec with Matchers:

  val config = MarkdownConfig(smartPunctuation = true)

  "smart punctuation" - {
    "em dash from ---" in {
      renderToHTML("Word---word.\n", config) shouldBe "<p>Word\u2014word.</p>\n"
    }

    "en dash from --" in {
      renderToHTML("Pages 1--10.\n", config) shouldBe "<p>Pages 1\u201310.</p>\n"
    }

    "ellipsis from ..." in {
      renderToHTML("Wait...\n", config) shouldBe "<p>Wait\u2026</p>\n"
    }

    "double quotes" in {
      val html = renderToHTML("He said \"hello\" to her.\n", config)
      html shouldBe s"<p>He said \u201Chello\u201D to her.</p>\n"
    }

    "single quotes" in {
      val html = renderToHTML("It's a 'test' here.\n", config)
      // It's → right single quote (apostrophe), 'test' → left then right
      html shouldBe s"<p>It\u2019s a \u2018test\u2019 here.</p>\n"
    }

    "quotes after opening punctuation" in {
      val html = renderToHTML("She said (\"yes\").\n", config)
      html should include("\u201Cyes\u201D")
    }

    "disabled by default" in {
      renderToHTML("\"hello\" -- world...\n") shouldBe "<p>&quot;hello&quot; -- world...</p>\n"
    }

    "smart punctuation inside emphasis" in {
      val html = renderToHTML("*\"hello\"*\n", config)
      html should include("\u201Chello\u201D")
    }

    "em dash takes priority over en dash" in {
      renderToHTML("a---b\n", config) shouldBe "<p>a\u2014b.</p>\n".replace(".", "")
      // Actually let me just check it contains em dash
      val html = renderToHTML("a---b\n", config)
      html should include("\u2014")
      html should not include "\u2013"
    }

    "four dashes" in {
      // ---- should be em dash + single dash
      val html = renderToHTML("a----b\n", config)
      html should include("\u2014")
    }
  }
