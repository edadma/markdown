package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FootnoteTest extends AnyFreeSpec with Matchers:

  val config = MarkdownConfig(footnotes = true)

  "footnotes" - {
    "basic footnote reference and definition" in {
      val input = "Hello[^1] world.\n\n[^1]: This is a footnote.\n"
      val html  = renderToHTML(input, config)
      html should include("""<sup class="footnote-ref"><a href="#fn-1" id="fnref-1">1</a></sup>""")
      html should include("""<section class="footnotes">""")
      html should include("""<li id="fn-1">""")
      html should include("This is a footnote.")
      html should include("""<a href="#fnref-1" class="footnote-backref">&#8617;</a>""")
    }

    "multiple footnotes numbered in reference order" in {
      val input = "First[^b] then[^a].\n\n[^a]: Alpha.\n\n[^b]: Beta.\n"
      val html  = renderToHTML(input, config)
      // [^b] referenced first → number 1, [^a] second → number 2
      html should include("""id="fnref-b">1</a>""")
      html should include("""id="fnref-a">2</a>""")
    }

    "footnote with multi-line content" in {
      val input = "Text[^note].\n\n[^note]: First paragraph.\n\n    Second paragraph.\n"
      val html  = renderToHTML(input, config)
      html should include("""<li id="fn-note">""")
      html should include("First paragraph.")
      html should include("Second paragraph.")
    }

    "unreferenced footnote definition produces no footnote section" in {
      val input = "No references here.\n\n[^unused]: This is unused.\n"
      val html  = renderToHTML(input, config)
      html should not include """<section class="footnotes">"""
    }

    "footnote reference without definition still renders" in {
      val input = "Text[^missing].\n"
      val html  = renderToHTML(input, config)
      html should include("""href="#fn-missing""")
    }

    "disabled by default" in {
      val input = "Text[^1].\n\n[^1]: Footnote.\n"
      val html  = renderToHTML(input)
      html should not include """<sup"""
    }

    "footnote with inline formatting in content" in {
      val input = "Text[^1].\n\n[^1]: This has **bold** content.\n"
      val html  = renderToHTML(input, config)
      html should include("<strong>bold</strong>")
    }

    "escaped caret is not a footnote" in {
      val input = "Text[\\^1].\n"
      val html  = renderToHTML(input, config)
      html should not include """<sup"""
    }
  }
