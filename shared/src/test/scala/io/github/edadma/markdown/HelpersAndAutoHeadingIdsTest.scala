package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for the public helpers added in 0.4.2:
  *
  *   - [[Document.headings]]
  *   - [[plainText]]
  *   - [[renderInlines]] (now public)
  *   - [[MarkdownConfig.autoHeadingIds]] / [[MarkdownConfig.slugify]]
  *   - [[MarkdownConfig.defaultSlugify]]
  */
class HelpersAndAutoHeadingIdsTest extends AnyFreeSpec with Matchers {

  "Document.headings" - {
    "returns top-level Heading blocks in source order" in {
      val doc = parseDocumentContent(
        """# One
          |
          |Body.
          |
          |## Two
          |
          |### Three
          |""".stripMargin,
      )

      doc.headings.map(h => h.level -> plainText(h.inlines)) shouldBe
        List(1 -> "One", 2 -> "Two", 3 -> "Three")
    }

    "returns Nil when there are no headings" in {
      parseDocumentContent("Just a paragraph.\n").headings shouldBe Nil
    }
  }

  "plainText" - {
    "flattens text-only inlines" in {
      val doc = parseDocumentContent("# Hello world\n")
      plainText(doc.headings.head.inlines) shouldBe "Hello world"
    }

    "strips formatting" in {
      val doc = parseDocumentContent("# Hello **bold** *and* `code`\n")
      plainText(doc.headings.head.inlines) shouldBe "Hello bold and code"
    }

    "follows link text" in {
      val doc = parseDocumentContent("# See [the docs](/docs)\n")
      plainText(doc.headings.head.inlines) shouldBe "See the docs"
    }

    "escape = true XML-escapes the result" in {
      val doc = parseDocumentContent("# A & B < C\n")
      plainText(doc.headings.head.inlines, escape = true) shouldBe "A &amp; B &lt; C"
    }
  }

  "renderInlines (public)" - {
    "renders bold + emphasis + code" in {
      val doc      = parseDocumentContent("# Hello **bold** *and* `code`\n")
      val inlines  = doc.headings.head.inlines
      renderInlines(inlines) shouldBe "Hello <strong>bold</strong> <em>and</em> <code>code</code>"
    }
  }

  "auto heading ids — off by default" in {
    val html = renderToHTML("## Hello, World!\n")
    html.trim shouldBe "<h2>Hello, World!</h2>"
  }

  "auto heading ids — on" in {
    val cfg  = MarkdownConfig.default.copy(autoHeadingIds = true)
    val html = renderToHTML("## Hello, World!\n", cfg)
    html.trim shouldBe """<h2 id="hello-world">Hello, World!</h2>"""
  }

  "auto heading ids — explicit id wins (with attributes extension)" in {
    val cfg  = MarkdownConfig.default.copy(attributes = true, autoHeadingIds = true)
    val html = renderToHTML("## Hello, World! {#explicit}\n", cfg)
    html.trim shouldBe """<h2 id="explicit">Hello, World!</h2>"""
  }

  "auto heading ids — multiple headings get distinct slugs" in {
    val cfg = MarkdownConfig.default.copy(autoHeadingIds = true)
    val doc = parseDocumentContent("# One\n## Two Words\n### Three—Four\n", cfg)
    doc.headings.map(_.attrs.flatMap(_.id)) shouldBe
      List(Some("one"), Some("two-words"), Some("three-four"))
  }

  "auto heading ids — slugify is pluggable" in {
    val cfg  = MarkdownConfig.default.copy(autoHeadingIds = true, slugify = s => s.toUpperCase)
    val html = renderToHTML("## hello\n", cfg)
    html.trim shouldBe """<h2 id="HELLO">hello</h2>"""
  }

  "defaultSlugify" - {
    val s = MarkdownConfig.defaultSlugify

    "lowercases" in       { s("Hello") shouldBe "hello" }
    "collapses runs" in   { s("Hello,    World!") shouldBe "hello-world" }
    "strips edges" in     { s("---hello---") shouldBe "hello" }
    "preserves digits" in { s("Section 2.5") shouldBe "section-2-5" }
    "non-ASCII" in        { s("café") shouldBe "café" } // letters preserved by Char.isLetterOrDigit
    "empty" in            { s("!!!") shouldBe "" }
  }
}
