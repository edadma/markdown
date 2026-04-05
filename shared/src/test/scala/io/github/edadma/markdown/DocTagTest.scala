package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DocTagTest extends AnyFreeSpec with Matchers:

  // Standard registry used in most tests: @api (no target, inline), @param (target, inline),
  // @returns (no target, inline), @example (no target, block), @literal (no target, opaque).
  private val stdRegistry = TagRegistry(
    TagDefinition("api", acceptsTarget = false, ContentMode.InlineMarkdown),
    TagDefinition("param", acceptsTarget = true, ContentMode.InlineMarkdown),
    TagDefinition("returns", acceptsTarget = false, ContentMode.InlineMarkdown),
    TagDefinition("example", acceptsTarget = false, ContentMode.BlockMarkdown),
    TagDefinition("literal", acceptsTarget = false, ContentMode.Opaque),
  )

  private val lenientCfg = MarkdownConfig(
    docTags = DocTagConfig(enabled = true, registry = stdRegistry, strictUnknownTags = false),
  )

  private val strictCfg = MarkdownConfig(
    docTags = DocTagConfig(enabled = true, registry = stdRegistry, strictUnknownTags = true),
  )

  private def parse(md: String, cfg: MarkdownConfig = lenientCfg): Document =
    parseDocumentContent(md, cfg)

  private def firstDocTag(doc: Document): DocTagBlock =
    doc.children.collect { case d: DocTagBlock => d }.head

  "disabled by default" - {
    "@ lines parse as ordinary paragraph text" in {
      val md  = "@param x description\n"
      val doc = parse(md, MarkdownConfig.default)
      doc.children.head shouldBe a[Paragraph]
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
    }

    "CommonMark example containing @ is unaffected when disabled" in {
      val html = renderToHTML("email@example.com is text.\n")
      html should include("email@example.com")
      html should not include "doc-tag"
    }
  }

  "trigger recognition" - {
    "simple tag with inline body" in {
      val doc = parse("@api reports a fatal error\n")
      val t   = firstDocTag(doc)
      t.name shouldBe "api"
      t.target shouldBe None
    }

    "tag name supports letters, digits, underscore, hyphen" in {
      val reg = TagRegistry(
        TagDefinition("since-2", acceptsTarget = false, ContentMode.InlineMarkdown),
        TagDefinition("see_also", acceptsTarget = false, ContentMode.InlineMarkdown),
      )
      val cfg = MarkdownConfig(docTags = DocTagConfig(enabled = true, registry = reg))
      val d1  = parseDocumentContent("@since-2 v2.0\n", cfg)
      val d2  = parseDocumentContent("@see_also foo\n", cfg)
      firstDocTag(d1).name shouldBe "since-2"
      firstDocTag(d2).name shouldBe "see_also"
    }

    "tag name must start with a letter — digit start is not a tag" in {
      val doc = parse("@1invalid body\n")
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
    }

    "escaped \\@ is literal, not a tag" in {
      val doc = parse("\\@param x body\n")
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
      doc.children.head shouldBe a[Paragraph]
    }

    "@ in middle of line is literal" in {
      val doc = parse("text @param x\n")
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
      doc.children.head shouldBe a[Paragraph]
    }
  }

  "target parsing (acceptsTarget = true)" - {
    "identifier target with dash separator" in {
      val t = firstDocTag(parse("@param msg — human-readable description\n"))
      t.name shouldBe "param"
      t.target shouldBe Some("msg")
      t.body match {
        case Paragraph(inlines) :: Nil =>
          val txt = inlines.collect { case Text(s) => s; case c: C => c.char.toString }.mkString
          txt should include("human-readable description")
        case _ => fail(s"unexpected body: ${t.body}")
      }
    }

    "identifier target with colon separator" in {
      val t = firstDocTag(parse("@param name: the name\n"))
      t.target shouldBe Some("name")
    }

    "identifier target with hyphen separator" in {
      val t = firstDocTag(parse("@param name - the name\n"))
      t.target shouldBe Some("name")
    }

    "no target when next token is not an identifier" in {
      val t = firstDocTag(parse("@param 123 oh hi\n"))
      t.target shouldBe None
    }

    "no target when body is just the separator + text" in {
      val t = firstDocTag(parse("@param : bare description\n"))
      t.target shouldBe None
    }
  }

  "acceptsTarget = false" - {
    "does not consume an identifier after the name" in {
      val t = firstDocTag(parse("@api reports a fatal error\n"))
      t.name shouldBe "api"
      t.target shouldBe None
      // The word "reports" must remain in the body.
      val html = renderToHTML("@api reports a fatal error\n", lenientCfg)
      html should include("reports a fatal error")
    }

    "does not strip leading separators" in {
      val t = firstDocTag(parse("@api - literal dash at start\n"))
      t.target shouldBe None
      // The leading '-' should still be part of the body — rendered as "- literal dash at start".
      val html = renderToHTML("@api - literal dash at start\n", lenientCfg)
      html should include("- literal dash at start")
    }
  }

  "content mode: Opaque" - {
    "body stored as a single Text node, no inline parsing" in {
      val doc = parse("@literal text with **markdown** and `code`\n")
      val t   = firstDocTag(doc)
      t.contentMode shouldBe ContentMode.Opaque
      t.body match {
        case Paragraph(List(Text(s))) :: Nil =>
          s shouldBe "text with **markdown** and `code`"
        case other => fail(s"expected opaque single Text: $other")
      }
      // In rendered HTML, ** should be escaped (not bolded)
      val html = renderToHTML("@literal text with **markdown** and `code`\n", lenientCfg)
      html should not include "<strong>"
      html should include("**markdown**")
    }
  }

  "content mode: InlineMarkdown" - {
    "emphasis and code spans are parsed" in {
      val html = renderToHTML("@api prints *pretty* `things`\n", lenientCfg)
      html should include("<em>pretty</em>")
      html should include("<code>things</code>")
    }

    "list syntax is NOT parsed as a block list" in {
      val html = renderToHTML("@api - one - two\n", lenientCfg)
      html should not include "<ul>"
    }
  }

  "content mode: BlockMarkdown" - {
    "parses list structure within body" in {
      val md =
        """@example
          |    - one
          |    - two
          |""".stripMargin
      val html = renderToHTML(md, lenientCfg)
      html should include("<ul>")
      html should include("<li>")
      html should include("one")
      html should include("two")
    }

    "parses code fence inside body" in {
      val md =
        """@example
          |    ```
          |    some code
          |    ```
          |""".stripMargin
      val html = renderToHTML(md, lenientCfg)
      html should include("<pre>")
      html should include("some code")
    }
  }

  "body continuation" - {
    "continuation lines with greater indent are joined" in {
      val md =
        """@api summary line
          |    continuation line
          |""".stripMargin
      val html = renderToHTML(md, lenientCfg)
      html should include("summary line")
      html should include("continuation line")
    }

    "blank line terminates the body" in {
      val md =
        """@api first
          |
          |Next paragraph.
          |""".stripMargin
      val doc  = parse(md)
      val tags = doc.children.collect { case d: DocTagBlock => d }
      tags.size shouldBe 1
      doc.children.exists(_.isInstanceOf[Paragraph]) shouldBe true
    }

    "line indented at or below the @ column terminates" in {
      val md =
        """@api summary
          |text at column 0 is not continuation
          |""".stripMargin
      val doc  = parse(md)
      val tags = doc.children.collect { case d: DocTagBlock => d }
      tags.size shouldBe 1
      doc.children.exists(_.isInstanceOf[Paragraph]) shouldBe true
    }

    "another @-tag line terminates and starts a new tag" in {
      val md =
        """@api first tag
          |@param x second tag
          |""".stripMargin
      val doc  = parse(md)
      val tags = doc.children.collect { case d: DocTagBlock => d }
      tags.map(_.name) shouldBe List("api", "param")
      tags(1).target shouldBe Some("x")
    }

    "EOF terminates the body" in {
      val md = "@api no trailing newline"
      val doc  = parse(md)
      val tags = doc.children.collect { case d: DocTagBlock => d }
      tags.size shouldBe 1
      tags.head.name shouldBe "api"
    }
  }

  "unknown tags" - {
    "lenient mode (default) emits a DocTagBlock" in {
      val doc = parse("@unknown some content\n", lenientCfg)
      val t   = firstDocTag(doc)
      t.name shouldBe "unknown"
      t.contentMode shouldBe ContentMode.InlineMarkdown
    }

    "strict mode falls back to paragraph text" in {
      val doc = parseDocumentContent("@unknown some content\n", strictCfg)
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
      doc.children.head shouldBe a[Paragraph]
    }

    "strict mode still recognizes known tags" in {
      val doc = parseDocumentContent("@api known tag\n", strictCfg)
      val tags = doc.children.collect { case d: DocTagBlock => d }
      tags.size shouldBe 1
      tags.head.name shouldBe "api"
    }
  }

  "interaction with containers" - {
    "inside a blockquote" in {
      val md =
        """> @api summary inside quote
          |""".stripMargin
      val html = renderToHTML(md, lenientCfg)
      html should include("<blockquote>")
      html should include("doc-tag-api")
    }

    "inside a list item" in {
      val md =
        """- @api inside list
          |""".stripMargin
      val html = renderToHTML(md, lenientCfg)
      html should include("<ul>")
      html should include("doc-tag-api")
    }

    "suppressed inside fenced code block" in {
      val md =
        """```
          |@api not a tag here
          |```
          |""".stripMargin
      val doc = parse(md)
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
      doc.children.exists(_.isInstanceOf[Code]) shouldBe true
    }

    "suppressed inside indented code block" in {
      val md = "    @api not a tag\n"
      val doc = parse(md)
      doc.children.find(_.isInstanceOf[DocTagBlock]) shouldBe None
      doc.children.head shouldBe a[Code]
    }

    "suppressed inside code span" in {
      val html = renderToHTML("Text `@api literal` text.\n", lenientCfg)
      html should include("<code>@api literal</code>")
      html should not include "doc-tag"
    }
  }

  "default HTML rendering" - {
    "tag without target renders name only in dt" in {
      val html = renderToHTML("@api summary\n", lenientCfg)
      html should include("""class="doc-tag doc-tag-api"""")
      html should include("<dt>api</dt>")
      html should include("summary")
    }

    "tag with target renders name: target in dt" in {
      val html = renderToHTML("@param msg — body\n", lenientCfg)
      html should include("""class="doc-tag doc-tag-param"""")
      html should include("<dt>param: msg</dt>")
    }
  }

  "CommonMark conformance" - {
    "ordinary markdown parses the same with the extension enabled (no @-lines)" in {
      val md = "# Heading\n\nA paragraph with **bold** and _em_.\n\n- item 1\n- item 2\n"
      val a  = renderToHTML(md, MarkdownConfig.default)
      val b  = renderToHTML(md, lenientCfg)
      a shouldBe b
    }

    "ordinary markdown with email parses the same" in {
      val md = "Contact: user@example.com for info.\n"
      val a  = renderToHTML(md, MarkdownConfig.default)
      val b  = renderToHTML(md, lenientCfg)
      a shouldBe b
    }
  }
