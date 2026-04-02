package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GFMExtensionsTest extends AnyFreeSpec with Matchers:

  // Strikethrough

  "strikethrough" - {
    val config = MarkdownConfig(strikethrough = true)

    "basic strikethrough with ~~" in {
      renderToHTML("~~hello~~\n", config) shouldBe "<p><del>hello</del></p>\n"
    }

    "strikethrough with inline content" in {
      renderToHTML("~~hello **world**~~\n", config) shouldBe "<p><del>hello <strong>world</strong></del></p>\n"
    }

    "strikethrough in a sentence" in {
      renderToHTML("This is ~~deleted~~ text.\n", config) shouldBe "<p>This is <del>deleted</del> text.</p>\n"
    }

    "single tilde is not strikethrough" in {
      renderToHTML("~hello~\n", config) shouldBe "<p>~hello~</p>\n"
    }

    "disabled by default" in {
      renderToHTML("~~hello~~\n") shouldBe "<p>~~hello~~</p>\n"
    }
  }

  // Task List Items

  "task list items" - {
    val config = MarkdownConfig(taskListItems = true)

    "unchecked task item" in {
      renderToHTML("- [ ] todo\n", config) shouldBe
        """<ul>
          |<li><input disabled="" type="checkbox"> todo</li>
          |</ul>
          |""".stripMargin
    }

    "checked task item" in {
      renderToHTML("- [x] done\n", config) shouldBe
        """<ul>
          |<li><input checked="" disabled="" type="checkbox"> done</li>
          |</ul>
          |""".stripMargin
    }

    "uppercase X also works" in {
      renderToHTML("- [X] done\n", config) shouldBe
        """<ul>
          |<li><input checked="" disabled="" type="checkbox"> done</li>
          |</ul>
          |""".stripMargin
    }

    "mixed regular and task items" in {
      renderToHTML("- [ ] todo\n- regular\n- [x] done\n", config) shouldBe
        """<ul>
          |<li><input disabled="" type="checkbox"> todo</li>
          |<li>regular</li>
          |<li><input checked="" disabled="" type="checkbox"> done</li>
          |</ul>
          |""".stripMargin
    }

    "disabled by default" in {
      renderToHTML("- [ ] todo\n") shouldBe "<ul>\n<li>[ ] todo</li>\n</ul>\n"
    }
  }

  // Extended Autolinks

  "extended autolinks" - {
    val config = MarkdownConfig(extendedAutolinks = true)

    "http URL" in {
      renderToHTML("Visit http://example.com today.\n", config) shouldBe
        "<p>Visit <a href=\"http://example.com\">http://example.com</a> today.</p>\n"
    }

    "https URL" in {
      renderToHTML("Visit https://example.com today.\n", config) shouldBe
        "<p>Visit <a href=\"https://example.com\">https://example.com</a> today.</p>\n"
    }

    "www URL gets http:// prefix" in {
      renderToHTML("Visit www.example.com today.\n", config) shouldBe
        "<p>Visit <a href=\"http://www.example.com\">www.example.com</a> today.</p>\n"
    }

    "URL with path" in {
      renderToHTML("See https://example.com/path/to/page for details.\n", config) shouldBe
        "<p>See <a href=\"https://example.com/path/to/page\">https://example.com/path/to/page</a> for details.</p>\n"
    }

    "trailing punctuation stripped" in {
      renderToHTML("Visit https://example.com.\n", config) shouldBe
        "<p>Visit <a href=\"https://example.com\">https://example.com</a>.</p>\n"
    }

    "disabled by default" in {
      renderToHTML("Visit http://example.com today.\n") shouldBe
        "<p>Visit http://example.com today.</p>\n"
    }
  }
