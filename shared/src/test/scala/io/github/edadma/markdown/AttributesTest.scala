package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AttributesTest extends AnyFreeSpec with Matchers:

  val config = MarkdownConfig(attributes = true)

  "attributes" - {
    "ATX heading with id" in {
      renderToHTML("# Hello {#intro}\n", config) shouldBe """<h1 id="intro">Hello</h1>""" + "\n"
    }

    "ATX heading with class" in {
      renderToHTML("## Title {.highlight}\n", config) shouldBe """<h2 class="highlight">Title</h2>""" + "\n"
    }

    "ATX heading with id and class" in {
      renderToHTML("# Hello {#intro .big}\n", config) shouldBe """<h1 id="intro" class="big">Hello</h1>""" + "\n"
    }

    "ATX heading with key-value" in {
      renderToHTML("# Hello {data-level=\"1\"}\n", config) shouldBe """<h1 data-level="1">Hello</h1>""" + "\n"
    }

    "ATX heading with multiple classes" in {
      renderToHTML("# Hello {.a .b}\n", config) shouldBe """<h1 class="a b">Hello</h1>""" + "\n"
    }

    "fenced code block with attributes" in {
      val input = "```python {#mycode .highlight}\nprint(1)\n```\n"
      val html  = renderToHTML(input, config)
      html should include("""id="mycode"""")
      html should include("""class="highlight"""")
      html should include("""class="language-python"""")
    }

    "image with attributes" in {
      val input = "![alt](img.png){#photo .responsive}\n"
      val html  = renderToHTML(input, config)
      html should include("""id="photo"""")
      html should include("""class="responsive"""")
      html should include("""src="img.png"""")
    }

    "disabled by default" in {
      renderToHTML("# Hello {#intro}\n") shouldBe "<h1>Hello {#intro}</h1>\n"
    }

    "heading without attributes unchanged" in {
      renderToHTML("# Plain heading\n", config) shouldBe "<h1>Plain heading</h1>\n"
    }

    "setext heading with attributes" in {
      val input = "Hello {#intro}\n===\n"
      val html  = renderToHTML(input, config)
      html shouldBe """<h1 id="intro">Hello</h1>""" + "\n"
    }

    "braces in heading without valid attributes" in {
      renderToHTML("# Hello {world}\n", config) shouldBe "<h1>Hello {world}</h1>\n"
    }
  }
