package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Hard_line_breaks_Test extends AnyFreeSpec with Matchers:
  "633 - 9212 - 9218" in {
    renderToHTML("foo  \nbaz\n") shouldBe "<p>foo<br />\nbaz</p>\n"
  }
  "634 - 9224 - 9230" in {
    renderToHTML("foo\\\nbaz\n") shouldBe "<p>foo<br />\nbaz</p>\n"
  }
  "635 - 9235 - 9241" in {
    renderToHTML("foo       \nbaz\n") shouldBe "<p>foo<br />\nbaz</p>\n"
  }
  "636 - 9246 - 9252" in {
    renderToHTML("foo  \n     bar\n") shouldBe "<p>foo<br />\nbar</p>\n"
  }
  "637 - 9255 - 9261" in {
    renderToHTML("foo\\\n     bar\n") shouldBe "<p>foo<br />\nbar</p>\n"
  }
  "638 - 9267 - 9273" in {
    renderToHTML("*foo  \nbar*\n") shouldBe "<p><em>foo<br />\nbar</em></p>\n"
  }
  "639 - 9276 - 9282" in {
    renderToHTML("*foo\\\nbar*\n") shouldBe "<p><em>foo<br />\nbar</em></p>\n"
  }
  "640 - 9287 - 9292" in {
    renderToHTML("`code  \nspan`\n") shouldBe "<p><code>code   span</code></p>\n"
  }
  "641 - 9295 - 9300" in {
    renderToHTML("`code\\\nspan`\n") shouldBe "<p><code>code\\ span</code></p>\n"
  }
  "642 - 9305 - 9311" in {
    renderToHTML("<a href=\"foo  \nbar\">\n") shouldBe "<p><a href=\"foo  \nbar\"></p>\n"
  }
  "643 - 9314 - 9320" in {
    renderToHTML("<a href=\"foo\\\nbar\">\n") shouldBe "<p><a href=\"foo\\\nbar\"></p>\n"
  }
  "644 - 9327 - 9331" in {
    renderToHTML("foo\\\n") shouldBe "<p>foo\\</p>\n"
  }
  "645 - 9334 - 9338" in {
    renderToHTML("foo  \n") shouldBe "<p>foo</p>\n"
  }
  "646 - 9341 - 9345" in {
    renderToHTML("### foo\\\n") shouldBe "<h3>foo\\</h3>\n"
  }
  "647 - 9348 - 9352" in {
    renderToHTML("### foo  \n") shouldBe "<h3>foo</h3>\n"
  }
