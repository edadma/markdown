package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Backslash_escapes_Test extends AnyFreeSpec with Matchers:
  "12 - 489 - 493" in {
    renderToHTML("\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~\n") shouldBe "<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n"
  }
  "13 - 499 - 503" in {
    renderToHTML("\\\t\\A\\a\\ \\3\\φ\\«\n") shouldBe "<p>\\\t\\A\\a\\ \\3\\φ\\«</p>\n"
  }
  "14 - 509 - 529" in {
    renderToHTML("\\*not emphasized*\n\\<br/> not a tag\n\\[not a link](/foo)\n\\`not code`\n1\\. not a list\n\\* not a list\n\\# not a heading\n\\[foo]: /url \"not a reference\"\n\\&ouml; not a character entity\n") shouldBe "<p>*not emphasized*\n&lt;br/&gt; not a tag\n[not a link](/foo)\n`not code`\n1. not a list\n* not a list\n# not a heading\n[foo]: /url &quot;not a reference&quot;\n&amp;ouml; not a character entity</p>\n"
  }
  "15 - 534 - 538" in {
    renderToHTML("\\\\*emphasis*\n") shouldBe "<p>\\<em>emphasis</em></p>\n"
  }
  "16 - 543 - 549" in {
    renderToHTML("foo\\\nbar\n") shouldBe "<p>foo<br />\nbar</p>\n"
  }
  "17 - 555 - 559" in {
    renderToHTML("`` \\[\\` ``\n") shouldBe "<p><code>\\[\\`</code></p>\n"
  }
  "18 - 562 - 567" in {
    renderToHTML("    \\[\\]\n") shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }
  "19 - 570 - 577" in {
    renderToHTML("~~~\n\\[\\]\n~~~\n") shouldBe "<pre><code>\\[\\]\n</code></pre>\n"
  }
  "20 - 580 - 584" in {
    renderToHTML("<https://example.com?find=\\*>\n") shouldBe "<p><a href=\"https://example.com?find=%5C*\">https://example.com?find=\\*</a></p>\n"
  }
  "21 - 587 - 591" in {
    renderToHTML("<a href=\"/bar\\/)\">\n") shouldBe "<a href=\"/bar\\/)\">\n"
  }
  "22 - 597 - 601" in {
    renderToHTML("[foo](/bar\\* \"ti\\*tle\")\n") shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
  }
  "23 - 604 - 610" in {
    renderToHTML("[foo]\n\n[foo]: /bar\\* \"ti\\*tle\"\n") shouldBe "<p><a href=\"/bar*\" title=\"ti*tle\">foo</a></p>\n"
  }
  "24 - 613 - 620" in {
    renderToHTML("``` foo\\+bar\nfoo\n```\n") shouldBe "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n"
  }
