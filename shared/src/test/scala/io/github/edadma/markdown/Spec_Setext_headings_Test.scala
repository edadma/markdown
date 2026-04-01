package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Setext_headings_Test extends AnyFreeSpec with Matchers:
  "80 - 1347 - 1356" in {
    renderToHTML("Foo *bar*\n=========\n\nFoo *bar*\n---------\n") shouldBe "<h1>Foo <em>bar</em></h1>\n<h2>Foo <em>bar</em></h2>\n"
  }
  "81 - 1361 - 1368" in {
    renderToHTML("Foo *bar\nbaz*\n====\n") shouldBe "<h1>Foo <em>bar\nbaz</em></h1>\n"
  }
  "82 - 1375 - 1382" in {
    renderToHTML("  Foo *bar\nbaz*\t\n====\n") shouldBe "<h1>Foo <em>bar\nbaz</em></h1>\n"
  }
  "83 - 1387 - 1396" in {
    renderToHTML("Foo\n-------------------------\n\nFoo\n=\n") shouldBe "<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }
  "84 - 1402 - 1415" in {
    renderToHTML("   Foo\n---\n\n  Foo\n-----\n\n  Foo\n  ===\n") shouldBe "<h2>Foo</h2>\n<h2>Foo</h2>\n<h1>Foo</h1>\n"
  }
  "85 - 1420 - 1433" in {
    renderToHTML("    Foo\n    ---\n\n    Foo\n---\n") shouldBe "<pre><code>Foo\n---\n\nFoo\n</code></pre>\n<hr />\n"
  }
  "86 - 1439 - 1444" in {
    renderToHTML("Foo\n   ----      \n") shouldBe "<h2>Foo</h2>\n"
  }
  "87 - 1449 - 1455" in {
    renderToHTML("Foo\n    ---\n") shouldBe "<p>Foo\n---</p>\n"
  }
  "88 - 1460 - 1471" in {
    renderToHTML("Foo\n= =\n\nFoo\n--- -\n") shouldBe "<p>Foo\n= =</p>\n<p>Foo</p>\n<hr />\n"
  }
  "89 - 1476 - 1481" in {
    renderToHTML("Foo  \n-----\n") shouldBe "<h2>Foo</h2>\n"
  }
  "90 - 1486 - 1491" in {
    renderToHTML("Foo\\\n----\n") shouldBe "<h2>Foo\\</h2>\n"
  }
  "91 - 1497 - 1510" in {
    renderToHTML("`Foo\n----\n`\n\n<a title=\"a lot\n---\nof dashes\"/>\n") shouldBe "<h2>`Foo</h2>\n<p>`</p>\n<h2>&lt;a title=&quot;a lot</h2>\n<p>of dashes&quot;/&gt;</p>\n"
  }
  "92 - 1516 - 1524" in {
    renderToHTML("> Foo\n---\n") shouldBe "<blockquote>\n<p>Foo</p>\n</blockquote>\n<hr />\n"
  }
  "93 - 1527 - 1537" in {
    renderToHTML("> foo\nbar\n===\n") shouldBe "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n"
  }
  "94 - 1540 - 1548" in {
    renderToHTML("- Foo\n---\n") shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n"
  }
  "95 - 1555 - 1562" in {
    renderToHTML("Foo\nBar\n---\n") shouldBe "<h2>Foo\nBar</h2>\n"
  }
  "96 - 1568 - 1580" in {
    renderToHTML("---\nFoo\n---\nBar\n---\nBaz\n") shouldBe "<hr />\n<h2>Foo</h2>\n<h2>Bar</h2>\n<p>Baz</p>\n"
  }
  "97 - 1585 - 1590" in {
    renderToHTML("\n====\n") shouldBe "<p>====</p>\n"
  }
  "98 - 1597 - 1603" in {
    renderToHTML("---\n---\n") shouldBe "<hr />\n<hr />\n"
  }
  "99 - 1606 - 1614" in {
    renderToHTML("- foo\n-----\n") shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n"
  }
  "100 - 1617 - 1624" in {
    renderToHTML("    foo\n---\n") shouldBe "<pre><code>foo\n</code></pre>\n<hr />\n"
  }
  "101 - 1627 - 1635" in {
    renderToHTML("> foo\n-----\n") shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }
  "102 - 1641 - 1646" in {
    renderToHTML("\\> foo\n------\n") shouldBe "<h2>&gt; foo</h2>\n"
  }
  "103 - 1672 - 1682" in {
    renderToHTML("Foo\n\nbar\n---\nbaz\n") shouldBe "<p>Foo</p>\n<h2>bar</h2>\n<p>baz</p>\n"
  }
  "104 - 1688 - 1700" in {
    renderToHTML("Foo\nbar\n\n---\n\nbaz\n") shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }
  "105 - 1706 - 1716" in {
    renderToHTML("Foo\nbar\n* * *\nbaz\n") shouldBe "<p>Foo\nbar</p>\n<hr />\n<p>baz</p>\n"
  }
  "106 - 1721 - 1731" in {
    renderToHTML("Foo\nbar\n\\---\nbaz\n") shouldBe "<p>Foo\nbar\n---\nbaz</p>\n"
  }
