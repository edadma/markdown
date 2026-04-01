package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Tabs_Test extends AnyFreeSpec with Matchers:
  "1 - 355 - 360" in {
    renderToHTML("\tfoo\tbaz\t\tbim\n") shouldBe "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
  }
  "2 - 362 - 367" in {
    renderToHTML("  \tfoo\tbaz\t\tbim\n") shouldBe "<pre><code>foo\tbaz\t\tbim\n</code></pre>\n"
  }
  "3 - 369 - 376" in {
    renderToHTML("    a\ta\n    ὐ\ta\n") shouldBe "<pre><code>a\ta\nὐ\ta\n</code></pre>\n"
  }
  "4 - 382 - 393" in {
    renderToHTML("  - foo\n\n\tbar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }
  "5 - 395 - 407" in {
    renderToHTML("- foo\n\n\t\tbar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<pre><code>  bar\n</code></pre>\n</li>\n</ul>\n"
  }
  "6 - 418 - 425" in {
    renderToHTML(">\t\tfoo\n") shouldBe "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n"
  }
  "7 - 427 - 436" in {
    renderToHTML("-\t\tfoo\n") shouldBe "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n"
  }
  "8 - 439 - 446" in {
    renderToHTML("    foo\n\tbar\n") shouldBe "<pre><code>foo\nbar\n</code></pre>\n"
  }
  "9 - 448 - 464" in {
    renderToHTML(" - foo\n   - bar\n\t - baz\n") shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }
  "10 - 466 - 470" in {
    renderToHTML("#\tFoo\n") shouldBe "<h1>Foo</h1>\n"
  }
  "11 - 472 - 476" in {
    renderToHTML("*\t*\t*\t\n") shouldBe "<hr />\n"
  }
