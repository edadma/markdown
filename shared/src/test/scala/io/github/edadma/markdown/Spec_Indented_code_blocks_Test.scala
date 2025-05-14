package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Indented_code_blocks_Test extends AnyFreeSpec with Matchers:
  "107 - 1749 - 1756" in {
    renderToHTML("    a simple\n      indented code block\n") shouldBe "<pre><code>a simple\n  indented code block\n</code></pre>\n"
  }
  "108 - 1763 - 1774" in {
    renderToHTML("  - foo\n\n    bar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }
  "109 - 1777 - 1790" in {
    renderToHTML("1.  foo\n\n    - bar\n") shouldBe "<ol>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
  }
  "110 - 1797 - 1808" in {
    renderToHTML("    <a/>\n    *hi*\n\n    - one\n") shouldBe "<pre><code>&lt;a/&gt;\n*hi*\n\n- one\n</code></pre>\n"
  }
  "111 - 1813 - 1830" in {
    renderToHTML("    chunk1\n\n    chunk2\n  \n \n \n    chunk3\n") shouldBe "<pre><code>chunk1\n\nchunk2\n\n\n\nchunk3\n</code></pre>\n"
  }
  "112 - 1836 - 1845" in {
    renderToHTML("    chunk1\n      \n      chunk2\n") shouldBe "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n"
  }
  "113 - 1851 - 1858" in {
    renderToHTML("Foo\n    bar\n\n") shouldBe "<p>Foo\nbar</p>\n"
  }
  "114 - 1865 - 1872" in {
    renderToHTML("    foo\nbar\n") shouldBe "<pre><code>foo\n</code></pre>\n<p>bar</p>\n"
  }
  "115 - 1878 - 1893" in {
    renderToHTML("# Heading\n    foo\nHeading\n------\n    foo\n----\n") shouldBe "<h1>Heading</h1>\n<pre><code>foo\n</code></pre>\n<h2>Heading</h2>\n<pre><code>foo\n</code></pre>\n<hr />\n"
  }
  "116 - 1898 - 1905" in {
    renderToHTML("        foo\n    bar\n") shouldBe "<pre><code>    foo\nbar\n</code></pre>\n"
  }
  "117 - 1911 - 1920" in {
    renderToHTML("\n    \n    foo\n    \n\n") shouldBe "<pre><code>foo\n</code></pre>\n"
  }
  "118 - 1925 - 1930" in {
    renderToHTML("    foo  \n") shouldBe "<pre><code>foo  \n</code></pre>\n"
  }
