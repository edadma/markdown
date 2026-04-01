package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_List_items_Test extends AnyFreeSpec with Matchers:
  "253 - 4135 - 4150" in {
    renderToHTML("A paragraph\nwith two lines.\n\n    indented code\n\n> A block quote.\n") shouldBe "<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n"
  }
  "254 - 4157 - 4176" in {
    renderToHTML("1.  A paragraph\n    with two lines.\n\n        indented code\n\n    > A block quote.\n") shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "255 - 4190 - 4199" in {
    renderToHTML("- one\n\n two\n") shouldBe "<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n"
  }
  "256 - 4202 - 4213" in {
    renderToHTML("- one\n\n  two\n") shouldBe "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
  }
  "257 - 4216 - 4226" in {
    renderToHTML(" -    one\n\n     two\n") shouldBe "<ul>\n<li>one</li>\n</ul>\n<pre><code> two\n</code></pre>\n"
  }
  "258 - 4229 - 4240" in {
    renderToHTML(" -    one\n\n      two\n") shouldBe "<ul>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ul>\n"
  }
  "259 - 4251 - 4266" in {
    renderToHTML("   > > 1.  one\n>>\n>>     two\n") shouldBe "<blockquote>\n<blockquote>\n<ol>\n<li>\n<p>one</p>\n<p>two</p>\n</li>\n</ol>\n</blockquote>\n</blockquote>\n"
  }
  "260 - 4278 - 4291" in {
    renderToHTML(">>- one\n>>\n  >  > two\n") shouldBe "<blockquote>\n<blockquote>\n<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n</blockquote>\n</blockquote>\n"
  }
  "261 - 4297 - 4304" in {
    renderToHTML("-one\n\n2.two\n") shouldBe "<p>-one</p>\n<p>2.two</p>\n"
  }
  "262 - 4310 - 4322" in {
    renderToHTML("- foo\n\n\n  bar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }
  "263 - 4327 - 4349" in {
    renderToHTML("1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam\n") shouldBe "<ol>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n<blockquote>\n<p>bam</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "264 - 4355 - 4373" in {
    renderToHTML("- Foo\n\n      bar\n\n\n      baz\n") shouldBe "<ul>\n<li>\n<p>Foo</p>\n<pre><code>bar\n\n\nbaz\n</code></pre>\n</li>\n</ul>\n"
  }
  "265 - 4377 - 4383" in {
    renderToHTML("123456789. ok\n") shouldBe "<ol start=\"123456789\">\n<li>ok</li>\n</ol>\n"
  }
  "266 - 4386 - 4390" in {
    renderToHTML("1234567890. not ok\n") shouldBe "<p>1234567890. not ok</p>\n"
  }
  "267 - 4395 - 4401" in {
    renderToHTML("0. ok\n") shouldBe "<ol start=\"0\">\n<li>ok</li>\n</ol>\n"
  }
  "268 - 4404 - 4410" in {
    renderToHTML("003. ok\n") shouldBe "<ol start=\"3\">\n<li>ok</li>\n</ol>\n"
  }
  "269 - 4415 - 4419" in {
    renderToHTML("-1. not ok\n") shouldBe "<p>-1. not ok</p>\n"
  }
  "270 - 4438 - 4450" in {
    renderToHTML("- foo\n\n      bar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ul>\n"
  }
  "271 - 4455 - 4467" in {
    renderToHTML("  10.  foo\n\n           bar\n") shouldBe "<ol start=\"10\">\n<li>\n<p>foo</p>\n<pre><code>bar\n</code></pre>\n</li>\n</ol>\n"
  }
  "272 - 4474 - 4486" in {
    renderToHTML("    indented code\n\nparagraph\n\n    more code\n") shouldBe "<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n"
  }
  "273 - 4489 - 4505" in {
    renderToHTML("1.     indented code\n\n   paragraph\n\n       more code\n") shouldBe "<ol>\n<li>\n<pre><code>indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
  }
  "274 - 4511 - 4527" in {
    renderToHTML("1.      indented code\n\n   paragraph\n\n       more code\n") shouldBe "<ol>\n<li>\n<pre><code> indented code\n</code></pre>\n<p>paragraph</p>\n<pre><code>more code\n</code></pre>\n</li>\n</ol>\n"
  }
  "275 - 4538 - 4545" in {
    renderToHTML("   foo\n\nbar\n") shouldBe "<p>foo</p>\n<p>bar</p>\n"
  }
  "276 - 4548 - 4557" in {
    renderToHTML("-    foo\n\n  bar\n") shouldBe "<ul>\n<li>foo</li>\n</ul>\n<p>bar</p>\n"
  }
  "277 - 4565 - 4576" in {
    renderToHTML("-  foo\n\n   bar\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n"
  }
  "278 - 4592 - 4613" in {
    renderToHTML("-\n  foo\n-\n  ```\n  bar\n  ```\n-\n      baz\n") shouldBe "<ul>\n<li>foo</li>\n<li>\n<pre><code>bar\n</code></pre>\n</li>\n<li>\n<pre><code>baz\n</code></pre>\n</li>\n</ul>\n"
  }
  "279 - 4618 - 4625" in {
    renderToHTML("-   \n  foo\n") shouldBe "<ul>\n<li>foo</li>\n</ul>\n"
  }
  "280 - 4632 - 4641" in {
    renderToHTML("-\n\n  foo\n") shouldBe "<ul>\n<li></li>\n</ul>\n<p>foo</p>\n"
  }
  "281 - 4646 - 4656" in {
    renderToHTML("- foo\n-\n- bar\n") shouldBe "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
  }
  "282 - 4661 - 4671" in {
    renderToHTML("- foo\n-   \n- bar\n") shouldBe "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
  }
  "283 - 4676 - 4686" in {
    renderToHTML("1. foo\n2.\n3. bar\n") shouldBe "<ol>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ol>\n"
  }
  "284 - 4691 - 4697" in {
    renderToHTML("*\n") shouldBe "<ul>\n<li></li>\n</ul>\n"
  }
  "285 - 4701 - 4712" in {
    renderToHTML("foo\n*\n\nfoo\n1.\n") shouldBe "<p>foo\n*</p>\n<p>foo\n1.</p>\n"
  }
  "286 - 4723 - 4742" in {
    renderToHTML(" 1.  A paragraph\n     with two lines.\n\n         indented code\n\n     > A block quote.\n") shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "287 - 4747 - 4766" in {
    renderToHTML("  1.  A paragraph\n      with two lines.\n\n          indented code\n\n      > A block quote.\n") shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "288 - 4771 - 4790" in {
    renderToHTML("   1.  A paragraph\n       with two lines.\n\n           indented code\n\n       > A block quote.\n") shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "289 - 4795 - 4810" in {
    renderToHTML("    1.  A paragraph\n        with two lines.\n\n            indented code\n\n        > A block quote.\n") shouldBe "<pre><code>1.  A paragraph\n    with two lines.\n\n        indented code\n\n    &gt; A block quote.\n</code></pre>\n"
  }
  "290 - 4825 - 4844" in {
    renderToHTML("  1.  A paragraph\nwith two lines.\n\n          indented code\n\n      > A block quote.\n") shouldBe "<ol>\n<li>\n<p>A paragraph\nwith two lines.</p>\n<pre><code>indented code\n</code></pre>\n<blockquote>\n<p>A block quote.</p>\n</blockquote>\n</li>\n</ol>\n"
  }
  "291 - 4849 - 4857" in {
    renderToHTML("  1.  A paragraph\n    with two lines.\n") shouldBe "<ol>\n<li>A paragraph\nwith two lines.</li>\n</ol>\n"
  }
  "292 - 4862 - 4876" in {
    renderToHTML("> 1. > Blockquote\ncontinued here.\n") shouldBe "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
  }
  "293 - 4879 - 4893" in {
    renderToHTML("> 1. > Blockquote\n> continued here.\n") shouldBe "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n"
  }
  "294 - 4907 - 4928" in {
    renderToHTML("- foo\n  - bar\n    - baz\n      - boo\n") shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz\n<ul>\n<li>boo</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }
  "295 - 4933 - 4945" in {
    renderToHTML("- foo\n - bar\n  - baz\n   - boo\n") shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n<li>boo</li>\n</ul>\n"
  }
  "296 - 4950 - 4961" in {
    renderToHTML("10) foo\n    - bar\n") shouldBe "<ol start=\"10\">\n<li>foo\n<ul>\n<li>bar</li>\n</ul>\n</li>\n</ol>\n"
  }
  "297 - 4966 - 4976" in {
    renderToHTML("10) foo\n   - bar\n") shouldBe "<ol start=\"10\">\n<li>foo</li>\n</ol>\n<ul>\n<li>bar</li>\n</ul>\n"
  }
  "298 - 4981 - 4991" in {
    renderToHTML("- - foo\n") shouldBe "<ul>\n<li>\n<ul>\n<li>foo</li>\n</ul>\n</li>\n</ul>\n"
  }
  "299 - 4994 - 5008" in {
    renderToHTML("1. - 2. foo\n") shouldBe "<ol>\n<li>\n<ul>\n<li>\n<ol start=\"2\">\n<li>foo</li>\n</ol>\n</li>\n</ul>\n</li>\n</ol>\n"
  }
  "300 - 5013 - 5027" in {
    renderToHTML("- # Foo\n- Bar\n  ---\n  baz\n") shouldBe "<ul>\n<li>\n<h1>Foo</h1>\n</li>\n<li>\n<h2>Bar</h2>\nbaz</li>\n</ul>\n"
  }
