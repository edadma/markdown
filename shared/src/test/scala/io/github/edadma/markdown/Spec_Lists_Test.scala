package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Lists_Test extends AnyFreeSpec with Matchers:
  "301 - 5249 - 5261" in {
    renderToHTML("- foo\n- bar\n+ baz\n") shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<ul>\n<li>baz</li>\n</ul>\n"
  }
  "302 - 5264 - 5276" in {
    renderToHTML("1. foo\n2. bar\n3) baz\n") shouldBe "<ol>\n<li>foo</li>\n<li>bar</li>\n</ol>\n<ol start=\"3\">\n<li>baz</li>\n</ol>\n"
  }
  "303 - 5283 - 5293" in {
    renderToHTML("Foo\n- bar\n- baz\n") shouldBe "<p>Foo</p>\n<ul>\n<li>bar</li>\n<li>baz</li>\n</ul>\n"
  }
  "304 - 5360 - 5366" in {
    renderToHTML("The number of windows in my house is\n14.  The number of doors is 6.\n") shouldBe "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>\n"
  }
  "305 - 5370 - 5378" in {
    renderToHTML("The number of windows in my house is\n1.  The number of doors is 6.\n") shouldBe "<p>The number of windows in my house is</p>\n<ol>\n<li>The number of doors is 6.</li>\n</ol>\n"
  }
  "306 - 5384 - 5403" in {
    renderToHTML("- foo\n\n- bar\n\n\n- baz\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n<li>\n<p>baz</p>\n</li>\n</ul>\n"
  }
  "307 - 5405 - 5427" in {
    renderToHTML("- foo\n  - bar\n    - baz\n\n\n      bim\n") shouldBe "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n"
  }
  "308 - 5435 - 5453" in {
    renderToHTML("- foo\n- bar\n\n<!-- -->\n\n- baz\n- bim\n") shouldBe "<ul>\n<li>foo</li>\n<li>bar</li>\n</ul>\n<!-- -->\n<ul>\n<li>baz</li>\n<li>bim</li>\n</ul>\n"
  }
  "309 - 5456 - 5479" in {
    renderToHTML("-   foo\n\n    notcode\n\n-   foo\n\n<!-- -->\n\n    code\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<p>notcode</p>\n</li>\n<li>\n<p>foo</p>\n</li>\n</ul>\n<!-- -->\n<pre><code>code\n</code></pre>\n"
  }
  "310 - 5487 - 5505" in {
    renderToHTML("- a\n - b\n  - c\n   - d\n  - e\n - f\n- g\n") shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n<li>e</li>\n<li>f</li>\n<li>g</li>\n</ul>\n"
  }
  "311 - 5508 - 5526" in {
    renderToHTML("1. a\n\n  2. b\n\n   3. c\n") shouldBe "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ol>\n"
  }
  "312 - 5532 - 5546" in {
    renderToHTML("- a\n - b\n  - c\n   - d\n    - e\n") shouldBe "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d\n- e</li>\n</ul>\n"
  }
  "313 - 5552 - 5569" in {
    renderToHTML("1. a\n\n  2. b\n\n    3. c\n") shouldBe "<ol>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ol>\n<pre><code>3. c\n</code></pre>\n"
  }
  "314 - 5575 - 5592" in {
    renderToHTML("- a\n- b\n\n- c\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  }
  "315 - 5597 - 5612" in {
    renderToHTML("* a\n*\n\n* c\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li></li>\n<li>\n<p>c</p>\n</li>\n</ul>\n"
  }
  "316 - 5619 - 5638" in {
    renderToHTML("- a\n- b\n\n  c\n- d\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }
  "317 - 5641 - 5659" in {
    renderToHTML("- a\n- b\n\n  [ref]: /url\n- d\n") shouldBe "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n<li>\n<p>d</p>\n</li>\n</ul>\n"
  }
  "318 - 5664 - 5683" in {
    renderToHTML("- a\n- ```\n  b\n\n\n  ```\n- c\n") shouldBe "<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>\n"
  }
  "319 - 5690 - 5708" in {
    renderToHTML("- a\n  - b\n\n    c\n- d\n") shouldBe "<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>\n"
  }
  "320 - 5714 - 5728" in {
    renderToHTML("* a\n  > b\n  >\n* c\n") shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n</li>\n<li>c</li>\n</ul>\n"
  }
  "321 - 5734 - 5752" in {
    renderToHTML("- a\n  > b\n  ```\n  c\n  ```\n- d\n") shouldBe "<ul>\n<li>a\n<blockquote>\n<p>b</p>\n</blockquote>\n<pre><code>c\n</code></pre>\n</li>\n<li>d</li>\n</ul>\n"
  }
  "322 - 5757 - 5763" in {
    renderToHTML("- a\n") shouldBe "<ul>\n<li>a</li>\n</ul>\n"
  }
  "323 - 5766 - 5777" in {
    renderToHTML("- a\n  - b\n") shouldBe "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>\n"
  }
  "324 - 5783 - 5797" in {
    renderToHTML("1. ```\n   foo\n   ```\n\n   bar\n") shouldBe "<ol>\n<li>\n<pre><code>foo\n</code></pre>\n<p>bar</p>\n</li>\n</ol>\n"
  }
  "325 - 5802 - 5817" in {
    renderToHTML("* foo\n  * bar\n\n  baz\n") shouldBe "<ul>\n<li>\n<p>foo</p>\n<ul>\n<li>bar</li>\n</ul>\n<p>baz</p>\n</li>\n</ul>\n"
  }
  "326 - 5820 - 5845" in {
    renderToHTML("- a\n  - b\n  - c\n\n- d\n  - e\n  - f\n") shouldBe "<ul>\n<li>\n<p>a</p>\n<ul>\n<li>b</li>\n<li>c</li>\n</ul>\n</li>\n<li>\n<p>d</p>\n<ul>\n<li>e</li>\n<li>f</li>\n</ul>\n</li>\n</ul>\n"
  }
