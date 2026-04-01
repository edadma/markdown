package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Block_quotes_Test extends AnyFreeSpec with Matchers:
  "228 - 3700 - 3710" in {
    renderToHTML("> # Foo\n> bar\n> baz\n") shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }
  "229 - 3715 - 3725" in {
    renderToHTML("># Foo\n>bar\n> baz\n") shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }
  "230 - 3730 - 3740" in {
    renderToHTML("   > # Foo\n   > bar\n > baz\n") shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }
  "231 - 3745 - 3754" in {
    renderToHTML("    > # Foo\n    > bar\n    > baz\n") shouldBe "<pre><code>&gt; # Foo\n&gt; bar\n&gt; baz\n</code></pre>\n"
  }
  "232 - 3760 - 3770" in {
    renderToHTML("> # Foo\n> bar\nbaz\n") shouldBe "<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }
  "233 - 3776 - 3786" in {
    renderToHTML("> bar\nbaz\n> foo\n") shouldBe "<blockquote>\n<p>bar\nbaz\nfoo</p>\n</blockquote>\n"
  }
  "234 - 3800 - 3808" in {
    renderToHTML("> foo\n---\n") shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<hr />\n"
  }
  "235 - 3820 - 3832" in {
    renderToHTML("> - foo\n- bar\n") shouldBe "<blockquote>\n<ul>\n<li>foo</li>\n</ul>\n</blockquote>\n<ul>\n<li>bar</li>\n</ul>\n"
  }
  "236 - 3838 - 3848" in {
    renderToHTML(">     foo\n    bar\n") shouldBe "<blockquote>\n<pre><code>foo\n</code></pre>\n</blockquote>\n<pre><code>bar\n</code></pre>\n"
  }
  "237 - 3851 - 3861" in {
    renderToHTML("> ```\nfoo\n```\n") shouldBe "<blockquote>\n<pre><code></code></pre>\n</blockquote>\n<p>foo</p>\n<pre><code></code></pre>\n"
  }
  "238 - 3867 - 3875" in {
    renderToHTML("> foo\n    - bar\n") shouldBe "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n"
  }
  "239 - 3891 - 3896" in {
    renderToHTML(">\n") shouldBe "<blockquote>\n</blockquote>\n"
  }
  "240 - 3899 - 3906" in {
    renderToHTML(">\n>  \n> \n") shouldBe "<blockquote>\n</blockquote>\n"
  }
  "241 - 3911 - 3919" in {
    renderToHTML(">\n> foo\n>  \n") shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n"
  }
  "242 - 3924 - 3935" in {
    renderToHTML("> foo\n\n> bar\n") shouldBe "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }
  "243 - 3946 - 3954" in {
    renderToHTML("> foo\n> bar\n") shouldBe "<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n"
  }
  "244 - 3959 - 3968" in {
    renderToHTML("> foo\n>\n> bar\n") shouldBe "<blockquote>\n<p>foo</p>\n<p>bar</p>\n</blockquote>\n"
  }
  "245 - 3973 - 3981" in {
    renderToHTML("foo\n> bar\n") shouldBe "<p>foo</p>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }
  "246 - 3987 - 3999" in {
    renderToHTML("> aaa\n***\n> bbb\n") shouldBe "<blockquote>\n<p>aaa</p>\n</blockquote>\n<hr />\n<blockquote>\n<p>bbb</p>\n</blockquote>\n"
  }
  "247 - 4005 - 4013" in {
    renderToHTML("> bar\nbaz\n") shouldBe "<blockquote>\n<p>bar\nbaz</p>\n</blockquote>\n"
  }
  "248 - 4016 - 4025" in {
    renderToHTML("> bar\n\nbaz\n") shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }
  "249 - 4028 - 4037" in {
    renderToHTML("> bar\n>\nbaz\n") shouldBe "<blockquote>\n<p>bar</p>\n</blockquote>\n<p>baz</p>\n"
  }
  "250 - 4044 - 4056" in {
    renderToHTML("> > > foo\nbar\n") shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }
  "251 - 4059 - 4073" in {
    renderToHTML(">>> foo\n> bar\n>>baz\n") shouldBe "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n"
  }
  "252 - 4081 - 4093" in {
    renderToHTML(">     code\n\n>    not code\n") shouldBe "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n"
  }
