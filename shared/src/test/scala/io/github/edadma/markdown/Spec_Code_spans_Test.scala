package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Code_spans_Test extends AnyFreeSpec with Matchers:
  "328 - 5886 - 5890" in {
    renderToHTML("`foo`\n") shouldBe "<p><code>foo</code></p>\n"
  }
  "329 - 5897 - 5901" in {
    renderToHTML("`` foo ` bar ``\n") shouldBe "<p><code>foo ` bar</code></p>\n"
  }
  "330 - 5907 - 5911" in {
    renderToHTML("` `` `\n") shouldBe "<p><code>``</code></p>\n"
  }
  "331 - 5915 - 5919" in {
    renderToHTML("`  ``  `\n") shouldBe "<p><code> `` </code></p>\n"
  }
  "332 - 5924 - 5928" in {
    renderToHTML("` a`\n") shouldBe "<p><code> a</code></p>\n"
  }
  "333 - 5933 - 5937" in {
    renderToHTML("` b `\n") shouldBe "<p><code> b </code></p>\n"
  }
  "334 - 5941 - 5947" in {
    renderToHTML("` `\n`  `\n") shouldBe "<p><code> </code>\n<code>  </code></p>\n"
  }
  "335 - 5952 - 5960" in {
    renderToHTML("``\nfoo\nbar  \nbaz\n``\n") shouldBe "<p><code>foo bar   baz</code></p>\n"
  }
  "336 - 5962 - 5968" in {
    renderToHTML("``\nfoo \n``\n") shouldBe "<p><code>foo </code></p>\n"
  }
  "337 - 5973 - 5978" in {
    renderToHTML("`foo   bar \nbaz`\n") shouldBe "<p><code>foo   bar  baz</code></p>\n"
  }
  "338 - 5990 - 5994" in {
    renderToHTML("`foo\\`bar`\n") shouldBe "<p><code>foo\\</code>bar`</p>\n"
  }
  "339 - 6001 - 6005" in {
    renderToHTML("``foo`bar``\n") shouldBe "<p><code>foo`bar</code></p>\n"
  }
  "340 - 6007 - 6011" in {
    renderToHTML("` foo `` bar `\n") shouldBe "<p><code>foo `` bar</code></p>\n"
  }
  "341 - 6019 - 6023" in {
    renderToHTML("*foo`*`\n") shouldBe "<p>*foo<code>*</code></p>\n"
  }
  "342 - 6028 - 6032" in {
    renderToHTML("[not a `link](/foo`)\n") shouldBe "<p>[not a <code>link](/foo</code>)</p>\n"
  }
  "343 - 6038 - 6042" in {
    renderToHTML("`<a href=\"`\">`\n") shouldBe "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>\n"
  }
  "344 - 6047 - 6051" in {
    renderToHTML("<a href=\"`\">`\n") shouldBe "<p><a href=\"`\">`</p>\n"
  }
  "345 - 6056 - 6060" in {
    renderToHTML("`<https://foo.bar.`baz>`\n") shouldBe "<p><code>&lt;https://foo.bar.</code>baz&gt;`</p>\n"
  }
  "346 - 6065 - 6069" in {
    renderToHTML("<https://foo.bar.`baz>`\n") shouldBe "<p><a href=\"https://foo.bar.%60baz\">https://foo.bar.`baz</a>`</p>\n"
  }
  "347 - 6075 - 6079" in {
    renderToHTML("```foo``\n") shouldBe "<p>```foo``</p>\n"
  }
  "348 - 6082 - 6086" in {
    renderToHTML("`foo\n") shouldBe "<p>`foo</p>\n"
  }
  "349 - 6091 - 6095" in {
    renderToHTML("`foo``bar``\n") shouldBe "<p>`foo<code>bar</code></p>\n"
  }
