package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Fenced_code_blocks_Test extends AnyFreeSpec with Matchers:
  "119 - 1980 - 1989" in {
    renderToHTML("```\n<\n >\n```\n") shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }
  "120 - 1994 - 2003" in {
    renderToHTML("~~~\n<\n >\n~~~\n") shouldBe "<pre><code>&lt;\n &gt;\n</code></pre>\n"
  }
  "121 - 2007 - 2013" in {
    renderToHTML("``\nfoo\n``\n") shouldBe "<p><code>foo</code></p>\n"
  }
  "122 - 2018 - 2027" in {
    renderToHTML("```\naaa\n~~~\n```\n") shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }
  "123 - 2030 - 2039" in {
    renderToHTML("~~~\naaa\n```\n~~~\n") shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }
  "124 - 2044 - 2053" in {
    renderToHTML("````\naaa\n```\n``````\n") shouldBe "<pre><code>aaa\n```\n</code></pre>\n"
  }
  "125 - 2056 - 2065" in {
    renderToHTML("~~~~\naaa\n~~~\n~~~~\n") shouldBe "<pre><code>aaa\n~~~\n</code></pre>\n"
  }
  "126 - 2071 - 2075" in {
    renderToHTML("```\n") shouldBe "<pre><code></code></pre>\n"
  }
  "127 - 2078 - 2088" in {
    renderToHTML("`````\n\n```\naaa\n") shouldBe "<pre><code>\n```\naaa\n</code></pre>\n"
  }
  "128 - 2091 - 2102" in {
    renderToHTML("> ```\n> aaa\n\nbbb\n") shouldBe "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n"
  }
  "129 - 2107 - 2116" in {
    renderToHTML("```\n\n  \n```\n") shouldBe "<pre><code>\n  \n</code></pre>\n"
  }
  "130 - 2121 - 2126" in {
    renderToHTML("```\n```\n") shouldBe "<pre><code></code></pre>\n"
  }
  "131 - 2133 - 2142" in {
    renderToHTML(" ```\n aaa\naaa\n```\n") shouldBe "<pre><code>aaa\naaa\n</code></pre>\n"
  }
  "132 - 2145 - 2156" in {
    renderToHTML("  ```\naaa\n  aaa\naaa\n  ```\n") shouldBe "<pre><code>aaa\naaa\naaa\n</code></pre>\n"
  }
  "133 - 2159 - 2170" in {
    renderToHTML("   ```\n   aaa\n    aaa\n  aaa\n   ```\n") shouldBe "<pre><code>aaa\n aaa\naaa\n</code></pre>\n"
  }
  "134 - 2175 - 2184" in {
    renderToHTML("    ```\n    aaa\n    ```\n") shouldBe "<pre><code>```\naaa\n```\n</code></pre>\n"
  }
  "135 - 2190 - 2197" in {
    renderToHTML("```\naaa\n  ```\n") shouldBe "<pre><code>aaa\n</code></pre>\n"
  }
  "136 - 2200 - 2207" in {
    renderToHTML("   ```\naaa\n  ```\n") shouldBe "<pre><code>aaa\n</code></pre>\n"
  }
  "137 - 2212 - 2220" in {
    renderToHTML("```\naaa\n    ```\n") shouldBe "<pre><code>aaa\n    ```\n</code></pre>\n"
  }
  "138 - 2226 - 2232" in {
    renderToHTML("``` ```\naaa\n") shouldBe "<p><code> </code>\naaa</p>\n"
  }
  "139 - 2235 - 2243" in {
    renderToHTML("~~~~~~\naaa\n~~~ ~~\n") shouldBe "<pre><code>aaa\n~~~ ~~\n</code></pre>\n"
  }
  "140 - 2249 - 2260" in {
    renderToHTML("foo\n```\nbar\n```\nbaz\n") shouldBe "<p>foo</p>\n<pre><code>bar\n</code></pre>\n<p>baz</p>\n"
  }
  "141 - 2266 - 2278" in {
    renderToHTML("foo\n---\n~~~\nbar\n~~~\n# baz\n") shouldBe "<h2>foo</h2>\n<pre><code>bar\n</code></pre>\n<h1>baz</h1>\n"
  }
  "142 - 2288 - 2299" in {
    renderToHTML("```ruby\ndef foo(x)\n  return 3\nend\n```\n") shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }
  "143 - 2302 - 2313" in {
    renderToHTML("~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~\n") shouldBe "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
  }
  "144 - 2316 - 2321" in {
    renderToHTML("````;\n````\n") shouldBe "<pre><code class=\"language-;\"></code></pre>\n"
  }
  "145 - 2326 - 2332" in {
    renderToHTML("``` aa ```\nfoo\n") shouldBe "<p><code>aa</code>\nfoo</p>\n"
  }
  "146 - 2337 - 2344" in {
    renderToHTML("~~~ aa ``` ~~~\nfoo\n~~~\n") shouldBe "<pre><code class=\"language-aa\">foo\n</code></pre>\n"
  }
  "147 - 2349 - 2356" in {
    renderToHTML("```\n``` aaa\n```\n") shouldBe "<pre><code>``` aaa\n</code></pre>\n"
  }
