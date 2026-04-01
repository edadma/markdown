package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Link_reference_definitions_Test extends AnyFreeSpec with Matchers:
  "192 - 3179 - 3185" in {
    renderToHTML("[foo]: /url \"title\"\n\n[foo]\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
  "193 - 3188 - 3196" in {
    renderToHTML("   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n") shouldBe "<p><a href=\"/url\" title=\"the title\">foo</a></p>\n"
  }
  "194 - 3199 - 3205" in {
    renderToHTML("[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n") shouldBe "<p><a href=\"my_(url)\" title=\"title (with parens)\">Foo*bar]</a></p>\n"
  }
  "195 - 3208 - 3216" in {
    renderToHTML("[Foo bar]:\n<my url>\n'title'\n\n[Foo bar]\n") shouldBe "<p><a href=\"my%20url\" title=\"title\">Foo bar</a></p>\n"
  }
  "196 - 3221 - 3235" in {
    renderToHTML("[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n") shouldBe "<p><a href=\"/url\" title=\"\ntitle\nline1\nline2\n\">foo</a></p>\n"
  }
  "197 - 3240 - 3250" in {
    renderToHTML("[foo]: /url 'title\n\nwith blank line'\n\n[foo]\n") shouldBe "<p>[foo]: /url 'title</p>\n<p>with blank line'</p>\n<p>[foo]</p>\n"
  }
  "198 - 3255 - 3262" in {
    renderToHTML("[foo]:\n/url\n\n[foo]\n") shouldBe "<p><a href=\"/url\">foo</a></p>\n"
  }
  "199 - 3267 - 3274" in {
    renderToHTML("[foo]:\n\n[foo]\n") shouldBe "<p>[foo]:</p>\n<p>[foo]</p>\n"
  }
  "200 - 3279 - 3285" in {
    renderToHTML("[foo]: <>\n\n[foo]\n") shouldBe "<p><a href=\"\">foo</a></p>\n"
  }
  "201 - 3290 - 3297" in {
    renderToHTML("[foo]: <bar>(baz)\n\n[foo]\n") shouldBe "<p>[foo]: <bar>(baz)</p>\n<p>[foo]</p>\n"
  }
  "202 - 3303 - 3309" in {
    renderToHTML("[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n") shouldBe "<p><a href=\"/url%5Cbar*baz\" title=\"foo&quot;bar\\baz\">foo</a></p>\n"
  }
  "203 - 3314 - 3320" in {
    renderToHTML("[foo]\n\n[foo]: url\n") shouldBe "<p><a href=\"url\">foo</a></p>\n"
  }
  "204 - 3326 - 3333" in {
    renderToHTML("[foo]\n\n[foo]: first\n[foo]: second\n") shouldBe "<p><a href=\"first\">foo</a></p>\n"
  }
  "205 - 3339 - 3345" in {
    renderToHTML("[FOO]: /url\n\n[Foo]\n") shouldBe "<p><a href=\"/url\">Foo</a></p>\n"
  }
  "206 - 3348 - 3354" in {
    renderToHTML("[ΑΓΩ]: /φου\n\n[αγω]\n") shouldBe "<p><a href=\"/%CF%86%CE%BF%CF%85\">αγω</a></p>\n"
  }
  "207 - 3363 - 3366" in {
    renderToHTML("[foo]: /url\n") shouldBe ""
  }
  "208 - 3371 - 3378" in {
    renderToHTML("[\nfoo\n]: /url\nbar\n") shouldBe "<p>bar</p>\n"
  }
  "209 - 3384 - 3388" in {
    renderToHTML("[foo]: /url \"title\" ok\n") shouldBe "<p>[foo]: /url &quot;title&quot; ok</p>\n"
  }
  "210 - 3393 - 3398" in {
    renderToHTML("[foo]: /url\n\"title\" ok\n") shouldBe "<p>&quot;title&quot; ok</p>\n"
  }
  "211 - 3404 - 3412" in {
    renderToHTML("    [foo]: /url \"title\"\n\n[foo]\n") shouldBe "<pre><code>[foo]: /url &quot;title&quot;\n</code></pre>\n<p>[foo]</p>\n"
  }
  "212 - 3418 - 3428" in {
    renderToHTML("```\n[foo]: /url\n```\n\n[foo]\n") shouldBe "<pre><code>[foo]: /url\n</code></pre>\n<p>[foo]</p>\n"
  }
  "213 - 3433 - 3442" in {
    renderToHTML("Foo\n[bar]: /baz\n\n[bar]\n") shouldBe "<p>Foo\n[bar]: /baz</p>\n<p>[bar]</p>\n"
  }
  "214 - 3448 - 3457" in {
    renderToHTML("# [Foo]\n[foo]: /url\n> bar\n") shouldBe "<h1><a href=\"/url\">Foo</a></h1>\n<blockquote>\n<p>bar</p>\n</blockquote>\n"
  }
  "215 - 3459 - 3467" in {
    renderToHTML("[foo]: /url\nbar\n===\n[foo]\n") shouldBe "<h1>bar</h1>\n<p><a href=\"/url\">foo</a></p>\n"
  }
  "216 - 3469 - 3476" in {
    renderToHTML("[foo]: /url\n===\n[foo]\n") shouldBe "<p>===\n<a href=\"/url\">foo</a></p>\n"
  }
  "217 - 3482 - 3495" in {
    renderToHTML("[foo]: /foo-url \"foo\"\n[bar]: /bar-url\n  \"bar\"\n[baz]: /baz-url\n\n[foo],\n[bar],\n[baz]\n") shouldBe "<p><a href=\"/foo-url\" title=\"foo\">foo</a>,\n<a href=\"/bar-url\" title=\"bar\">bar</a>,\n<a href=\"/baz-url\">baz</a></p>\n"
  }
  "218 - 3503 - 3511" in {
    renderToHTML("[foo]\n\n> [foo]: /url\n") shouldBe "<p><a href=\"/url\">foo</a></p>\n<blockquote>\n</blockquote>\n"
  }
