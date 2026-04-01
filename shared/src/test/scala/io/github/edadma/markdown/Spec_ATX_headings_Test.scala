package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_ATX_headings_Test extends AnyFreeSpec with Matchers:
  "62 - 1112 - 1126" in {
    renderToHTML("# foo\n## foo\n### foo\n#### foo\n##### foo\n###### foo\n") shouldBe "<h1>foo</h1>\n<h2>foo</h2>\n<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n"
  }
  "63 - 1131 - 1135" in {
    renderToHTML("####### foo\n") shouldBe "<p>####### foo</p>\n"
  }
  "64 - 1146 - 1153" in {
    renderToHTML("#5 bolt\n\n#hashtag\n") shouldBe "<p>#5 bolt</p>\n<p>#hashtag</p>\n"
  }
  "65 - 1158 - 1162" in {
    renderToHTML("\\## foo\n") shouldBe "<p>## foo</p>\n"
  }
  "66 - 1167 - 1171" in {
    renderToHTML("# foo *bar* \\*baz\\*\n") shouldBe "<h1>foo <em>bar</em> *baz*</h1>\n"
  }
  "67 - 1176 - 1180" in {
    renderToHTML("#                  foo                     \n") shouldBe "<h1>foo</h1>\n"
  }
  "68 - 1185 - 1193" in {
    renderToHTML(" ### foo\n  ## foo\n   # foo\n") shouldBe "<h3>foo</h3>\n<h2>foo</h2>\n<h1>foo</h1>\n"
  }
  "69 - 1198 - 1203" in {
    renderToHTML("    # foo\n") shouldBe "<pre><code># foo\n</code></pre>\n"
  }
  "70 - 1206 - 1212" in {
    renderToHTML("foo\n    # bar\n") shouldBe "<p>foo\n# bar</p>\n"
  }
  "71 - 1217 - 1223" in {
    renderToHTML("## foo ##\n  ###   bar    ###\n") shouldBe "<h2>foo</h2>\n<h3>bar</h3>\n"
  }
  "72 - 1228 - 1234" in {
    renderToHTML("# foo ##################################\n##### foo ##\n") shouldBe "<h1>foo</h1>\n<h5>foo</h5>\n"
  }
  "73 - 1239 - 1243" in {
    renderToHTML("### foo ###     \n") shouldBe "<h3>foo</h3>\n"
  }
  "74 - 1250 - 1254" in {
    renderToHTML("### foo ### b\n") shouldBe "<h3>foo ### b</h3>\n"
  }
  "75 - 1259 - 1263" in {
    renderToHTML("# foo#\n") shouldBe "<h1>foo#</h1>\n"
  }
  "76 - 1269 - 1277" in {
    renderToHTML("### foo \\###\n## foo #\\##\n# foo \\#\n") shouldBe "<h3>foo ###</h3>\n<h2>foo ###</h2>\n<h1>foo #</h1>\n"
  }
  "77 - 1283 - 1291" in {
    renderToHTML("****\n## foo\n****\n") shouldBe "<hr />\n<h2>foo</h2>\n<hr />\n"
  }
  "78 - 1294 - 1302" in {
    renderToHTML("Foo bar\n# baz\nBar foo\n") shouldBe "<p>Foo bar</p>\n<h1>baz</h1>\n<p>Bar foo</p>\n"
  }
  "79 - 1307 - 1315" in {
    renderToHTML("## \n#\n### ###\n") shouldBe "<h2></h2>\n<h1></h1>\n<h3></h3>\n"
  }
