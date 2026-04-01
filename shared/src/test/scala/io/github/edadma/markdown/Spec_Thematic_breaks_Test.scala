package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Thematic_breaks_Test extends AnyFreeSpec with Matchers:
  "43 - 879 - 887" in {
    renderToHTML("***\n---\n___\n") shouldBe "<hr />\n<hr />\n<hr />\n"
  }
  "44 - 892 - 896" in {
    renderToHTML("+++\n") shouldBe "<p>+++</p>\n"
  }
  "45 - 899 - 903" in {
    renderToHTML("===\n") shouldBe "<p>===</p>\n"
  }
  "46 - 908 - 916" in {
    renderToHTML("--\n**\n__\n") shouldBe "<p>--\n**\n__</p>\n"
  }
  "47 - 921 - 929" in {
    renderToHTML(" ***\n  ***\n   ***\n") shouldBe "<hr />\n<hr />\n<hr />\n"
  }
  "48 - 934 - 939" in {
    renderToHTML("    ***\n") shouldBe "<pre><code>***\n</code></pre>\n"
  }
  "49 - 942 - 948" in {
    renderToHTML("Foo\n    ***\n") shouldBe "<p>Foo\n***</p>\n"
  }
  "50 - 953 - 957" in {
    renderToHTML("_____________________________________\n") shouldBe "<hr />\n"
  }
  "51 - 962 - 966" in {
    renderToHTML(" - - -\n") shouldBe "<hr />\n"
  }
  "52 - 969 - 973" in {
    renderToHTML(" **  * ** * ** * **\n") shouldBe "<hr />\n"
  }
  "53 - 976 - 980" in {
    renderToHTML("-     -      -      -\n") shouldBe "<hr />\n"
  }
  "54 - 985 - 989" in {
    renderToHTML("- - - -    \n") shouldBe "<hr />\n"
  }
  "55 - 994 - 1004" in {
    renderToHTML("_ _ _ _ a\n\na------\n\n---a---\n") shouldBe "<p>_ _ _ _ a</p>\n<p>a------</p>\n<p>---a---</p>\n"
  }
  "56 - 1010 - 1014" in {
    renderToHTML(" *-*\n") shouldBe "<p><em>-</em></p>\n"
  }
  "57 - 1019 - 1031" in {
    renderToHTML("- foo\n***\n- bar\n") shouldBe "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
  }
  "58 - 1036 - 1044" in {
    renderToHTML("Foo\n***\nbar\n") shouldBe "<p>Foo</p>\n<hr />\n<p>bar</p>\n"
  }
  "59 - 1053 - 1060" in {
    renderToHTML("Foo\n---\nbar\n") shouldBe "<h2>Foo</h2>\n<p>bar</p>\n"
  }
  "60 - 1066 - 1078" in {
    renderToHTML("* Foo\n* * *\n* Bar\n") shouldBe "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
  }
  "61 - 1083 - 1093" in {
    renderToHTML("- Foo\n- * * *\n") shouldBe "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n"
  }
