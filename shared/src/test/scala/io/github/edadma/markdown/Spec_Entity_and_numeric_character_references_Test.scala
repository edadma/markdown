package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Entity_and_numeric_character_references_Test extends AnyFreeSpec with Matchers:
  "25 - 649 - 657" in {
    renderToHTML("&nbsp; &amp; &copy; &AElig; &Dcaron;\n&frac34; &HilbertSpace; &DifferentialD;\n&ClockwiseContourIntegral; &ngE;\n") shouldBe "<p>  &amp; © Æ Ď\n¾ ℋ ⅆ\n∲ ≧̸</p>\n"
  }
  "26 - 668 - 672" in {
    renderToHTML("&#35; &#1234; &#992; &#0;\n") shouldBe "<p># Ӓ Ϡ �</p>\n"
  }
  "27 - 681 - 685" in {
    renderToHTML("&#X22; &#XD06; &#xcab;\n") shouldBe "<p>&quot; ആ ಫ</p>\n"
  }
  "28 - 690 - 700" in {
    renderToHTML("&nbsp &x; &#; &#x;\n&#87654321;\n&#abcdef0;\n&ThisIsNotDefined; &hi?;\n") shouldBe "<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;\n&amp;#87654321;\n&amp;#abcdef0;\n&amp;ThisIsNotDefined; &amp;hi?;</p>\n"
  }
  "29 - 707 - 711" in {
    renderToHTML("&copy\n") shouldBe "<p>&amp;copy</p>\n"
  }
  "30 - 717 - 721" in {
    renderToHTML("&MadeUpEntity;\n") shouldBe "<p>&amp;MadeUpEntity;</p>\n"
  }
  "31 - 728 - 732" in {
    renderToHTML("<a href=\"&ouml;&ouml;.html\">\n") shouldBe "<a href=\"&ouml;&ouml;.html\">\n"
  }
  "32 - 735 - 739" in {
    renderToHTML("[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n") shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
  }
  "33 - 742 - 748" in {
    renderToHTML("[foo]\n\n[foo]: /f&ouml;&ouml; \"f&ouml;&ouml;\"\n") shouldBe "<p><a href=\"/f%C3%B6%C3%B6\" title=\"föö\">foo</a></p>\n"
  }
  "34 - 751 - 758" in {
    renderToHTML("``` f&ouml;&ouml;\nfoo\n```\n") shouldBe "<pre><code class=\"language-föö\">foo\n</code></pre>\n"
  }
  "35 - 764 - 768" in {
    renderToHTML("`f&ouml;&ouml;`\n") shouldBe "<p><code>f&amp;ouml;&amp;ouml;</code></p>\n"
  }
  "36 - 771 - 776" in {
    renderToHTML("    f&ouml;f&ouml;\n") shouldBe "<pre><code>f&amp;ouml;f&amp;ouml;\n</code></pre>\n"
  }
  "37 - 783 - 789" in {
    renderToHTML("&#42;foo&#42;\n*foo*\n") shouldBe "<p>*foo*\n<em>foo</em></p>\n"
  }
  "38 - 791 - 800" in {
    renderToHTML("&#42; foo\n\n* foo\n") shouldBe "<p>* foo</p>\n<ul>\n<li>foo</li>\n</ul>\n"
  }
  "39 - 802 - 808" in {
    renderToHTML("foo&#10;&#10;bar\n") shouldBe "<p>foo\n\nbar</p>\n"
  }
  "40 - 810 - 814" in {
    renderToHTML("&#9;foo\n") shouldBe "<p>\tfoo</p>\n"
  }
  "41 - 817 - 821" in {
    renderToHTML("[a](url &quot;tit&quot;)\n") shouldBe "<p>[a](url &quot;tit&quot;)</p>\n"
  }
