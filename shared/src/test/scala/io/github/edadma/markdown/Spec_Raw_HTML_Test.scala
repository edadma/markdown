package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Raw_HTML_Test extends AnyFreeSpec with Matchers:
  "613 - 9016 - 9020" in {
    renderToHTML("<a><bab><c2c>\n") shouldBe "<p><a><bab><c2c></p>\n"
  }
  "614 - 9025 - 9029" in {
    renderToHTML("<a/><b2/>\n") shouldBe "<p><a/><b2/></p>\n"
  }
  "615 - 9034 - 9040" in {
    renderToHTML("<a  /><b2\ndata=\"foo\" >\n") shouldBe "<p><a  /><b2\ndata=\"foo\" ></p>\n"
  }
  "616 - 9045 - 9051" in {
    renderToHTML("<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n") shouldBe "<p><a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 /></p>\n"
  }
  "617 - 9056 - 9060" in {
    renderToHTML("Foo <responsive-image src=\"foo.jpg\" />\n") shouldBe "<p>Foo <responsive-image src=\"foo.jpg\" /></p>\n"
  }
  "618 - 9065 - 9069" in {
    renderToHTML("<33> <__>\n") shouldBe "<p>&lt;33&gt; &lt;__&gt;</p>\n"
  }
  "619 - 9074 - 9078" in {
    renderToHTML("<a h*#ref=\"hi\">\n") shouldBe "<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>\n"
  }
  "620 - 9083 - 9087" in {
    renderToHTML("<a href=\"hi'> <a href=hi'>\n") shouldBe "<p>&lt;a href=&quot;hi'&gt; &lt;a href=hi'&gt;</p>\n"
  }
  "621 - 9092 - 9102" in {
    renderToHTML("< a><\nfoo><bar/ >\n<foo bar=baz\nbim!bop />\n") shouldBe "<p>&lt; a&gt;&lt;\nfoo&gt;&lt;bar/ &gt;\n&lt;foo bar=baz\nbim!bop /&gt;</p>\n"
  }
  "622 - 9107 - 9111" in {
    renderToHTML("<a href='bar'title=title>\n") shouldBe "<p>&lt;a href='bar'title=title&gt;</p>\n"
  }
  "623 - 9116 - 9120" in {
    renderToHTML("</a></foo >\n") shouldBe "<p></a></foo ></p>\n"
  }
  "624 - 9125 - 9129" in {
    renderToHTML("</a href=\"foo\">\n") shouldBe "<p>&lt;/a href=&quot;foo&quot;&gt;</p>\n"
  }
  "625 - 9134 - 9140" in {
    renderToHTML("foo <!-- this is a --\ncomment - with hyphens -->\n") shouldBe "<p>foo <!-- this is a --\ncomment - with hyphens --></p>\n"
  }
  "626 - 9142 - 9149" in {
    renderToHTML("foo <!--> foo -->\n\nfoo <!---> foo -->\n") shouldBe "<p>foo <!--> foo --&gt;</p>\n<p>foo <!---> foo --&gt;</p>\n"
  }
  "627 - 9154 - 9158" in {
    renderToHTML("foo <?php echo $a; ?>\n") shouldBe "<p>foo <?php echo $a; ?></p>\n"
  }
  "628 - 9163 - 9167" in {
    renderToHTML("foo <!ELEMENT br EMPTY>\n") shouldBe "<p>foo <!ELEMENT br EMPTY></p>\n"
  }
  "629 - 9172 - 9176" in {
    renderToHTML("foo <![CDATA[>&<]]>\n") shouldBe "<p>foo <![CDATA[>&<]]></p>\n"
  }
  "630 - 9182 - 9186" in {
    renderToHTML("foo <a href=\"&ouml;\">\n") shouldBe "<p>foo <a href=\"&ouml;\"></p>\n"
  }
  "631 - 9191 - 9195" in {
    renderToHTML("foo <a href=\"\\*\">\n") shouldBe "<p>foo <a href=\"\\*\"></p>\n"
  }
  "632 - 9198 - 9202" in {
    renderToHTML("<a href=\"\\\"\">\n") shouldBe "<p>&lt;a href=&quot;&quot;&quot;&gt;</p>\n"
  }
