package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Images_Test extends AnyFreeSpec with Matchers:
  "572 - 8541 - 8545" in {
    renderToHTML("![foo](/url \"title\")\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"
  }
  "573 - 8548 - 8554" in {
    renderToHTML("![foo *bar*]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n") shouldBe "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"
  }
  "574 - 8557 - 8561" in {
    renderToHTML("![foo ![bar](/url)](/url2)\n") shouldBe "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n"
  }
  "575 - 8564 - 8568" in {
    renderToHTML("![foo [bar](/url)](/url2)\n") shouldBe "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n"
  }
  "576 - 8578 - 8584" in {
    renderToHTML("![foo *bar*][]\n\n[foo *bar*]: train.jpg \"train & tracks\"\n") shouldBe "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"
  }
  "577 - 8587 - 8593" in {
    renderToHTML("![foo *bar*][foobar]\n\n[FOOBAR]: train.jpg \"train & tracks\"\n") shouldBe "<p><img src=\"train.jpg\" alt=\"foo bar\" title=\"train &amp; tracks\" /></p>\n"
  }
  "578 - 8596 - 8600" in {
    renderToHTML("![foo](train.jpg)\n") shouldBe "<p><img src=\"train.jpg\" alt=\"foo\" /></p>\n"
  }
  "579 - 8603 - 8607" in {
    renderToHTML("My ![foo bar](/path/to/train.jpg  \"title\"   )\n") shouldBe "<p>My <img src=\"/path/to/train.jpg\" alt=\"foo bar\" title=\"title\" /></p>\n"
  }
  "580 - 8610 - 8614" in {
    renderToHTML("![foo](<url>)\n") shouldBe "<p><img src=\"url\" alt=\"foo\" /></p>\n"
  }
  "581 - 8617 - 8621" in {
    renderToHTML("![](/url)\n") shouldBe "<p><img src=\"/url\" alt=\"\" /></p>\n"
  }
  "582 - 8626 - 8632" in {
    renderToHTML("![foo][bar]\n\n[bar]: /url\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" /></p>\n"
  }
  "583 - 8635 - 8641" in {
    renderToHTML("![foo][bar]\n\n[BAR]: /url\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" /></p>\n"
  }
  "584 - 8646 - 8652" in {
    renderToHTML("![foo][]\n\n[foo]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"
  }
  "585 - 8655 - 8661" in {
    renderToHTML("![*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"foo bar\" title=\"title\" /></p>\n"
  }
  "586 - 8666 - 8672" in {
    renderToHTML("![Foo][]\n\n[foo]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"Foo\" title=\"title\" /></p>\n"
  }
  "587 - 8678 - 8686" in {
    renderToHTML("![foo] \n[]\n\n[foo]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" title=\"title\" />\n[]</p>\n"
  }
  "588 - 8691 - 8697" in {
    renderToHTML("![foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"foo\" title=\"title\" /></p>\n"
  }
  "589 - 8700 - 8706" in {
    renderToHTML("![*foo* bar]\n\n[*foo* bar]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"foo bar\" title=\"title\" /></p>\n"
  }
  "590 - 8711 - 8718" in {
    renderToHTML("![[foo]]\n\n[[foo]]: /url \"title\"\n") shouldBe "<p>![[foo]]</p>\n<p>[[foo]]: /url &quot;title&quot;</p>\n"
  }
  "591 - 8723 - 8729" in {
    renderToHTML("![Foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p><img src=\"/url\" alt=\"Foo\" title=\"title\" /></p>\n"
  }
  "592 - 8735 - 8741" in {
    renderToHTML("!\\[foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p>![foo]</p>\n"
  }
  "593 - 8747 - 8753" in {
    renderToHTML("\\![foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p>!<a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
