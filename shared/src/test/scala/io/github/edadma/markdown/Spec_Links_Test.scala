package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Links_Test extends AnyFreeSpec with Matchers:
  "482 - 7539 - 7543" in {
    renderToHTML("[link](/uri \"title\")\n") shouldBe "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
  }
  "483 - 7549 - 7553" in {
    renderToHTML("[link](/uri)\n") shouldBe "<p><a href=\"/uri\">link</a></p>\n"
  }
  "484 - 7555 - 7559" in {
    renderToHTML("[](./target.md)\n") shouldBe "<p><a href=\"./target.md\"></a></p>\n"
  }
  "485 - 7562 - 7566" in {
    renderToHTML("[link]()\n") shouldBe "<p><a href=\"\">link</a></p>\n"
  }
  "486 - 7569 - 7573" in {
    renderToHTML("[link](<>)\n") shouldBe "<p><a href=\"\">link</a></p>\n"
  }
  "487 - 7576 - 7580" in {
    renderToHTML("[]()\n") shouldBe "<p><a href=\"\"></a></p>\n"
  }
  "488 - 7585 - 7589" in {
    renderToHTML("[link](/my uri)\n") shouldBe "<p>[link](/my uri)</p>\n"
  }
  "489 - 7591 - 7595" in {
    renderToHTML("[link](</my uri>)\n") shouldBe "<p><a href=\"/my%20uri\">link</a></p>\n"
  }
  "490 - 7600 - 7606" in {
    renderToHTML("[link](foo\nbar)\n") shouldBe "<p>[link](foo\nbar)</p>\n"
  }
  "491 - 7608 - 7614" in {
    renderToHTML("[link](<foo\nbar>)\n") shouldBe "<p>[link](<foo\nbar>)</p>\n"
  }
  "492 - 7619 - 7623" in {
    renderToHTML("[a](<b)c>)\n") shouldBe "<p><a href=\"b)c\">a</a></p>\n"
  }
  "493 - 7627 - 7631" in {
    renderToHTML("[link](<foo\\>)\n") shouldBe "<p>[link](&lt;foo&gt;)</p>\n"
  }
  "494 - 7636 - 7644" in {
    renderToHTML("[a](<b)c\n[a](<b)c>\n[a](<b>c)\n") shouldBe "<p>[a](&lt;b)c\n[a](&lt;b)c&gt;\n[a](<b>c)</p>\n"
  }
  "495 - 7648 - 7652" in {
    renderToHTML("[link](\\(foo\\))\n") shouldBe "<p><a href=\"(foo)\">link</a></p>\n"
  }
  "496 - 7657 - 7661" in {
    renderToHTML("[link](foo(and(bar)))\n") shouldBe "<p><a href=\"foo(and(bar))\">link</a></p>\n"
  }
  "497 - 7666 - 7670" in {
    renderToHTML("[link](foo(and(bar))\n") shouldBe "<p>[link](foo(and(bar))</p>\n"
  }
  "498 - 7673 - 7677" in {
    renderToHTML("[link](foo\\(and\\(bar\\))\n") shouldBe "<p><a href=\"foo(and(bar)\">link</a></p>\n"
  }
  "499 - 7680 - 7684" in {
    renderToHTML("[link](<foo(and(bar)>)\n") shouldBe "<p><a href=\"foo(and(bar)\">link</a></p>\n"
  }
  "500 - 7690 - 7694" in {
    renderToHTML("[link](foo\\)\\:)\n") shouldBe "<p><a href=\"foo):\">link</a></p>\n"
  }
  "501 - 7699 - 7709" in {
    renderToHTML("[link](#fragment)\n\n[link](https://example.com#fragment)\n\n[link](https://example.com?foo=3#frag)\n") shouldBe "<p><a href=\"#fragment\">link</a></p>\n<p><a href=\"https://example.com#fragment\">link</a></p>\n<p><a href=\"https://example.com?foo=3#frag\">link</a></p>\n"
  }
  "502 - 7715 - 7719" in {
    renderToHTML("[link](foo\\bar)\n") shouldBe "<p><a href=\"foo%5Cbar\">link</a></p>\n"
  }
  "503 - 7731 - 7735" in {
    renderToHTML("[link](foo%20b&auml;)\n") shouldBe "<p><a href=\"foo%20b%C3%A4\">link</a></p>\n"
  }
  "504 - 7742 - 7746" in {
    renderToHTML("[link](\"title\")\n") shouldBe "<p><a href=\"%22title%22\">link</a></p>\n"
  }
  "505 - 7751 - 7759" in {
    renderToHTML("[link](/url \"title\")\n[link](/url 'title')\n[link](/url (title))\n") shouldBe "<p><a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a>\n<a href=\"/url\" title=\"title\">link</a></p>\n"
  }
  "506 - 7765 - 7769" in {
    renderToHTML("[link](/url \"title \\\"&quot;\")\n") shouldBe "<p><a href=\"/url\" title=\"title &quot;&quot;\">link</a></p>\n"
  }
  "507 - 7776 - 7780" in {
    renderToHTML("[link](/url \"title\")\n") shouldBe "<p><a href=\"/url%C2%A0%22title%22\">link</a></p>\n"
  }
  "508 - 7785 - 7789" in {
    renderToHTML("[link](/url \"title \"and\" title\")\n") shouldBe "<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>\n"
  }
  "509 - 7794 - 7798" in {
    renderToHTML("[link](/url 'title \"and\" title')\n") shouldBe "<p><a href=\"/url\" title=\"title &quot;and&quot; title\">link</a></p>\n"
  }
  "510 - 7819 - 7824" in {
    renderToHTML("[link](   /uri\n  \"title\"  )\n") shouldBe "<p><a href=\"/uri\" title=\"title\">link</a></p>\n"
  }
  "511 - 7830 - 7834" in {
    renderToHTML("[link] (/uri)\n") shouldBe "<p>[link] (/uri)</p>\n"
  }
  "512 - 7840 - 7844" in {
    renderToHTML("[link [foo [bar]]](/uri)\n") shouldBe "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"
  }
  "513 - 7847 - 7851" in {
    renderToHTML("[link] bar](/uri)\n") shouldBe "<p>[link] bar](/uri)</p>\n"
  }
  "514 - 7854 - 7858" in {
    renderToHTML("[link [bar](/uri)\n") shouldBe "<p>[link <a href=\"/uri\">bar</a></p>\n"
  }
  "515 - 7861 - 7865" in {
    renderToHTML("[link \\[bar](/uri)\n") shouldBe "<p><a href=\"/uri\">link [bar</a></p>\n"
  }
  "516 - 7870 - 7874" in {
    renderToHTML("[link *foo **bar** `#`*](/uri)\n") shouldBe "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
  }
  "517 - 7877 - 7881" in {
    renderToHTML("[![moon](moon.jpg)](/uri)\n") shouldBe "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"
  }
  "518 - 7886 - 7890" in {
    renderToHTML("[foo [bar](/uri)](/uri)\n") shouldBe "<p>[foo <a href=\"/uri\">bar</a>](/uri)</p>\n"
  }
  "519 - 7893 - 7897" in {
    renderToHTML("[foo *[bar [baz](/uri)](/uri)*](/uri)\n") shouldBe "<p>[foo <em>[bar <a href=\"/uri\">baz</a>](/uri)</em>](/uri)</p>\n"
  }
  "520 - 7900 - 7904" in {
    renderToHTML("![[[foo](uri1)](uri2)](uri3)\n") shouldBe "<p><img src=\"uri3\" alt=\"[foo](uri2)\" /></p>\n"
  }
  "521 - 7910 - 7914" in {
    renderToHTML("*[foo*](/uri)\n") shouldBe "<p>*<a href=\"/uri\">foo*</a></p>\n"
  }
  "522 - 7917 - 7921" in {
    renderToHTML("[foo *bar](baz*)\n") shouldBe "<p><a href=\"baz*\">foo *bar</a></p>\n"
  }
  "523 - 7927 - 7931" in {
    renderToHTML("*foo [bar* baz]\n") shouldBe "<p><em>foo [bar</em> baz]</p>\n"
  }
  "524 - 7937 - 7941" in {
    renderToHTML("[foo <bar attr=\"](baz)\">\n") shouldBe "<p>[foo <bar attr=\"](baz)\"></p>\n"
  }
  "525 - 7944 - 7948" in {
    renderToHTML("[foo`](/uri)`\n") shouldBe "<p>[foo<code>](/uri)</code></p>\n"
  }
  "526 - 7951 - 7955" in {
    renderToHTML("[foo<https://example.com/?search=](uri)>\n") shouldBe "<p>[foo<a href=\"https://example.com/?search=%5D(uri)\">https://example.com/?search=](uri)</a></p>\n"
  }
  "527 - 7989 - 7995" in {
    renderToHTML("[foo][bar]\n\n[bar]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
  "528 - 8004 - 8010" in {
    renderToHTML("[link [foo [bar]]][ref]\n\n[ref]: /uri\n") shouldBe "<p><a href=\"/uri\">link [foo [bar]]</a></p>\n"
  }
  "529 - 8013 - 8019" in {
    renderToHTML("[link \\[bar][ref]\n\n[ref]: /uri\n") shouldBe "<p><a href=\"/uri\">link [bar</a></p>\n"
  }
  "530 - 8024 - 8030" in {
    renderToHTML("[link *foo **bar** `#`*][ref]\n\n[ref]: /uri\n") shouldBe "<p><a href=\"/uri\">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>\n"
  }
  "531 - 8033 - 8039" in {
    renderToHTML("[![moon](moon.jpg)][ref]\n\n[ref]: /uri\n") shouldBe "<p><a href=\"/uri\"><img src=\"moon.jpg\" alt=\"moon\" /></a></p>\n"
  }
  "532 - 8044 - 8050" in {
    renderToHTML("[foo [bar](/uri)][ref]\n\n[ref]: /uri\n") shouldBe "<p>[foo <a href=\"/uri\">bar</a>]<a href=\"/uri\">ref</a></p>\n"
  }
  "533 - 8053 - 8059" in {
    renderToHTML("[foo *bar [baz][ref]*][ref]\n\n[ref]: /uri\n") shouldBe "<p>[foo <em>bar <a href=\"/uri\">baz</a></em>]<a href=\"/uri\">ref</a></p>\n"
  }
  "534 - 8068 - 8074" in {
    renderToHTML("*[foo*][ref]\n\n[ref]: /uri\n") shouldBe "<p>*<a href=\"/uri\">foo*</a></p>\n"
  }
  "535 - 8077 - 8083" in {
    renderToHTML("[foo *bar][ref]*\n\n[ref]: /uri\n") shouldBe "<p><a href=\"/uri\">foo *bar</a>*</p>\n"
  }
  "536 - 8089 - 8095" in {
    renderToHTML("[foo <bar attr=\"][ref]\">\n\n[ref]: /uri\n") shouldBe "<p>[foo <bar attr=\"][ref]\"></p>\n"
  }
  "537 - 8098 - 8104" in {
    renderToHTML("[foo`][ref]`\n\n[ref]: /uri\n") shouldBe "<p>[foo<code>][ref]</code></p>\n"
  }
  "538 - 8107 - 8113" in {
    renderToHTML("[foo<https://example.com/?search=][ref]>\n\n[ref]: /uri\n") shouldBe "<p>[foo<a href=\"https://example.com/?search=%5D%5Bref%5D\">https://example.com/?search=][ref]</a></p>\n"
  }
  "539 - 8118 - 8124" in {
    renderToHTML("[foo][BaR]\n\n[bar]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
  "540 - 8129 - 8135" in {
    renderToHTML("[ẞ]\n\n[SS]: /url\n") shouldBe "<p><a href=\"/url\">ẞ</a></p>\n"
  }
  "541 - 8141 - 8148" in {
    renderToHTML("[Foo\n  bar]: /url\n\n[Baz][Foo bar]\n") shouldBe "<p><a href=\"/url\">Baz</a></p>\n"
  }
  "542 - 8154 - 8160" in {
    renderToHTML("[foo] [bar]\n\n[bar]: /url \"title\"\n") shouldBe "<p>[foo] <a href=\"/url\" title=\"title\">bar</a></p>\n"
  }
  "543 - 8163 - 8171" in {
    renderToHTML("[foo]\n[bar]\n\n[bar]: /url \"title\"\n") shouldBe "<p>[foo]\n<a href=\"/url\" title=\"title\">bar</a></p>\n"
  }
  "544 - 8204 - 8212" in {
    renderToHTML("[foo]: /url1\n\n[foo]: /url2\n\n[bar][foo]\n") shouldBe "<p><a href=\"/url1\">bar</a></p>\n"
  }
  "545 - 8219 - 8225" in {
    renderToHTML("[bar][foo\\!]\n\n[foo!]: /url\n") shouldBe "<p>[bar][foo!]</p>\n"
  }
  "546 - 8231 - 8238" in {
    renderToHTML("[foo][ref[]\n\n[ref[]: /uri\n") shouldBe "<p>[foo][ref[]</p>\n<p>[ref[]: /uri</p>\n"
  }
  "547 - 8241 - 8248" in {
    renderToHTML("[foo][ref[bar]]\n\n[ref[bar]]: /uri\n") shouldBe "<p>[foo][ref[bar]]</p>\n<p>[ref[bar]]: /uri</p>\n"
  }
  "548 - 8251 - 8258" in {
    renderToHTML("[[[foo]]]\n\n[[[foo]]]: /url\n") shouldBe "<p>[[[foo]]]</p>\n<p>[[[foo]]]: /url</p>\n"
  }
  "549 - 8261 - 8267" in {
    renderToHTML("[foo][ref\\[]\n\n[ref\\[]: /uri\n") shouldBe "<p><a href=\"/uri\">foo</a></p>\n"
  }
  "550 - 8272 - 8278" in {
    renderToHTML("[bar\\\\]: /uri\n\n[bar\\\\]\n") shouldBe "<p><a href=\"/uri\">bar\\</a></p>\n"
  }
  "551 - 8284 - 8291" in {
    renderToHTML("[]\n\n[]: /uri\n") shouldBe "<p>[]</p>\n<p>[]: /uri</p>\n"
  }
  "552 - 8294 - 8305" in {
    renderToHTML("[\n ]\n\n[\n ]: /uri\n") shouldBe "<p>[\n]</p>\n<p>[\n]: /uri</p>\n"
  }
  "553 - 8317 - 8323" in {
    renderToHTML("[foo][]\n\n[foo]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
  "554 - 8326 - 8332" in {
    renderToHTML("[*foo* bar][]\n\n[*foo* bar]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"
  }
  "555 - 8337 - 8343" in {
    renderToHTML("[Foo][]\n\n[foo]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"
  }
  "556 - 8350 - 8358" in {
    renderToHTML("[foo] \n[]\n\n[foo]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a>\n[]</p>\n"
  }
  "557 - 8370 - 8376" in {
    renderToHTML("[foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">foo</a></p>\n"
  }
  "558 - 8379 - 8385" in {
    renderToHTML("[*foo* bar]\n\n[*foo* bar]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\"><em>foo</em> bar</a></p>\n"
  }
  "559 - 8388 - 8394" in {
    renderToHTML("[[*foo* bar]]\n\n[*foo* bar]: /url \"title\"\n") shouldBe "<p>[<a href=\"/url\" title=\"title\"><em>foo</em> bar</a>]</p>\n"
  }
  "560 - 8397 - 8403" in {
    renderToHTML("[[bar [foo]\n\n[foo]: /url\n") shouldBe "<p>[[bar <a href=\"/url\">foo</a></p>\n"
  }
  "561 - 8408 - 8414" in {
    renderToHTML("[Foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p><a href=\"/url\" title=\"title\">Foo</a></p>\n"
  }
  "562 - 8419 - 8425" in {
    renderToHTML("[foo] bar\n\n[foo]: /url\n") shouldBe "<p><a href=\"/url\">foo</a> bar</p>\n"
  }
  "563 - 8431 - 8437" in {
    renderToHTML("\\[foo]\n\n[foo]: /url \"title\"\n") shouldBe "<p>[foo]</p>\n"
  }
  "564 - 8443 - 8449" in {
    renderToHTML("[foo*]: /url\n\n*[foo*]\n") shouldBe "<p>*<a href=\"/url\">foo*</a></p>\n"
  }
  "565 - 8455 - 8462" in {
    renderToHTML("[foo][bar]\n\n[foo]: /url1\n[bar]: /url2\n") shouldBe "<p><a href=\"/url2\">foo</a></p>\n"
  }
  "566 - 8464 - 8470" in {
    renderToHTML("[foo][]\n\n[foo]: /url1\n") shouldBe "<p><a href=\"/url1\">foo</a></p>\n"
  }
  "567 - 8474 - 8480" in {
    renderToHTML("[foo]()\n\n[foo]: /url1\n") shouldBe "<p><a href=\"\">foo</a></p>\n"
  }
  "568 - 8482 - 8488" in {
    renderToHTML("[foo](not a link)\n\n[foo]: /url1\n") shouldBe "<p><a href=\"/url1\">foo</a>(not a link)</p>\n"
  }
  "569 - 8493 - 8499" in {
    renderToHTML("[foo][bar][baz]\n\n[baz]: /url\n") shouldBe "<p>[foo]<a href=\"/url\">bar</a></p>\n"
  }
  "570 - 8505 - 8512" in {
    renderToHTML("[foo][bar][baz]\n\n[baz]: /url1\n[bar]: /url2\n") shouldBe "<p><a href=\"/url2\">foo</a><a href=\"/url1\">baz</a></p>\n"
  }
  "571 - 8518 - 8525" in {
    renderToHTML("[foo][bar][baz]\n\n[baz]: /url1\n[foo]: /url2\n") shouldBe "<p>[foo]<a href=\"/url1\">bar</a></p>\n"
  }
