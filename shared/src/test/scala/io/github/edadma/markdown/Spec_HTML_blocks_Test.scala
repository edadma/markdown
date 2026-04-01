package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_HTML_blocks_Test extends AnyFreeSpec with Matchers:
  "148 - 2428 - 2443" in {
    renderToHTML("<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n") shouldBe "<table><tr><td>\n<pre>\n**Hello**,\n<p><em>world</em>.\n</pre></p>\n</td></tr></table>\n"
  }
  "149 - 2457 - 2476" in {
    renderToHTML("<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n\nokay.\n") shouldBe "<table>\n  <tr>\n    <td>\n           hi\n    </td>\n  </tr>\n</table>\n<p>okay.</p>\n"
  }
  "150 - 2479 - 2487" in {
    renderToHTML(" <div>\n  *hello*\n         <foo><a>\n") shouldBe " <div>\n  *hello*\n         <foo><a>\n"
  }
  "151 - 2492 - 2498" in {
    renderToHTML("</div>\n*foo*\n") shouldBe "</div>\n*foo*\n"
  }
  "152 - 2503 - 2513" in {
    renderToHTML("<DIV CLASS=\"foo\">\n\n*Markdown*\n\n</DIV>\n") shouldBe "<DIV CLASS=\"foo\">\n<p><em>Markdown</em></p>\n</DIV>\n"
  }
  "153 - 2519 - 2527" in {
    renderToHTML("<div id=\"foo\"\n  class=\"bar\">\n</div>\n") shouldBe "<div id=\"foo\"\n  class=\"bar\">\n</div>\n"
  }
  "154 - 2530 - 2538" in {
    renderToHTML("<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n") shouldBe "<div id=\"foo\" class=\"bar\n  baz\">\n</div>\n"
  }
  "155 - 2542 - 2551" in {
    renderToHTML("<div>\n*foo*\n\n*bar*\n") shouldBe "<div>\n*foo*\n<p><em>bar</em></p>\n"
  }
  "156 - 2558 - 2564" in {
    renderToHTML("<div id=\"foo\"\n*hi*\n") shouldBe "<div id=\"foo\"\n*hi*\n"
  }
  "157 - 2567 - 2573" in {
    renderToHTML("<div class\nfoo\n") shouldBe "<div class\nfoo\n"
  }
  "158 - 2579 - 2585" in {
    renderToHTML("<div *???-&&&-<---\n*foo*\n") shouldBe "<div *???-&&&-<---\n*foo*\n"
  }
  "159 - 2591 - 2595" in {
    renderToHTML("<div><a href=\"bar\">*foo*</a></div>\n") shouldBe "<div><a href=\"bar\">*foo*</a></div>\n"
  }
  "160 - 2598 - 2606" in {
    renderToHTML("<table><tr><td>\nfoo\n</td></tr></table>\n") shouldBe "<table><tr><td>\nfoo\n</td></tr></table>\n"
  }
  "161 - 2615 - 2625" in {
    renderToHTML("<div></div>\n``` c\nint x = 33;\n```\n") shouldBe "<div></div>\n``` c\nint x = 33;\n```\n"
  }
  "162 - 2632 - 2640" in {
    renderToHTML("<a href=\"foo\">\n*bar*\n</a>\n") shouldBe "<a href=\"foo\">\n*bar*\n</a>\n"
  }
  "163 - 2645 - 2653" in {
    renderToHTML("<Warning>\n*bar*\n</Warning>\n") shouldBe "<Warning>\n*bar*\n</Warning>\n"
  }
  "164 - 2656 - 2664" in {
    renderToHTML("<i class=\"foo\">\n*bar*\n</i>\n") shouldBe "<i class=\"foo\">\n*bar*\n</i>\n"
  }
  "165 - 2667 - 2673" in {
    renderToHTML("</ins>\n*bar*\n") shouldBe "</ins>\n*bar*\n"
  }
  "166 - 2682 - 2690" in {
    renderToHTML("<del>\n*foo*\n</del>\n") shouldBe "<del>\n*foo*\n</del>\n"
  }
  "167 - 2697 - 2707" in {
    renderToHTML("<del>\n\n*foo*\n\n</del>\n") shouldBe "<del>\n<p><em>foo</em></p>\n</del>\n"
  }
  "168 - 2715 - 2719" in {
    renderToHTML("<del>*foo*</del>\n") shouldBe "<p><del><em>foo</em></del></p>\n"
  }
  "169 - 2731 - 2747" in {
    renderToHTML("<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\nokay\n") shouldBe "<pre language=\"haskell\"><code>\nimport Text.HTML.TagSoup\n\nmain :: IO ()\nmain = print $ parseTags tags\n</code></pre>\n<p>okay</p>\n"
  }
  "170 - 2752 - 2766" in {
    renderToHTML("<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\nokay\n") shouldBe "<script type=\"text/javascript\">\n// JavaScript example\n\ndocument.getElementById(\"demo\").innerHTML = \"Hello JavaScript!\";\n</script>\n<p>okay</p>\n"
  }
  "171 - 2771 - 2787" in {
    renderToHTML("<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n") shouldBe "<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n"
  }
  "172 - 2791 - 2807" in {
    renderToHTML("<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\nokay\n") shouldBe "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\n<p>okay</p>\n"
  }
  "173 - 2814 - 2824" in {
    renderToHTML("<style\n  type=\"text/css\">\n\nfoo\n") shouldBe "<style\n  type=\"text/css\">\n\nfoo\n"
  }
  "174 - 2827 - 2838" in {
    renderToHTML("> <div>\n> foo\n\nbar\n") shouldBe "<blockquote>\n<div>\nfoo\n</blockquote>\n<p>bar</p>\n"
  }
  "175 - 2841 - 2851" in {
    renderToHTML("- <div>\n- foo\n") shouldBe "<ul>\n<li>\n<div>\n</li>\n<li>foo</li>\n</ul>\n"
  }
  "176 - 2856 - 2862" in {
    renderToHTML("<style>p{color:red;}</style>\n*foo*\n") shouldBe "<style>p{color:red;}</style>\n<p><em>foo</em></p>\n"
  }
  "177 - 2865 - 2871" in {
    renderToHTML("<!-- foo -->*bar*\n*baz*\n") shouldBe "<!-- foo -->*bar*\n<p><em>baz</em></p>\n"
  }
  "178 - 2877 - 2885" in {
    renderToHTML("<script>\nfoo\n</script>1. *bar*\n") shouldBe "<script>\nfoo\n</script>1. *bar*\n"
  }
  "179 - 2890 - 2902" in {
    renderToHTML("<!-- Foo\n\nbar\n   baz -->\nokay\n") shouldBe "<!-- Foo\n\nbar\n   baz -->\n<p>okay</p>\n"
  }
  "180 - 2908 - 2922" in {
    renderToHTML("<?php\n\n  echo '>';\n\n?>\nokay\n") shouldBe "<?php\n\n  echo '>';\n\n?>\n<p>okay</p>\n"
  }
  "181 - 2927 - 2931" in {
    renderToHTML("<!DOCTYPE html>\n") shouldBe "<!DOCTYPE html>\n"
  }
  "182 - 2936 - 2964" in {
    renderToHTML("<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\nokay\n") shouldBe "<![CDATA[\nfunction matchwo(a,b)\n{\n  if (a < b && a < 0) then {\n    return 1;\n\n  } else {\n\n    return 0;\n  }\n}\n]]>\n<p>okay</p>\n"
  }
  "183 - 2970 - 2978" in {
    renderToHTML("  <!-- foo -->\n\n    <!-- foo -->\n") shouldBe "  <!-- foo -->\n<pre><code>&lt;!-- foo --&gt;\n</code></pre>\n"
  }
  "184 - 2981 - 2989" in {
    renderToHTML("  <div>\n\n    <div>\n") shouldBe "  <div>\n<pre><code>&lt;div&gt;\n</code></pre>\n"
  }
  "185 - 2995 - 3005" in {
    renderToHTML("Foo\n<div>\nbar\n</div>\n") shouldBe "<p>Foo</p>\n<div>\nbar\n</div>\n"
  }
  "186 - 3012 - 3022" in {
    renderToHTML("<div>\nbar\n</div>\n*foo*\n") shouldBe "<div>\nbar\n</div>\n*foo*\n"
  }
  "187 - 3027 - 3035" in {
    renderToHTML("Foo\n<a href=\"bar\">\nbaz\n") shouldBe "<p>Foo\n<a href=\"bar\">\nbaz</p>\n"
  }
  "188 - 3068 - 3078" in {
    renderToHTML("<div>\n\n*Emphasized* text.\n\n</div>\n") shouldBe "<div>\n<p><em>Emphasized</em> text.</p>\n</div>\n"
  }
  "189 - 3081 - 3089" in {
    renderToHTML("<div>\n*Emphasized* text.\n</div>\n") shouldBe "<div>\n*Emphasized* text.\n</div>\n"
  }
  "190 - 3103 - 3123" in {
    renderToHTML("<table>\n\n<tr>\n\n<td>\nHi\n</td>\n\n</tr>\n\n</table>\n") shouldBe "<table>\n<tr>\n<td>\nHi\n</td>\n</tr>\n</table>\n"
  }
  "191 - 3130 - 3151" in {
    renderToHTML("<table>\n\n  <tr>\n\n    <td>\n      Hi\n    </td>\n\n  </tr>\n\n</table>\n") shouldBe "<table>\n  <tr>\n<pre><code>&lt;td&gt;\n  Hi\n&lt;/td&gt;\n</code></pre>\n  </tr>\n</table>\n"
  }
