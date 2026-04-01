package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Spec_Emphasis_and_strong_emphasis_Test extends AnyFreeSpec with Matchers:
  "350 - 6308 - 6312" in {
    renderToHTML("*foo bar*\n") shouldBe "<p><em>foo bar</em></p>\n"
  }
  "351 - 6318 - 6322" in {
    renderToHTML("a * foo bar*\n") shouldBe "<p>a * foo bar*</p>\n"
  }
  "352 - 6329 - 6333" in {
    renderToHTML("a*\"foo\"*\n") shouldBe "<p>a*&quot;foo&quot;*</p>\n"
  }
  "353 - 6338 - 6342" in {
    renderToHTML("* a *\n") shouldBe "<p>* a *</p>\n"
  }
  "354 - 6347 - 6357" in {
    renderToHTML("*$*alpha.\n\n*£*bravo.\n\n*€*charlie.\n") shouldBe "<p>*$*alpha.</p>\n<p>*£*bravo.</p>\n<p>*€*charlie.</p>\n"
  }
  "355 - 6362 - 6366" in {
    renderToHTML("foo*bar*\n") shouldBe "<p>foo<em>bar</em></p>\n"
  }
  "356 - 6369 - 6373" in {
    renderToHTML("5*6*78\n") shouldBe "<p>5<em>6</em>78</p>\n"
  }
  "357 - 6378 - 6382" in {
    renderToHTML("_foo bar_\n") shouldBe "<p><em>foo bar</em></p>\n"
  }
  "358 - 6388 - 6392" in {
    renderToHTML("_ foo bar_\n") shouldBe "<p>_ foo bar_</p>\n"
  }
  "359 - 6398 - 6402" in {
    renderToHTML("a_\"foo\"_\n") shouldBe "<p>a_&quot;foo&quot;_</p>\n"
  }
  "360 - 6407 - 6411" in {
    renderToHTML("foo_bar_\n") shouldBe "<p>foo_bar_</p>\n"
  }
  "361 - 6414 - 6418" in {
    renderToHTML("5_6_78\n") shouldBe "<p>5_6_78</p>\n"
  }
  "362 - 6421 - 6425" in {
    renderToHTML("пристаням_стремятся_\n") shouldBe "<p>пристаням_стремятся_</p>\n"
  }
  "363 - 6431 - 6435" in {
    renderToHTML("aa_\"bb\"_cc\n") shouldBe "<p>aa_&quot;bb&quot;_cc</p>\n"
  }
  "364 - 6442 - 6446" in {
    renderToHTML("foo-_(bar)_\n") shouldBe "<p>foo-<em>(bar)</em></p>\n"
  }
  "365 - 6454 - 6458" in {
    renderToHTML("_foo*\n") shouldBe "<p>_foo*</p>\n"
  }
  "366 - 6464 - 6468" in {
    renderToHTML("*foo bar *\n") shouldBe "<p>*foo bar *</p>\n"
  }
  "367 - 6473 - 6479" in {
    renderToHTML("*foo bar\n*\n") shouldBe "<p>*foo bar\n*</p>\n"
  }
  "368 - 6486 - 6490" in {
    renderToHTML("*(*foo)\n") shouldBe "<p>*(*foo)</p>\n"
  }
  "369 - 6496 - 6500" in {
    renderToHTML("*(*foo*)*\n") shouldBe "<p><em>(<em>foo</em>)</em></p>\n"
  }
  "370 - 6505 - 6509" in {
    renderToHTML("*foo*bar\n") shouldBe "<p><em>foo</em>bar</p>\n"
  }
  "371 - 6518 - 6522" in {
    renderToHTML("_foo bar _\n") shouldBe "<p>_foo bar _</p>\n"
  }
  "372 - 6528 - 6532" in {
    renderToHTML("_(_foo)\n") shouldBe "<p>_(_foo)</p>\n"
  }
  "373 - 6537 - 6541" in {
    renderToHTML("_(_foo_)_\n") shouldBe "<p><em>(<em>foo</em>)</em></p>\n"
  }
  "374 - 6546 - 6550" in {
    renderToHTML("_foo_bar\n") shouldBe "<p>_foo_bar</p>\n"
  }
  "375 - 6553 - 6557" in {
    renderToHTML("_пристаням_стремятся\n") shouldBe "<p>_пристаням_стремятся</p>\n"
  }
  "376 - 6560 - 6564" in {
    renderToHTML("_foo_bar_baz_\n") shouldBe "<p><em>foo_bar_baz</em></p>\n"
  }
  "377 - 6571 - 6575" in {
    renderToHTML("_(bar)_.\n") shouldBe "<p><em>(bar)</em>.</p>\n"
  }
  "378 - 6580 - 6584" in {
    renderToHTML("**foo bar**\n") shouldBe "<p><strong>foo bar</strong></p>\n"
  }
  "379 - 6590 - 6594" in {
    renderToHTML("** foo bar**\n") shouldBe "<p>** foo bar**</p>\n"
  }
  "380 - 6601 - 6605" in {
    renderToHTML("a**\"foo\"**\n") shouldBe "<p>a**&quot;foo&quot;**</p>\n"
  }
  "381 - 6610 - 6614" in {
    renderToHTML("foo**bar**\n") shouldBe "<p>foo<strong>bar</strong></p>\n"
  }
  "382 - 6619 - 6623" in {
    renderToHTML("__foo bar__\n") shouldBe "<p><strong>foo bar</strong></p>\n"
  }
  "383 - 6629 - 6633" in {
    renderToHTML("__ foo bar__\n") shouldBe "<p>__ foo bar__</p>\n"
  }
  "384 - 6637 - 6643" in {
    renderToHTML("__\nfoo bar__\n") shouldBe "<p>__\nfoo bar__</p>\n"
  }
  "385 - 6649 - 6653" in {
    renderToHTML("a__\"foo\"__\n") shouldBe "<p>a__&quot;foo&quot;__</p>\n"
  }
  "386 - 6658 - 6662" in {
    renderToHTML("foo__bar__\n") shouldBe "<p>foo__bar__</p>\n"
  }
  "387 - 6665 - 6669" in {
    renderToHTML("5__6__78\n") shouldBe "<p>5__6__78</p>\n"
  }
  "388 - 6672 - 6676" in {
    renderToHTML("пристаням__стремятся__\n") shouldBe "<p>пристаням__стремятся__</p>\n"
  }
  "389 - 6679 - 6683" in {
    renderToHTML("__foo, __bar__, baz__\n") shouldBe "<p><strong>foo, <strong>bar</strong>, baz</strong></p>\n"
  }
  "390 - 6690 - 6694" in {
    renderToHTML("foo-__(bar)__\n") shouldBe "<p>foo-<strong>(bar)</strong></p>\n"
  }
  "391 - 6703 - 6707" in {
    renderToHTML("**foo bar **\n") shouldBe "<p>**foo bar **</p>\n"
  }
  "392 - 6716 - 6720" in {
    renderToHTML("**(**foo)\n") shouldBe "<p>**(**foo)</p>\n"
  }
  "393 - 6726 - 6730" in {
    renderToHTML("*(**foo**)*\n") shouldBe "<p><em>(<strong>foo</strong>)</em></p>\n"
  }
  "394 - 6733 - 6739" in {
    renderToHTML("**Gomphocarpus (*Gomphocarpus physocarpus*, syn.\n*Asclepias physocarpa*)**\n") shouldBe "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.\n<em>Asclepias physocarpa</em>)</strong></p>\n"
  }
  "395 - 6742 - 6746" in {
    renderToHTML("**foo \"*bar*\" foo**\n") shouldBe "<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>\n"
  }
  "396 - 6751 - 6755" in {
    renderToHTML("**foo**bar\n") shouldBe "<p><strong>foo</strong>bar</p>\n"
  }
  "397 - 6763 - 6767" in {
    renderToHTML("__foo bar __\n") shouldBe "<p>__foo bar __</p>\n"
  }
  "398 - 6773 - 6777" in {
    renderToHTML("__(__foo)\n") shouldBe "<p>__(__foo)</p>\n"
  }
  "399 - 6783 - 6787" in {
    renderToHTML("_(__foo__)_\n") shouldBe "<p><em>(<strong>foo</strong>)</em></p>\n"
  }
  "400 - 6792 - 6796" in {
    renderToHTML("__foo__bar\n") shouldBe "<p>__foo__bar</p>\n"
  }
  "401 - 6799 - 6803" in {
    renderToHTML("__пристаням__стремятся\n") shouldBe "<p>__пристаням__стремятся</p>\n"
  }
  "402 - 6806 - 6810" in {
    renderToHTML("__foo__bar__baz__\n") shouldBe "<p><strong>foo__bar__baz</strong></p>\n"
  }
  "403 - 6817 - 6821" in {
    renderToHTML("__(bar)__.\n") shouldBe "<p><strong>(bar)</strong>.</p>\n"
  }
  "404 - 6829 - 6833" in {
    renderToHTML("*foo [bar](/url)*\n") shouldBe "<p><em>foo <a href=\"/url\">bar</a></em></p>\n"
  }
  "405 - 6836 - 6842" in {
    renderToHTML("*foo\nbar*\n") shouldBe "<p><em>foo\nbar</em></p>\n"
  }
  "406 - 6848 - 6852" in {
    renderToHTML("_foo __bar__ baz_\n") shouldBe "<p><em>foo <strong>bar</strong> baz</em></p>\n"
  }
  "407 - 6855 - 6859" in {
    renderToHTML("_foo _bar_ baz_\n") shouldBe "<p><em>foo <em>bar</em> baz</em></p>\n"
  }
  "408 - 6862 - 6866" in {
    renderToHTML("__foo_ bar_\n") shouldBe "<p><em><em>foo</em> bar</em></p>\n"
  }
  "409 - 6869 - 6873" in {
    renderToHTML("*foo *bar**\n") shouldBe "<p><em>foo <em>bar</em></em></p>\n"
  }
  "410 - 6876 - 6880" in {
    renderToHTML("*foo **bar** baz*\n") shouldBe "<p><em>foo <strong>bar</strong> baz</em></p>\n"
  }
  "411 - 6882 - 6886" in {
    renderToHTML("*foo**bar**baz*\n") shouldBe "<p><em>foo<strong>bar</strong>baz</em></p>\n"
  }
  "412 - 6906 - 6910" in {
    renderToHTML("*foo**bar*\n") shouldBe "<p><em>foo**bar</em></p>\n"
  }
  "413 - 6919 - 6923" in {
    renderToHTML("***foo** bar*\n") shouldBe "<p><em><strong>foo</strong> bar</em></p>\n"
  }
  "414 - 6926 - 6930" in {
    renderToHTML("*foo **bar***\n") shouldBe "<p><em>foo <strong>bar</strong></em></p>\n"
  }
  "415 - 6933 - 6937" in {
    renderToHTML("*foo**bar***\n") shouldBe "<p><em>foo<strong>bar</strong></em></p>\n"
  }
  "416 - 6944 - 6948" in {
    renderToHTML("foo***bar***baz\n") shouldBe "<p>foo<em><strong>bar</strong></em>baz</p>\n"
  }
  "417 - 6950 - 6954" in {
    renderToHTML("foo******bar*********baz\n") shouldBe "<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>\n"
  }
  "418 - 6959 - 6963" in {
    renderToHTML("*foo **bar *baz* bim** bop*\n") shouldBe "<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>\n"
  }
  "419 - 6966 - 6970" in {
    renderToHTML("*foo [*bar*](/url)*\n") shouldBe "<p><em>foo <a href=\"/url\"><em>bar</em></a></em></p>\n"
  }
  "420 - 6975 - 6979" in {
    renderToHTML("** is not an empty emphasis\n") shouldBe "<p>** is not an empty emphasis</p>\n"
  }
  "421 - 6982 - 6986" in {
    renderToHTML("**** is not an empty strong emphasis\n") shouldBe "<p>**** is not an empty strong emphasis</p>\n"
  }
  "422 - 6995 - 6999" in {
    renderToHTML("**foo [bar](/url)**\n") shouldBe "<p><strong>foo <a href=\"/url\">bar</a></strong></p>\n"
  }
  "423 - 7002 - 7008" in {
    renderToHTML("**foo\nbar**\n") shouldBe "<p><strong>foo\nbar</strong></p>\n"
  }
  "424 - 7014 - 7018" in {
    renderToHTML("__foo _bar_ baz__\n") shouldBe "<p><strong>foo <em>bar</em> baz</strong></p>\n"
  }
  "425 - 7021 - 7025" in {
    renderToHTML("__foo __bar__ baz__\n") shouldBe "<p><strong>foo <strong>bar</strong> baz</strong></p>\n"
  }
  "426 - 7028 - 7032" in {
    renderToHTML("____foo__ bar__\n") shouldBe "<p><strong><strong>foo</strong> bar</strong></p>\n"
  }
  "427 - 7035 - 7039" in {
    renderToHTML("**foo **bar****\n") shouldBe "<p><strong>foo <strong>bar</strong></strong></p>\n"
  }
  "428 - 7042 - 7046" in {
    renderToHTML("**foo *bar* baz**\n") shouldBe "<p><strong>foo <em>bar</em> baz</strong></p>\n"
  }
  "429 - 7049 - 7053" in {
    renderToHTML("**foo*bar*baz**\n") shouldBe "<p><strong>foo<em>bar</em>baz</strong></p>\n"
  }
  "430 - 7056 - 7060" in {
    renderToHTML("***foo* bar**\n") shouldBe "<p><strong><em>foo</em> bar</strong></p>\n"
  }
  "431 - 7063 - 7067" in {
    renderToHTML("**foo *bar***\n") shouldBe "<p><strong>foo <em>bar</em></strong></p>\n"
  }
  "432 - 7072 - 7078" in {
    renderToHTML("**foo *bar **baz**\nbim* bop**\n") shouldBe "<p><strong>foo <em>bar <strong>baz</strong>\nbim</em> bop</strong></p>\n"
  }
  "433 - 7081 - 7085" in {
    renderToHTML("**foo [*bar*](/url)**\n") shouldBe "<p><strong>foo <a href=\"/url\"><em>bar</em></a></strong></p>\n"
  }
  "434 - 7090 - 7094" in {
    renderToHTML("__ is not an empty emphasis\n") shouldBe "<p>__ is not an empty emphasis</p>\n"
  }
  "435 - 7097 - 7101" in {
    renderToHTML("____ is not an empty strong emphasis\n") shouldBe "<p>____ is not an empty strong emphasis</p>\n"
  }
  "436 - 7107 - 7111" in {
    renderToHTML("foo ***\n") shouldBe "<p>foo ***</p>\n"
  }
  "437 - 7114 - 7118" in {
    renderToHTML("foo *\\**\n") shouldBe "<p>foo <em>*</em></p>\n"
  }
  "438 - 7121 - 7125" in {
    renderToHTML("foo *_*\n") shouldBe "<p>foo <em>_</em></p>\n"
  }
  "439 - 7128 - 7132" in {
    renderToHTML("foo *****\n") shouldBe "<p>foo *****</p>\n"
  }
  "440 - 7135 - 7139" in {
    renderToHTML("foo **\\***\n") shouldBe "<p>foo <strong>*</strong></p>\n"
  }
  "441 - 7142 - 7146" in {
    renderToHTML("foo **_**\n") shouldBe "<p>foo <strong>_</strong></p>\n"
  }
  "442 - 7153 - 7157" in {
    renderToHTML("**foo*\n") shouldBe "<p>*<em>foo</em></p>\n"
  }
  "443 - 7160 - 7164" in {
    renderToHTML("*foo**\n") shouldBe "<p><em>foo</em>*</p>\n"
  }
  "444 - 7167 - 7171" in {
    renderToHTML("***foo**\n") shouldBe "<p>*<strong>foo</strong></p>\n"
  }
  "445 - 7174 - 7178" in {
    renderToHTML("****foo*\n") shouldBe "<p>***<em>foo</em></p>\n"
  }
  "446 - 7181 - 7185" in {
    renderToHTML("**foo***\n") shouldBe "<p><strong>foo</strong>*</p>\n"
  }
  "447 - 7188 - 7192" in {
    renderToHTML("*foo****\n") shouldBe "<p><em>foo</em>***</p>\n"
  }
  "448 - 7198 - 7202" in {
    renderToHTML("foo ___\n") shouldBe "<p>foo ___</p>\n"
  }
  "449 - 7205 - 7209" in {
    renderToHTML("foo _\\__\n") shouldBe "<p>foo <em>_</em></p>\n"
  }
  "450 - 7212 - 7216" in {
    renderToHTML("foo _*_\n") shouldBe "<p>foo <em>*</em></p>\n"
  }
  "451 - 7219 - 7223" in {
    renderToHTML("foo _____\n") shouldBe "<p>foo _____</p>\n"
  }
  "452 - 7226 - 7230" in {
    renderToHTML("foo __\\___\n") shouldBe "<p>foo <strong>_</strong></p>\n"
  }
  "453 - 7233 - 7237" in {
    renderToHTML("foo __*__\n") shouldBe "<p>foo <strong>*</strong></p>\n"
  }
  "454 - 7240 - 7244" in {
    renderToHTML("__foo_\n") shouldBe "<p>_<em>foo</em></p>\n"
  }
  "455 - 7251 - 7255" in {
    renderToHTML("_foo__\n") shouldBe "<p><em>foo</em>_</p>\n"
  }
  "456 - 7258 - 7262" in {
    renderToHTML("___foo__\n") shouldBe "<p>_<strong>foo</strong></p>\n"
  }
  "457 - 7265 - 7269" in {
    renderToHTML("____foo_\n") shouldBe "<p>___<em>foo</em></p>\n"
  }
  "458 - 7272 - 7276" in {
    renderToHTML("__foo___\n") shouldBe "<p><strong>foo</strong>_</p>\n"
  }
  "459 - 7279 - 7283" in {
    renderToHTML("_foo____\n") shouldBe "<p><em>foo</em>___</p>\n"
  }
  "460 - 7289 - 7293" in {
    renderToHTML("**foo**\n") shouldBe "<p><strong>foo</strong></p>\n"
  }
  "461 - 7296 - 7300" in {
    renderToHTML("*_foo_*\n") shouldBe "<p><em><em>foo</em></em></p>\n"
  }
  "462 - 7303 - 7307" in {
    renderToHTML("__foo__\n") shouldBe "<p><strong>foo</strong></p>\n"
  }
  "463 - 7310 - 7314" in {
    renderToHTML("_*foo*_\n") shouldBe "<p><em><em>foo</em></em></p>\n"
  }
  "464 - 7320 - 7324" in {
    renderToHTML("****foo****\n") shouldBe "<p><strong><strong>foo</strong></strong></p>\n"
  }
  "465 - 7327 - 7331" in {
    renderToHTML("____foo____\n") shouldBe "<p><strong><strong>foo</strong></strong></p>\n"
  }
  "466 - 7338 - 7342" in {
    renderToHTML("******foo******\n") shouldBe "<p><strong><strong><strong>foo</strong></strong></strong></p>\n"
  }
  "467 - 7347 - 7351" in {
    renderToHTML("***foo***\n") shouldBe "<p><em><strong>foo</strong></em></p>\n"
  }
  "468 - 7354 - 7358" in {
    renderToHTML("_____foo_____\n") shouldBe "<p><em><strong><strong>foo</strong></strong></em></p>\n"
  }
  "469 - 7363 - 7367" in {
    renderToHTML("*foo _bar* baz_\n") shouldBe "<p><em>foo _bar</em> baz_</p>\n"
  }
  "470 - 7370 - 7374" in {
    renderToHTML("*foo __bar *baz bim__ bam*\n") shouldBe "<p><em>foo <strong>bar *baz bim</strong> bam</em></p>\n"
  }
  "471 - 7379 - 7383" in {
    renderToHTML("**foo **bar baz**\n") shouldBe "<p>**foo <strong>bar baz</strong></p>\n"
  }
  "472 - 7386 - 7390" in {
    renderToHTML("*foo *bar baz*\n") shouldBe "<p>*foo <em>bar baz</em></p>\n"
  }
  "473 - 7395 - 7399" in {
    renderToHTML("*[bar*](/url)\n") shouldBe "<p>*<a href=\"/url\">bar*</a></p>\n"
  }
  "474 - 7402 - 7406" in {
    renderToHTML("_foo [bar_](/url)\n") shouldBe "<p>_foo <a href=\"/url\">bar_</a></p>\n"
  }
  "475 - 7409 - 7413" in {
    renderToHTML("*<img src=\"foo\" title=\"*\"/>\n") shouldBe "<p>*<img src=\"foo\" title=\"*\"/></p>\n"
  }
  "476 - 7416 - 7420" in {
    renderToHTML("**<a href=\"**\">\n") shouldBe "<p>**<a href=\"**\"></p>\n"
  }
  "477 - 7423 - 7427" in {
    renderToHTML("__<a href=\"__\">\n") shouldBe "<p>__<a href=\"__\"></p>\n"
  }
  "478 - 7430 - 7434" in {
    renderToHTML("*a `*`*\n") shouldBe "<p><em>a <code>*</code></em></p>\n"
  }
  "479 - 7437 - 7441" in {
    renderToHTML("_a `_`_\n") shouldBe "<p><em>a <code>_</code></em></p>\n"
  }
  "480 - 7444 - 7448" in {
    renderToHTML("**a<https://foo.bar/?q=**>\n") shouldBe "<p>**a<a href=\"https://foo.bar/?q=**\">https://foo.bar/?q=**</a></p>\n"
  }
  "481 - 7451 - 7455" in {
    renderToHTML("__a<https://foo.bar/?q=__>\n") shouldBe "<p>__a<a href=\"https://foo.bar/?q=__\">https://foo.bar/?q=__</a></p>\n"
  }
