package io.github.edadma.markdown

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TestTest extends AnyFreeSpec with Matchers {

  "89 - 1476 - 1481" in {
    val input = "Foo  \n-----\n"
    val html  = renderToHTML(input)

    html shouldBe "<h2>Foo</h2>\n"
  }

}
