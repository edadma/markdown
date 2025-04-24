package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable

class LinkReferenceDefinitionParserTest extends AnyFlatSpec with Matchers {

  "The link reference definition parser" should "parse basic link references" in {
    val input = """[foo]: /url "title"
[bar]: /url2"""

    val (document, linkRefs) = parseDocumentContentWithRefs(input)

    linkRefs should have size 2
    linkRefs should contain key "foo"
    linkRefs("foo").destination should be("/url")
    linkRefs("foo").title should be(Some("title")) // No quotes
    linkRefs should contain key "bar"
    linkRefs("bar").destination should be("/url2")
    linkRefs("bar").title should be(None)

    // Document should be empty since there are only link references
    document.children should be(empty)
  }

  it should "handle different title syntaxes without including delimiters" in {
    val input = """[foo]: /url "double quoted"
[bar]: /url2 'single quoted'
[baz]: /url3 (parenthesized)"""

    val (_, linkRefs) = parseDocumentContentWithRefs(input)

    linkRefs should have size 3
    linkRefs("foo").title should be(Some("double quoted"))
    linkRefs("bar").title should be(Some("single quoted"))
    linkRefs("baz").title should be(Some("parenthesized"))
  }

  it should "normalize labels" in {
    val input = """[FOO]: /url "title"
[  Bar  ]: /url2"""

    val (_, linkRefs) = parseDocumentContentWithRefs(input)

    linkRefs should have size 2
    linkRefs should contain key "foo"
    linkRefs should contain key "bar"
  }

  it should "handle URLs with angle brackets" in {
    val input = """[foo]: </url with spaces> "title"
[bar]: <https://example.com?q=test>"""

    val (_, linkRefs) = parseDocumentContentWithRefs(input)

    linkRefs should have size 2
    linkRefs("foo").destination should be("/url with spaces")
    linkRefs("bar").destination should be("https://example.com?q=test")
  }

  it should "only use the first definition when multiple exist" in {
    val input = """[foo]: /first "first title"
[foo]: /second "second title"
[FOO]: /third "third title"""

    val (_, linkRefs) = parseDocumentContentWithRefs(input)

    linkRefs should have size 1
    linkRefs("foo").destination should be("/first")
    linkRefs("foo").title should be(Some("first title"))
  }
}
