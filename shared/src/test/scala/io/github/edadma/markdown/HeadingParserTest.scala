package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingParserTest extends AnyFlatSpec with Matchers {

  "The heading parser" should "parse ATX headings with levels 1-6" in {
    val input    = """# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6"""
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading 1"))),
      Heading(2, List(Text("Heading 2"))),
      Heading(3, List(Text("Heading 3"))),
      Heading(4, List(Text("Heading 4"))),
      Heading(5, List(Text("Heading 5"))),
      Heading(6, List(Text("Heading 6"))),
    ))
  }

  it should "handle ATX headings with leading and trailing spaces" in {
    val input = """#  Heading with leading spaces
##    Heading with more spaces   """

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading with leading spaces"))),
      Heading(2, List(Text("Heading with more spaces"))),
    ))
  }

  it should "handle ATX headings with trailing hash marks" in {
    val input = """# Heading 1 #
## Heading 2 ##"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading 1"))),
      Heading(2, List(Text("Heading 2"))),
    ))
  }

  it should "handle empty ATX headings" in {
    val input = """#
##
###     """

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List()),
      Heading(2, List()),
      Heading(3, List()),
    ))
  }

  it should "not treat escaped hash characters as headings" in {
    val input = """\# Not a heading
This is \## not a heading either"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(
        Text("# Not a heading"),
        SoftLineBreak(),
        Text("This is ## not a heading either"),
      )),
    ))
  }

  it should "require a space after the hash for ATX headings" in {
    val input = """#Not a heading
##Also not a heading
### This is a heading"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(
        Text("#Not a heading"),
        SoftLineBreak(),
        Text("##Also not a heading"),
        SoftLineBreak(),
      )),
      Heading(3, List(Text("This is a heading"))),
    ))
  }

  it should "parse Setext headings with equals signs as level 1" in {
    val input = """Heading 1
=========="""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading 1"))),
    ))
  }

  it should "parse Setext headings with hyphens as level 2" in {
    val input = """Heading 2
----------"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(2, List(Text("Heading 2"))),
    ))
  }

  it should "handle Setext headings with varying underline lengths" in {
    val input = """Heading 1
=

Heading 2
-"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading 1"))),
      Heading(2, List(Text("Heading 2"))),
    ))
  }

  it should "handle Setext headings with trailing whitespace in underlines" in {
    val input = """Heading 1
=======

Heading 2
-----     """

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("Heading 1"))),
      Heading(2, List(Text("Heading 2"))),
    ))
  }

  it should "work with mixed heading styles in the same document" in {
    val input = """# ATX Heading 1

Setext Heading 1
===============

## ATX Heading 2

Setext Heading 2
---------------"""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Heading(1, List(Text("ATX Heading 1"))),
      Heading(1, List(Text("Setext Heading 1"))),
      Heading(2, List(Text("ATX Heading 2"))),
      Heading(2, List(Text("Setext Heading 2"))),
    ))
  }

  it should "handle headings adjacent to paragraphs" in {
    val input = """Regular paragraph.

Heading 1
=========

Another paragraph.

# ATX heading

Final paragraph."""

    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Paragraph(List(Text("Regular paragraph."), SoftLineBreak())),
      Heading(1, List(Text("Heading 1"))),
      Paragraph(List(Text("Another paragraph."), SoftLineBreak())),
      Heading(1, List(Text("ATX heading"))),
      Paragraph(List(Text("Final paragraph."))),
    ))
  }
}
