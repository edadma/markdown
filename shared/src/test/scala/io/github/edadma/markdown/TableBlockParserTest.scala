package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableBlockParserTest extends AnyFlatSpec with Matchers {
  private val config = MarkdownConfig.withExtensions(tables = true)

  "The table block parser" should "parse a basic table" in {
    val input = """| Header 1 | Header 2 |
                   || -------- | -------- |
                   || Cell 1   | Cell 2   |
                   || Cell 3   | Cell 4   |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
        )),
        List(
          TableRow(List(
            TableCell(List(Text("Cell 1"))),
            TableCell(List(Text("Cell 2"))),
          )),
          TableRow(List(
            TableCell(List(Text("Cell 3"))),
            TableCell(List(Text("Cell 4"))),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "handle tables without leading and trailing pipes" in {
    val input = """Header 1 | Header 2
                  |--------- | ---------
                  |Cell 1 | Cell 2
                  |Cell 3 | Cell 4""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
        )),
        List(
          TableRow(List(
            TableCell(List(Text("Cell 1"))),
            TableCell(List(Text("Cell 2"))),
          )),
          TableRow(List(
            TableCell(List(Text("Cell 3"))),
            TableCell(List(Text("Cell 4"))),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "parse tables with alignment indicators" in {
    val input = """| Left | Center | Right | Default |
                   || :--- | :----: | ----: | ------- |
                   || L1   | C1     | R1    | D1      |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Left"))),
          TableCell(List(Text("Center"))),
          TableCell(List(Text("Right"))),
          TableCell(List(Text("Default"))),
        )),
        List(
          TableRow(List(
            TableCell(List(Text("L1"))),
            TableCell(List(Text("C1"))),
            TableCell(List(Text("R1"))),
            TableCell(List(Text("D1"))),
          )),
        ),
        List(TableAlignment.Left, TableAlignment.Center, TableAlignment.Right, TableAlignment.None),
      ),
    ))
  }

  it should "handle tables with inline formatting in cells" in {
    val input = """| *Emphasized* | **Strong** |
                   || ------------ | ---------- |
                   || `Code`       | [Link](url) |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Emphasis(List(Text("Emphasized"))))),
          TableCell(List(Strong(List(Text("Strong"))))),
        )),
        List(
          TableRow(List(
            TableCell(List(CodeSpan("Code"))),
            TableCell(List(Link("url", None, List(Text("Link"))))),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "handle tables with empty cells" in {
    val input = """| Header 1 | Header 2 |
                   || -------- | -------- |
                   ||          | Cell 2   |
                   || Cell 3   |          |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
        )),
        List(
          TableRow(List(
            TableCell(List()),
            TableCell(List(Text("Cell 2"))),
          )),
          TableRow(List(
            TableCell(List(Text("Cell 3"))),
            TableCell(List()),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "handle tables with mismatched column counts" in {
    val input = """| Header 1 | Header 2 | Header 3 |
                   || -------- | -------- | -------- |
                   || Row 1 Only One Cell |
                   || Cell 1 | Cell 2 | Cell 3 | Extra |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
          TableCell(List(Text("Header 3"))),
        )),
        List(
          TableRow(List(
            TableCell(List(Text("Row 1 Only One Cell"))),
          )),
          TableRow(List(
            TableCell(List(Text("Cell 1"))),
            TableCell(List(Text("Cell 2"))),
            TableCell(List(Text("Cell 3"))),
            TableCell(List(Text("Extra"))),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "handle tables with just a header row" in {
    val input = """| Header 1 | Header 2 |
                   || -------- | -------- |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
        )),
        List(),
        List(TableAlignment.None, TableAlignment.None),
      ),
    ))
  }

  it should "not parse a table with invalid delimiter row" in {
    val input = """
                  || Header 1 | Header 2 |
                  || Not a proper delimiter |
                  || Cell 1   | Cell 2   |""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Paragraph(List(
        Text("| Header 1 | Header 2 |"),
        SoftLineBreak(),
        Text("| Not a proper delimiter |"),
        SoftLineBreak(),
        Text("| Cell 1   | Cell 2   |"),
      )),
    ))
  }

  it should "handle tables adjacent to other block elements" in {
    val input = """Paragraph before.

                  || Header 1 | Header 2 |
                  || -------- | -------- |
                  || Cell 1   | Cell 2   |

                  |Paragraph after.""".stripMargin
    val document = parseDocumentContent(input, config)

    document shouldBe Document(List(
      Paragraph(List(Text("Paragraph before."))),
      Table(
        TableRow(List(
          TableCell(List(Text("Header 1"))),
          TableCell(List(Text("Header 2"))),
        )),
        List(
          TableRow(List(
            TableCell(List(Text("Cell 1"))),
            TableCell(List(Text("Cell 2"))),
          )),
        ),
        List(TableAlignment.None, TableAlignment.None),
      ),
      Paragraph(List(Text("Paragraph after."))),
    ))
  }

  it should "ignore tables when the feature is disabled" in {
    val input =
      """| Header 1 | Header 2 |
         |---|---|
         | Cell 1 | Cell 2 |""".stripMargin

    // Use default config (tables disabled)
    val config   = MarkdownConfig.default
    val document = parseDocumentContent(input, config)

    // Should be parsed as a paragraph instead of a table
    document.children.head shouldBe a[Paragraph]
  }
}
