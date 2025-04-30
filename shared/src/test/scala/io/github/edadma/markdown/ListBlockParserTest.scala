package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListBlockParserTest extends AnyFlatSpec with Matchers {

  "The list block parser" should "parse a simple unordered list" in {
    val input = """- Item 1
                  |- Item 2
                  |- Item 3""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("Item 1"))))),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
          ListItem(List(Paragraph(List(Text("Item 3"))))),
        ),
      ),
    ))
  }

  it should "parse a simple ordered list" in {
    val input = """1. First item
                  |2. Second item
                  |3. Third item""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = true, startNumber = Some(1), delimiter = Some('.'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("First item"))))),
          ListItem(List(Paragraph(List(Text("Second item"))))),
          ListItem(List(Paragraph(List(Text("Third item"))))),
        ),
      ),
    ))
  }

  it should "parse lists with different marker styles" in {
    val input = """* Asterisk item
                  |+ Plus item
                  |- Hyphen item
                  |
                  |1) Parenthesis item
                  |2) Another item""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(
      children = List(
        ListBlock(
          data = ListData(
            isOrdered = false,
            bulletChar = Some(value = '*'),
            startNumber = None,
            delimiter = None,
            isTight = true,
            indent = 0,
          ),
          items = List(
            ListItem(content = List(Paragraph(inlines = List(Text(content = "Asterisk item"))))),
          ),
        ),
        ListBlock(
          data = ListData(
            isOrdered = false,
            bulletChar = Some(value = '+'),
            startNumber = None,
            delimiter = None,
            isTight = true,
            indent = 0,
          ),
          items = List(ListItem(content = List(Paragraph(inlines = List(Text(content = "Plus item")))))),
        ),
        ListBlock(
          data = ListData(
            isOrdered = false,
            bulletChar = Some(value = '-'),
            startNumber = None,
            delimiter = None,
            isTight = true,
            indent = 0,
          ),
          items = List(
            ListItem(
              content = List(Paragraph(inlines = List(Text(content = "Hyphen item")))),
            ),
          ),
        ),
        ListBlock(
          data = ListData(
            isOrdered = true,
            bulletChar = None,
            startNumber = Some(value = 1),
            delimiter = Some(value = ')'),
            isTight = true,
            indent = 0,
          ),
          items = List(
            ListItem(content = List(Paragraph(inlines = List(Text(content = "Parenthesis item"))))),
            ListItem(content = List(Paragraph(inlines = List(Text(content = "Another item"))))),
          ),
        ),
      ),
    )
  }

  it should "handle indented list items" in {
    val input = """  - Item with two spaces indentation
                  |   - Item with three spaces indentation""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 2),
        List(
          ListItem(List(Paragraph(List(Text("Item with two spaces indentation"))))),
          ListItem(List(Paragraph(List(Text("Item with three spaces indentation"))))),
        ),
      ),
    ))
  }

  it should "parse a loose list (with blank lines between items)" in {
    val input = """- Item 1
                  |
                  |- Item 2
                  |
                  |- Item 3""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = false, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("Item 1"))))),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
          ListItem(List(Paragraph(List(Text("Item 3"))))),
        ),
      ),
    ))
  }

  it should "handle multi-line items in a tight list" in {
    val input = """- Item 1
                  |  continued
                  |- Item 2
                  |  also continued""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("Item 1"), SoftLineBreak(), Text("continued"))))),
          ListItem(List(Paragraph(List(Text("Item 2"), SoftLineBreak(), Text("also continued"))))),
        ),
      ),
    ))
  }

  it should "handle multi-paragraph items (making the list loose)" in {
    val input = """- Item 1
                  |
                  |  Second paragraph of item 1
                  |- Item 2""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = false, indent = 0),
        List(
          ListItem(List(
            Paragraph(List(Text("Item 1"))),
            Paragraph(List(Text("Second paragraph of item 1"))),
          )),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
        ),
      ),
    ))
  }

  it should "parse nested lists" in {
    val input = """- Item 1
                  |  - Nested 1.1
                  |  - Nested 1.2
                  |- Item 2""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(
            Paragraph(List(Text("Item 1"))),
            ListBlock(
              ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 2),
              List(
                ListItem(List(Paragraph(List(Text("Nested 1.1"))))),
                ListItem(List(Paragraph(List(Text("Nested 1.2"))))),
              ),
            ),
          )),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
        ),
      ),
    ))
  }

  it should "handle nested mixed list types" in {
    val input = """- Unordered item
                  |  1. Nested ordered 1
                  |  2. Nested ordered 2
                  |     - Deeply nested unordered""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(
            Paragraph(List(Text("Unordered item"))),
            ListBlock(
              ListData(isOrdered = true, startNumber = Some(1), delimiter = Some('.'), isTight = true, indent = 2),
              List(
                ListItem(List(Paragraph(List(Text("Nested ordered 1"))))),
                ListItem(List(
                  Paragraph(List(Text("Nested ordered 2"))),
                  ListBlock(
                    ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 5),
                    List(
                      ListItem(List(Paragraph(List(Text("Deeply nested unordered"))))),
                    ),
                  ),
                )),
              ),
            ),
          )),
        ),
      ),
    ))
  }

  it should "handle code blocks in list items" in {
    val input = """- Item with code
                  |
                  |  ```
                  |  function() {
                  |    return true;
                  |  }
                  |  ```
                  |
                  |- Another item""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = false, indent = 0),
        List(
          ListItem(List(
            Paragraph(List(Text("Item with code"))),
            Code("function() {\n  return true;\n}", None),
          )),
          ListItem(List(Paragraph(List(Text("Another item"))))),
        ),
      ),
    ))
  }

  it should "handle ordered lists with arbitrary start numbers" in {
    val input = """42. Item starting at 42
                  |43. Next item""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = true, startNumber = Some(42), delimiter = Some('.'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("Item starting at 42"))))),
          ListItem(List(Paragraph(List(Text("Next item"))))),
        ),
      ),
    ))
  }

  it should "handle list items with hard line breaks" in {
    val input = """- Item with hard\
                  |  break
                  |- Second item""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(Text("Item with hard"), HardLineBreak(), Text("break"))))),
          ListItem(List(Paragraph(List(Text("Second item"))))),
        ),
      ),
    ))
  }

  it should "handle lists with blockquotes inside" in {
    val input = """- Item 1
                  |
                  |  > This is a blockquote
                  |  > inside a list item
                  |
                  |- Item 2""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = false, indent = 0),
        List(
          ListItem(List(
            Paragraph(List(Text("Item 1"))),
            BlockQuote(List(
              Paragraph(List(
                Text("This is a blockquote"),
                SoftLineBreak(),
                Text("inside a list item"),
              )),
            )),
          )),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
        ),
      ),
    ))
  }

  it should "treat lazy continuation lines as part of the list item" in {
    val input = """- Item 1
                  |continuation without proper indentation
                  |- Item 2""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      ListBlock(
        ListData(isOrdered = false, bulletChar = Some('-'), isTight = true, indent = 0),
        List(
          ListItem(List(Paragraph(List(
            Text("Item 1"),
            SoftLineBreak(),
            Text("continuation without proper indentation"),
          )))),
          ListItem(List(Paragraph(List(Text("Item 2"))))),
        ),
      ),
    ))
  }

//  it should "handle list items with thematic breaks" in {
//    val input = """- Item 1
//                     |
//                     |  ---
//                     |
//                     |  After the break
//                     |- Item 2""".stripMargin
//    val document = parseDocumentContent(input)
//
//    document shouldBe Document(List(
//      ListBlock(
//        ListData(isOrdered = false, bulletChar = Some('-'), isTight = false),
//        List(
//          ListItem(List(
//            Paragraph(List(Text("Item 1"), SoftLineBreak())),
//            ThematicBreak(),
//            Paragraph(List(Text("After the break"))),
//          )),
//          ListItem(List(Paragraph(List(Text("Item 2"))))),
//        ),
//      ),
//    ))
//  }
}
