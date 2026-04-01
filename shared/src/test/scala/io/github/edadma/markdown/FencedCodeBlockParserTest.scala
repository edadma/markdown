package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FencedCodeBlockParserTest extends AnyFlatSpec with Matchers {

  "The fenced code block parser" should "parse a simple backtick fenced code block" in {
    val input = """```
                  |This is a code block.
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This is a code block.", None),
    ))
  }

  it should "parse a simple tilde fenced code block" in {
    val input = """~~~
                  |This is a code block.
                  |~~~""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This is a code block.", None),
    ))
  }

  it should "parse a code block with language info" in {
    val input = """```scala
                  |def hello(): Unit = {
                  |  println("Hello, world!")
                  |}
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code(
        """def hello(): Unit = {
  println("Hello, world!")
}""",
        Some("scala"),
      ),
    ))
  }

  it should "handle fences with more than 3 characters" in {
    val input = """``````
                  |This fence has 6 backticks
                  |``````""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This fence has 6 backticks", None),
    ))
  }

  it should "allow closing fence with more characters than opening fence" in {
    val input = """```
                  |This block's closing fence is longer
                  |`````""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This block's closing fence is longer", None),
    ))
  }

  it should "preserve indentation in fenced code blocks" in {
    val input = """```
                  |    This line has 4 spaces
                  |  This line has 2 spaces
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code(
        """    This line has 4 spaces
  This line has 2 spaces""",
        None,
      ),
    ))
  }

  it should "allow fenced code blocks to be indented up to 3 spaces" in {
    val input = """   ```
                  |   This block is indented with 3 spaces
                  |   ```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This block is indented with 3 spaces", None),
    ))
  }

  it should "allow closing fence to be indented more than opening fence" in {
    val input = """```
                  |code
                  |   ```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("code", None),
    ))
  }

  it should "not allow closing fence to have different character than opening fence" in {
    val input = """```
                  |Mixed fences don't match
                  |~~~""".stripMargin
    val document = parseDocumentContent(input)

    // Should continue until end of document because there's no matching fence
    document shouldBe Document(List(
      Code("Mixed fences don't match\n~~~", None),
    ))
  }

  it should "handle fenced code blocks interrupted by end of document" in {
    val input = """```
                  |This block has no closing fence""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This block has no closing fence", None),
    ))
  }

  it should "handle multi-line fenced code blocks with blank lines" in {
    val input = """```
                  |Line 1
                  |
                  |Line 3
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code(
        """Line 1

Line 3""",
        None,
      ),
    ))
  }

  it should "handle code with backticks inside fenced blocks" in {
    val input = """```
                  |This contains `inline code` backticks
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code("This contains `inline code` backticks", None),
    ))
  }

  it should "handle code blocks with complex language specifiers" in {
    val input = """```java highlight-line=3
                  |public class Test {
                  |  public static void main(String[] args) {
                  |    System.out.println("Hello, world!");
                  |  }
                  |}
                  |```""".stripMargin
    val document = parseDocumentContent(input)

    document shouldBe Document(List(
      Code(
        """public class Test {
  public static void main(String[] args) {
    System.out.println("Hello, world!");
  }
}""",
        Some("java"),
      ),
    ))
  }
}
