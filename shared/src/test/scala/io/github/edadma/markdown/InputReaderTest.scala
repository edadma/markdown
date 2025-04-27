package io.github.edadma.markdown

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputReaderTest extends AnyFlatSpec with Matchers {

  "InputReader" should "process basic characters correctly" in {
    val input  = "abc"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1) // Drop EndOfInput

    stream should have length 3
    stream(0).char should be('a')
    stream(0).pos should be(0)
    stream(0).line should be(0)
    stream(0).column should be(0)
    stream(0).isLiteral should be(false)

    stream(1).char should be('b')
    stream(2).char should be('c')
  }

  it should "normalize line endings" in {
    val input  = "a\nb\r\nc\rd"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1)

    stream should have length 7
    stream(1).char should be('\n')
    stream(3).char should be('\n')
    stream(5).char should be('\n')

    // Check line numbers
    stream(0).line should be(0) // a
    stream(2).line should be(1) // b
    stream(4).line should be(2) // c
    stream(6).line should be(3) // d
  }

  it should "replace null characters with replacement character" in {
    val input  = "a\u0000b"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1)

    stream should have length 3
    stream(1).char should be('\uFFFD')
  }

  it should "handle backslash escapes correctly" in {
    val input  = "\\*\\!\\[\\]\\(\\)"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1)

    stream.foreach(c => c.isLiteral should be(true))
    stream.map(_.char).mkString should be("*![]()")
  }

  it should "not escape non-punctuation characters" in {
    val input  = "\\a\\1"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1)

    stream should have length 4
    stream(0).char should be('\\')
    stream(0).isLiteral should be(false)
    stream(1).char should be('a')
    stream(1).isLiteral should be(false)
  }

  it should "preserve tabs in the base stream" in {
    val input  = "a\tb"
    val reader = new InputReader(input)
    val stream = reader.stream.toList.dropRight(1)

    stream should have length 3
    stream(1).char should be('\t')
  }

  it should "expand tabs correctly in block structure contexts" in {
    val input          = "a\tb"
    val reader         = new InputReader(input)
    val expandedStream = reader.getStreamWithExpandedTabs().toList.dropRight(1)

    // Should be: 'a' + 3 spaces (tab at column 1) + 'b'
    expandedStream should have length 5
    expandedStream(0).char should be('a')
    expandedStream.slice(1, 4).foreach(_.char should be(' '))
    expandedStream(4).char should be('b')
  }

  it should "handle multiple tab expansion correctly" in {
    val input          = "\t\ta"
    val reader         = new InputReader(input)
    val expandedStream = reader.getStreamWithExpandedTabs().toList.dropRight(1)

    // Should be: 8 spaces (two tabs at columns 0 and 4) + 'a'
    expandedStream should have length 9
    expandedStream.take(8).foreach(_.char should be(' '))
    expandedStream(8).char should be('a')
  }

  it should "reset tab expansion after newlines" in {
    val input          = "abc\n\td"
    val reader         = new InputReader(input)
    val expandedStream = reader.getStreamWithExpandedTabs().toList.dropRight(1)

    // Should be: 'abc' + '\n' + 4 spaces + 'd'
    expandedStream should have length 9
    expandedStream(3).char should be('\n')
    expandedStream.slice(4, 8).foreach(_.char should be(' '))
    expandedStream(8).char should be('d')
  }

  it should "handle tab expansion at different column positions" in {
    val cases = Map(
      // String -> expected column after tab
      "a\tb"     -> 4,
      "abc\td"   -> 4,
      "abcd\te"  -> 8,
      "abcde\tf" -> 8,
    )

    for ((input, expectedCol) <- cases) {
      val reader   = new InputReader(input)
      val lastChar = reader.stream.dropRight(1).last

      withClue(s"For input '$input': ") {
        lastChar.column should be(expectedCol)
      }
    }
  }

  it should "preserve isLiteral flag through tab expansion" in {
    val input          = "\\*\t"
    val reader         = new InputReader(input)
    val expandedStream = reader.getStreamWithExpandedTabs().toList.dropRight(1)

    // First character should be literal '*'
    expandedStream(0).char should be('*')
    expandedStream(0).isLiteral should be(true)

    // The spaces from the expanded tab should not be literal
    expandedStream.tail.foreach(c => c.isLiteral should be(false))
  }
}

/*

  it should "handle entity references correctly" in {
    val input  = "&amp; &lt; &gt; &quot; &apos;"
    val reader = new InputReader(input)
    val stream = reader.stream.filterNot(_.char == ' ').toList.dropRight(1)

    stream.map(_.char).mkString should be("&<>\"'")
  }

  it should "handle numeric character references correctly" in {
    val input  = "&#65; &#x41;"
    val reader = new InputReader(input)
    val stream = reader.stream.filterNot(_.char == ' ').toList.dropRight(1)

    stream should have length 2
    stream(0).char should be('A') // Decimal 65
    stream(1).char should be('A') // Hex 41
  }
 */
