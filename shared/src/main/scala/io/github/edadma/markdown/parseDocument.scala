package io.github.edadma.markdown

import scala.collection.mutable.ArrayBuffer

// Add to Node.scala:
// case class Heading(level: Int, inlines: List[Inline]) extends Block
// case class CodeBlock(content: String, infoString: Option[String] = None) extends Block
// case class BlockQuote(children: List[Block]) extends Block
// case class ThematicBreak() extends Block

// Block parser implementation
def parseDocument(stream: LazyList[Cursor]): Document = Document(parseBlocks(stream)).processInlines

// The main block parsing function that delegates to specific block parsers
private def parseBlocks(stream: LazyList[Cursor]): List[Block] = {
  // Group the stream into lines
  val lines = groupIntoLines(stream)

  // Process lines to detect block structures
  processLines(lines)
}

// Interface for block parsers
trait BlockParser {
  // Check if this parser can handle the given line
  def canStart(line: LazyList[Cursor]): Boolean

  // Parse a block starting with the given line
  // Returns the parsed block and the number of lines consumed
  def parse(lines: List[LazyList[Cursor]]): (Block, Int)
}

// Process lines to build blocks
private def processLines(lines: List[LazyList[Cursor]]): List[Block] = {
  var blocks: List[Block] = Nil
  var remainingLines      = lines

  // The list of block parsers in priority order
  val blockParsers: List[BlockParser] = List(
    ParagraphBlockParser, // We'll add more parsers here later
  )

  // Process lines until none remain
  while (remainingLines.nonEmpty) {
    // Find a parser for the current line
    blockParsers.find(_.canStart(remainingLines.head)) match {
      case Some(parser) =>
        // Parse the block and update remaining lines
        val (block, linesConsumed) = parser.parse(remainingLines)
        blocks = blocks :+ block
        remainingLines = remainingLines.drop(linesConsumed)

      case None =>
        // Skip unrecognized lines (shouldn't happen in practice)
        remainingLines = remainingLines.tail
    }
  }

  blocks
}

// A parser for paragraph blocks - our first concrete implementation
object ParagraphBlockParser extends BlockParser {
  def canStart(line: LazyList[Cursor]): Boolean = {
    // A paragraph can start with any non-blank line
    !isBlankLine(line)
  }

  def parse(lines: List[LazyList[Cursor]]): (Block, Int) = {
    // Find the first blank line
    val paragraphLines = lines.takeWhile(line => !isBlankLine(line))

    // The actual number of lines consumed is the paragraph plus the blank line
    val linesConsumed = if (paragraphLines.length < lines.length) {
      paragraphLines.length + 1 // Include the blank line that terminated the paragraph
    } else {
      paragraphLines.length // The paragraph runs to the end
    }

    (Paragraph(paragraphLines.flatten), linesConsumed)
  }
}

// Function to check if a line is blank
private def isBlankLine(line: LazyList[Cursor]): Boolean = {
  // A blank line contains only whitespace or is empty (excluding newline)
  val contentChars = line.filter(_.char != '\n')
  contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
}

// Group cursor stream into lines
private def groupIntoLines(stream: LazyList[Cursor]): List[LazyList[Cursor]] = {
  var lines: List[LazyList[Cursor]] = Nil
  var currentLine: List[Cursor]     = Nil

  // Process each cursor
  stream.foreach { cursor =>
    if (cursor == EndOfInput) {
      // End of input - add final line if not empty
      if (currentLine.nonEmpty) {
        lines = LazyList.from(currentLine.reverse) :: lines
      }
    } else if (cursor.char == '\n') {
      // End of line - add current line (including the newline) and start a new one
      currentLine = cursor :: currentLine
      lines = LazyList.from(currentLine.reverse) :: lines
      currentLine = Nil
    } else {
      // Add to current line
      currentLine = cursor :: currentLine
    }
  }

  // Return lines in original order
  lines.reverse
}
