package io.github.edadma.markdown

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

// Add to Node.scala:
// case class Heading(level: Int, inlines: List[Inline]) extends Block
// case class CodeBlock(content: String, infoString: Option[String] = None) extends Block
// case class BlockQuote(children: List[Block]) extends Block
// case class ThematicBreak() extends Block

// Block parser implementation
def parseDocument(stream: LazyList[Cursor]): (Document, immutable.Map[String, LinkReference]) = {
  val linkRefs      = new mutable.HashMap[String, LinkReference]
  val blocks        = parseBlocks(stream, linkRefs)
  val immutableRefs = linkRefs.toMap // Convert to immutable map

  (Document(blocks.filterNot(_ == null).map(_.processInlines(immutableRefs))), immutableRefs)
}

// The main block parsing function that delegates to specific block parsers
private def parseBlocks(
    stream: LazyList[Cursor],
    linkRefs: mutable.Map[String, LinkReference],
): List[Block] = {
  // Group the stream into lines
  val lines = groupIntoLines(stream)

  // Process lines to detect block structures
  processLines(lines, linkRefs)
}

// Interface for block parsers
trait BlockParser {
  // Check if this parser can handle the given line
  def canStart(line: LazyList[Cursor]): Boolean

  // Parse a block starting with the given line
  // Returns the parsed block and the number of lines consumed
  def parse(lines: List[LazyList[Cursor]], linkRefs: mutable.Map[String, LinkReference]): (Block, Int)
}

// Process lines to build blocks
private def processLines(
    lines: List[LazyList[Cursor]],
    linkRefs: scala.collection.mutable.Map[String, LinkReference],
): List[Block] = {
  val blocks         = new ListBuffer[Block]
  var remainingLines = lines

  // The list of block parsers in priority order
  val blockParsers: List[BlockParser] = List(
    LinkReferenceDefinitionParser,
    ParagraphBlockParser, // We'll add more parsers here later
  )

  // Process lines until none remain
  while (remainingLines.nonEmpty) {
    // Find a parser for the current line
    blockParsers.find(_.canStart(remainingLines.head)) match {
      case Some(parser) =>
        // Parse the block and update remaining lines
        val (block, linesConsumed) = parser.parse(remainingLines, linkRefs)
        if (block != null) {
          blocks.addOne(block)
        }
        remainingLines = remainingLines.drop(linesConsumed)

      case None =>
        // Skip unrecognized lines (shouldn't happen in practice)
        remainingLines = remainingLines.tail
    }
  }

  blocks.toList
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
