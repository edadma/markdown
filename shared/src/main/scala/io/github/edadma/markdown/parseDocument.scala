package io.github.edadma.markdown

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

// Add to Node.scala:
// case class Heading(level: Int, inlines: List[Inline]) extends Block
// case class CodeBlock(content: String, infoString: Option[String] = None) extends Block
// case class BlockQuote(children: List[Block]) extends Block
// case class ThematicBreak() extends Block

// Block parser implementation
def parseDocument(stream: LazyList[C]): (Document, immutable.Map[String, LinkReference]) = {
  val linkRefs      = new mutable.HashMap[String, LinkReference]
  val blocks        = parseBlocks(stream, linkRefs)
  val immutableRefs = linkRefs.toMap // Convert to immutable map

  (Document(blocks.filterNot(_ == null).map(_.processInlines(immutableRefs))), immutableRefs)
}

def extractHeaders(document: Document): List[(Int, String)] = {
  val headers = collection.mutable.ListBuffer[(Int, String)]()

  def visit(node: Node): Unit = node match {
    case Document(children)      => children.foreach(visit)
    case Heading(level, inlines) => headers += ((level, inlinesToPlainText(inlines)))
    case _                       => // Skip other node types
  }

  visit(document)
  headers.toList
}

def inlinesToPlainText(inlines: List[Inline]): String = {
  inlines.map {
    case Text(content)      => content
    case Emphasis(children) => inlinesToPlainText(children)
    case Strong(children)   => inlinesToPlainText(children)
    case _                  => ""
  }.mkString
}

// The main block parsing function that delegates to specific block parsers
private def parseBlocks(
                         stream: LazyList[C],
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
  def canStart(line: List[LazyList[C]]): Boolean

  // Parse a block starting with the given line
  // Returns the parsed block and the number of lines consumed
  def parse(lines: List[LazyList[C]], linkRefs: mutable.Map[String, LinkReference]): (Block, Int)
}

// Process lines to build blocks
private def processLines(
                          lines: List[LazyList[C]],
                          linkRefs: scala.collection.mutable.Map[String, LinkReference],
): List[Block] = {
  val blocks         = new ListBuffer[Block]
  var remainingLines = lines

  // The list of block parsers in priority order
  val blockParsers: List[BlockParser] = List(
    LinkReferenceDefinitionParser,
    SetextHeadingBlockParser,
    ATXHeadingBlockParser,
    ParagraphBlockParser,
  )

  // Process lines until none remain
  while (remainingLines.nonEmpty) {
    // Find a parser for the current line
    blockParsers.find(_.canStart(remainingLines)) match {
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
private def groupIntoLines(stream: LazyList[C]): List[LazyList[C]] = {
  var lines: List[LazyList[C]] = Nil
  var currentLine: List[C]     = Nil

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
