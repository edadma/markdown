package io.github.edadma.markdown

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

def parseDocument(
    stream: LazyList[C],
    config: MarkdownConfig = MarkdownConfig.default,
): (Document, immutable.Map[String, LinkReference]) = {
  val linkRefs      = new mutable.HashMap[String, LinkReference]
  val blocks        = parseBlocks(stream, linkRefs, config)
  val immutableRefs = linkRefs.toMap // Convert to immutable map

  (Document(blocks.filterNot(_ == null).map(_.processInlines(immutableRefs, config))), immutableRefs)
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
    case CodeSpan(content)  => content
    case Emphasis(children) => inlinesToPlainText(children)
    case Strong(children)   => inlinesToPlainText(children)
    case c: C               => c.char.toString
    case _                  => ""
  }.mkString
}

// The main block parsing function that delegates to specific block parsers
private def parseBlocks(
    stream: LazyList[C],
    linkRefs: mutable.Map[String, LinkReference],
    config: MarkdownConfig,
): List[Block] = {
  // Group the stream into lines
  val lines = groupIntoLines(stream)

  // Process lines to detect block structures
  processLines(lines, linkRefs, 0, config)
}

// Interface for block parsers
trait BlockParser {

  val name: String

  /** Can this parser start on the given lines?
    * @param lines
    *   a list of lines, each as a LazyList[C]
    * @return
    *   true if this parser should handle the first line
    */
  def canStart(line: LazyList[List[C]], config: MarkdownConfig): Boolean

  /** Parse a block starting at the head of `lines`.
    *
    * @param lines
    *   the remaining lines in the document
    * @param linkRefs
    *   mutable map of link reference definitions
    * @return
    *   a tuple of the parsed Block and the number of lines consumed
    */
  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int)
}

val blockParsers: ArrayBuffer[BlockParser] = ArrayBuffer(
  LinkReferenceDefinitionParser,
  ThematicBreakBlockParser,
  ListBlockParser,
  SetextHeadingBlockParser,
  ATXHeadingBlockParser,
  HTMLBlockParser,
  TableBlockParser,
  IndentedCodeBlockParser,
  FencedCodeBlockParser,
  BlockQuoteParser,
  ParagraphBlockParser,
)

// Process lines to build blocks
private def processLines(
    lines: LazyList[List[C]],
    linkRefs: mutable.Map[String, LinkReference],
    parentIndent: Int,
    config: MarkdownConfig,
): List[Block] = {
  val blocks         = new ListBuffer[Block]
  var remainingLines = lines

  // Process lines until none remain
  while (remainingLines.nonEmpty) {
    // Find a parser for the current line
    blockParsers.find(_.canStart(remainingLines, config)) match {
      case Some(parser) =>
        // Parse the block and update remaining lines
        val (block, linesConsumed) = parser.parse(remainingLines, linkRefs, parentIndent, config)
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
// Group cursor stream into lines - now using LazyList for the sequence of lines
private def groupIntoLines(stream: LazyList[C]): LazyList[List[C]] = {
  // Use LazyList.unfold to create a lazy sequence of lines
  LazyList.unfold(stream) { remaining =>
    if (remaining.isEmpty || remaining.head == EndOfInput) {
      None // End of stream
    } else {
      // Collect characters up to and including the next newline
      val (lineChars, rest) = remaining.span(c => c != EndOfInput && c.char != '\n')

      // Include the newline in the current line if present
      val (line, nextRemaining) =
        if (rest.nonEmpty && rest.head.char == '\n') {
          (lineChars.toList :+ rest.head, rest.tail)
        } else {
          (lineChars.toList, rest)
        }

      // Return the current line and the remaining stream
      Some((line, nextRemaining))
    }
  }
}
