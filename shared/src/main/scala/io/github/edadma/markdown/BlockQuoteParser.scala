package io.github.edadma.markdown

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object BlockQuoteParser extends BlockParser {
  val name: String = "block quotes"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) return false

    // A block quote starts with a > character (possibly after up to 3 spaces of indentation)
    val line        = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val indentMatch = "^ {0,3}>.*$".r.matches(line)

    indentMatch
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {

    // Collect all lines that belong to this block quote
    val (blockQuoteLines, linesConsumed) = collectBlockQuoteLines(lines)

    // Process the content by removing the > markers
    val processedLines = processBlockQuoteContent(blockQuoteLines)

    // Recursively parse the content as its own document
    val blocks = processLines(processedLines, linkRefs, parentIndent, config) // maybe wrong: parentIndent

    (BlockQuote(blocks), linesConsumed)
  }

  /** Collects all lines that belong to the block quote, handling lazy continuation */
  private[markdown] def collectBlockQuoteLines(lines: LazyList[List[C]]): (LazyList[List[C]], Int) = {
    var result             = new ListBuffer[List[C]]
    var count              = 0
    var currentLines       = lines
    var inParagraph        = false
    var continueCollecting = true

    while (currentLines.nonEmpty && continueCollecting) {
      val line     = currentLines.head
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      // Check if this line starts with a block quote marker
      val isBlockQuoteLine = "^ {0,3}>.*$".r.matches(lineText)

      // Check if this line is blank (only whitespace)
      val isBlank = lineText.trim.isEmpty

      if (isBlockQuoteLine) {
        // Line starts with >, add it
        result = result :+ line
        count += 1
        // Check if it's a blank line with just a > marker or content
        val contentAfterMarker = lineText.replaceFirst("^ {0,3}>[ \t]?", "")
        inParagraph = contentAfterMarker.trim.nonEmpty // Track if we're in a paragraph
        currentLines = currentLines.tail
      } else if (isBlank) {
        // Blank line without a > marker
        // According to CommonMark spec, this separates blockquotes
        continueCollecting = false
        // Don't increment count here - we don't consume the blank line
      } else if (inParagraph && !couldStartBlock(currentLines)) {
        // Lazy continuation: include non-marker line if it continues a paragraph
        // but not if the line could start a new block construct
        result = result :+ line
        count += 1
        currentLines = currentLines.tail
      } else {
        // End of block quote
        continueCollecting = false
      }
    }

    (LazyList.from(result.toList), count)
  }

  /** Check if lines could start a block-level construct that shouldn't be lazy-continued */
  private def couldStartBlock(lines: LazyList[List[C]]): Boolean = {
    val config = MarkdownConfig.default
    ThematicBreakBlockParser.canStart(lines, config) ||
    ATXHeadingBlockParser.canStart(lines, config) ||
    FencedCodeBlockParser.canStart(lines, config) ||
    HTMLBlockParser.canStart(lines, config) ||
    ListBlockParser.canStart(lines, config)
  }

  /** Process block quote content by removing the > markers */
  private[markdown] def processBlockQuoteContent(lines: LazyList[List[C]]): LazyList[List[C]] = {
    lines.map { line =>
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      if (lineText.matches("^ {0,3}>.*$")) {
        // Remove the > marker and up to one space after it
        val markerPos    = lineText.indexOf('>')
        val contentStart = markerPos + 1
        val adjustedContentStart =
          if (contentStart < lineText.length && lineText(contentStart) == ' ')
            contentStart + 1
          else
            contentStart

        // Create new line with marker removed
        line.drop(adjustedContentStart)
      } else {
        // For lazy continuation lines, keep as is
        line
      }
    }
  }
}
