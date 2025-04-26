package io.github.edadma.markdown

import scala.collection.mutable

object BlockQuoteParser extends BlockParser {
  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.isEmpty) return false

    // A block quote starts with a > character (possibly after up to 3 spaces of indentation)
    val line        = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val indentMatch = "^ {0,3}>.*$".r.matches(line)

    indentMatch
  }

  def parse(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
  ): (Block, Int) = {

    // Collect all lines that belong to this block quote
    val (blockQuoteLines, linesConsumed) = collectBlockQuoteLines(lines)

    // Process the content by removing the > markers
    val processedLines = processBlockQuoteContent(blockQuoteLines)

    // Recursively parse the content as its own document
    val blocks = parseNestedBlocks(processedLines, linkRefs)

    (BlockQuote(blocks), linesConsumed)
  }

  /** Collects all lines that belong to the block quote, handling lazy continuation */
  private def collectBlockQuoteLines(lines: List[LazyList[C]]): (List[LazyList[C]], Int) = {
    var result             = List.empty[LazyList[C]]
    var count              = 0
    var currentLines       = lines
    var inParagraph        = false
    var continueCollecting = true

    while (currentLines.nonEmpty && continueCollecting) {
      val line     = currentLines.head
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      // Check if this line starts with a block quote marker
      val isBlockQuoteLine = "^ {0,3}>.*$".r.matches(lineText)

      if (isBlockQuoteLine) {
        // Line starts with >, add it
        result = result :+ line
        count += 1
        inParagraph = !lineText.matches("^ {0,3}>[ \t]*$") // Track if we're in a paragraph
        currentLines = currentLines.tail
      } else if (isBlankLine(line)) {
        // Blank line: include it if not at the end
        result = result :+ line
        count += 1
        inParagraph = false
        currentLines = currentLines.tail
      } else if (inParagraph) {
        // Lazy continuation: include non-marker line if it continues a paragraph
        result = result :+ line
        count += 1
        currentLines = currentLines.tail
      } else {
        // End of block quote
        continueCollecting = false
      }
    }

    // Handle trailing blank lines (they shouldn't be part of the blockquote)
    val nonBlankLineIndex = result.lastIndexWhere(line =>
      !isBlankLine(line),
    )

    val finalResult = if (nonBlankLineIndex >= 0 && nonBlankLineIndex < result.length - 1) {
      // Remove trailing blank lines
      result.take(nonBlankLineIndex + 1)
    } else {
      result
    }

    (finalResult, count)
  }

  /** Process block quote content by removing the > markers */
  private def processBlockQuoteContent(lines: List[LazyList[C]]): List[LazyList[C]] = {
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

  /** Parse the processed lines recursively */
  private def parseNestedBlocks(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
  ): List[Block] = {
    processLines(lines, linkRefs)
  }

  /** Helper to check if a line is blank */
  private def isBlankLine(line: LazyList[C]): Boolean = {
    line.takeWhile(_.char != '\n').forall(c => c.char == ' ' || c.char == '\t')
  }
}
