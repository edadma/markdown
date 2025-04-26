package io.github.edadma.markdown

import scala.collection.mutable

object FencedCodeBlockParser extends BlockParser {

  /** Check if the first line in `lines` can start a fenced code block. A fenced code block starts with a line
    * containing either:
    *   - 3 or more backticks (```)
    *   - 3 or more tildes (~~~) Both can be indented up to 3 spaces.
    */
  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.isEmpty) return false

    val line     = lines.head
    val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

    // Count leading spaces (up to 3) to allow for indentation
    val leadingSpaces = lineText.takeWhile(_ == ' ').length min 3

    // Look for fence pattern after indentation
    val afterIndent = lineText.substring(leadingSpaces)

    // Check for 3+ backticks or 3+ tildes
    (afterIndent.startsWith("```") && afterIndent.takeWhile(_ == '`').length >= 3) ||
    (afterIndent.startsWith("~~~") && afterIndent.takeWhile(_ == '~').length >= 3)
  }

  /** Parse a fenced code block starting at the head of `lines`. Collects all lines until a matching closing fence is
    * found.
    */
  def parse(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
  ): (Block, Int) = {
    if (lines.isEmpty) return (null, 0)

    val firstLine     = lines.head
    val firstLineText = firstLine.takeWhile(_.char != '\n').map(_.char).mkString

    // Determine fence type and properties
    val leadingIndent = firstLineText.takeWhile(_ == ' ').length min 3
    val afterIndent   = firstLineText.substring(leadingIndent)

    // Determine fence character (backtick or tilde)
    val fenceChar   = afterIndent.charAt(0)
    val fenceLength = afterIndent.takeWhile(_ == fenceChar).length

    // Extract info string (after fence, ignoring trailing whitespace)
    val infoString = {
      val info = afterIndent.substring(fenceLength).trim
      if (info.isEmpty) None else Some(info)
    }

    // Process content lines
    val contentBuilder = new StringBuilder
    var lineCount      = 1 // We've already processed the first line
    var currentIndex   = 1 // Start from the second line

    // Track if we've found a closing fence
    var closedBlock = false

    while (currentIndex < lines.length && !closedBlock) {
      val line     = lines(currentIndex)
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      // Check if this is a closing fence (rename variable to avoid conflict with method)
      val foundClosingFence = isClosingFence(lineText, fenceChar, fenceLength, leadingIndent)

      if (foundClosingFence) {
        // Found closing fence - end block
        closedBlock = true
      } else {
        // Regular content line - add to content

        // For the first content line, don't add a newline
        if (lineCount > 1) {
          contentBuilder.append('\n')
        }

        contentBuilder.append(lineText)
      }

      lineCount += 1
      currentIndex += 1
    }

    // Create the code block
    (Code(contentBuilder.toString, infoString), lineCount)
  }

  /** Check if a line is a valid closing fence for a fenced code block
    */
  private def isClosingFence(
      line: String,
      fenceChar: Char,
      minFenceLength: Int,
      openingIndent: Int,
  ): Boolean = {
    // Allow indentation up to 3 spaces plus opening indentation
    val maxIndent     = openingIndent + 3
    val leadingSpaces = line.takeWhile(_ == ' ').length min maxIndent

    // Check if there's a fence after indentation
    if (line.length <= leadingSpaces) return false

    val afterIndent = line.substring(leadingSpaces)

    // Must start with the right fence character
    if (!afterIndent.startsWith(fenceChar.toString)) return false

    // Must be at least as long as opening fence
    val fenceLength = afterIndent.takeWhile(_ == fenceChar).length
    if (fenceLength < minFenceLength) return false

    // After fence must be empty or whitespace
    val afterFence = afterIndent.substring(fenceLength)
    afterFence.trim.isEmpty
  }
}
