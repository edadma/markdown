package io.github.edadma.markdown

import scala.collection.mutable

object IndentedCodeBlockParser extends BlockParser {
  val name: String = "indented code blocks"

  /** Check if the first line in `lines` can start an indented code block. An indented code block starts with a line
    * indented with at least 4 spaces (or 1 tab) and cannot interrupt a paragraph.
    */
  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) return false

    val line = lines.head

    // Check if line is indented by at least 4 spaces/1 tab
    val virtualIndent = countVirtualIndent(line)

    // Line must have at least 4 spaces of indent and not be blank after removing indent
    virtualIndent >= 4 && !isBlankAfterIndent(line, virtualIndent)
  }

  /** Parse an indented code block starting at the head of `lines`. Collects all indented lines until a non-indented,
    * non-blank line is found.
    */
  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    val contentBuilder = new StringBuilder
    var lineCount      = 0
    var currentLines   = lines
    var inCodeBlock    = true

    // Track the content and pending blank lines separately
    var hasContent        = false
    var pendingBlankLines = 0

    // Process lines until we find one that's not part of the code block
    while (inCodeBlock && currentLines.nonEmpty) {
      val line = currentLines.head

      if (isBlankLine(line)) {
        // Don't append blank line yet - track it as pending
        pendingBlankLines += 1
        lineCount += 1
        currentLines = currentLines.tail
      } else {
        // Check if line has enough indentation
        val virtualIndent = countVirtualIndent(line)

        if (virtualIndent >= 4) {
          // This is a content line - first add any pending blank lines
          for (_ <- 0 until pendingBlankLines) {
            if (hasContent) {
              contentBuilder.append("\n")
            }
          }
          pendingBlankLines = 0

          // Add newline between content lines (if not the first content line)
          if (hasContent) {
            contentBuilder.append("\n")
          }

          // Remove exactly 4 spaces of indentation
          val contentChars = removeIndent(line, 4)
          contentBuilder.append(contentChars)

          hasContent = true
          lineCount += 1
          currentLines = currentLines.tail
        } else {
          // Not enough indentation, code block ends
          inCodeBlock = false
        }
      }
    }

    // Check if we found any actual code content
    if (!hasContent) {
      // No actual code content was found
      return (null, 0)
    }

    (Code(contentBuilder.toString, None, indented = true), lineCount)
  }

  /** Count the virtual indent (in spaces) at the beginning of a line
    */
  private def countVirtualIndent(line: List[C]): Int = {
    var virtualCol = 0
    var i          = 0

    while (i < line.size && (line(i).char == ' ' || line(i).char == '\t')) {
      if (line(i).char == ' ') {
        virtualCol += 1
      } else if (line(i).char == '\t') {
        // Tab advances to the next tab stop (multiples of 4)
        virtualCol = (virtualCol + 4) & ~3 // Equivalent to: virtualCol + (4 - (virtualCol % 4))
      }
      i += 1
    }

    virtualCol
  }

  /** Remove exactly n spaces worth of indentation from a line Returns the remainder of the line as a string
    */
  private def removeIndent(line: List[C], spacesToRemove: Int): String = {
    var virtualCol = 0
    var i          = 0

    // Skip characters until we've removed the required spaces
    while (i < line.size && virtualCol < spacesToRemove) {
      if (line(i).char == ' ') {
        virtualCol += 1
      } else if (line(i).char == '\t') {
        // Tab advances to the next tab stop
        virtualCol = (virtualCol + 4) & ~3
      }
      i += 1
    }

    // Return the rest of the line (excluding newline)
    line.drop(i).takeWhile(_.char != '\n').map(_.char).mkString
  }

  /** Check if a line is blank (contains only whitespace or is empty)
    */
  private def isBlankLine(line: List[C]): Boolean = {
    line.takeWhile(_.char != '\n').forall(c => c.char == ' ' || c.char == '\t')
  }

  /** Check if a line would be blank after removing the given indent
    */
  private def isBlankAfterIndent(line: List[C], indent: Int): Boolean = {
    var virtualCol = 0
    var i          = 0

    // Skip characters until we've covered the indentation
    while (i < line.size && virtualCol < indent) {
      if (line(i).char == ' ') {
        virtualCol += 1
      } else if (line(i).char == '\t') {
        virtualCol = (virtualCol + 4) & ~3
      }
      i += 1
    }

    // Check if the remainder of the line (up to newline) is empty
    line.drop(i).takeWhile(_.char != '\n').isEmpty
  }
}
