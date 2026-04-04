package io.github.edadma.markdown

import scala.collection.mutable
import scala.language.postfixOps

object ATXHeadingBlockParser extends BlockParser {
  val name: String = "ATX headings"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) return false

    val line =
      if lines.head.last.char == '\n' then lines.head.dropRight(1)
      else lines.head

    // Skip up to 3 leading spaces
    val leadingSpaces = line.takeWhile(c => c.char == ' ').size
    if (leadingSpaces > 3) return false

    val afterSpaces = line.drop(leadingSpaces)

    // Check if line starts with at least one # followed by a space
    afterSpaces.headOption.exists(c => !c.isLiteral && c.char == '#') && {
      val headingStart = afterSpaces.takeWhile(c => !c.isLiteral && c.char == '#').size

      // Heading level must be 1-6, and must be followed by space/tab or end of line
      headingStart >= 1 && headingStart <= 6 && (
        afterSpaces.size <= headingStart || // End of line
          afterSpaces.drop(headingStart).headOption.exists(c => c.char == ' ' || c.char == '\t')
      )
    }
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    val line = lines.head

    // Skip up to 3 leading spaces
    var pos = 0
    while (pos < line.size && pos < 3 && line(pos).char == ' ') {
      pos += 1
    }

    // Count heading level (1-6 #s)
    var level = 0
    while (pos < line.size && level < 6 && line(pos).char == '#' && !line(pos).isLiteral) {
      level += 1
      pos += 1
    }

    // Skip whitespace after #s
    while (pos < line.size && (line(pos).char == ' ' || line(pos).char == '\t')) {
      pos += 1
    }

    // Extract raw content (excluding newline)
    val contentCursorsReversed = line.drop(pos).takeWhile(c => c.char != '\n').toList.reverse

    // Strip trailing whitespace, then trailing non-escaped #s only if preceded by whitespace
    val afterTrailingWhitespace = contentCursorsReversed.dropWhile(_.char.isWhitespace)
    val contentCursorsTrimmed =
      if (afterTrailingWhitespace.nonEmpty && afterTrailingWhitespace.head.char == '#' && !afterTrailingWhitespace.head.isLiteral) {
        val afterHashes = afterTrailingWhitespace.dropWhile(c => c.char == '#' && !c.isLiteral)
        // Only strip if hashes were preceded by whitespace or content is now empty
        if (afterHashes.isEmpty || afterHashes.head.char.isWhitespace)
          afterHashes.dropWhile(_.char.isWhitespace).reverse
        else
          contentCursorsReversed.dropWhile(_.char.isWhitespace).reverse
      } else {
        afterTrailingWhitespace.reverse
      }

    // Check for trailing attributes {#id .class key=value}
    if (config.attributes) {
      val contentStr = contentCursorsTrimmed.map(_.char).mkString
      val (stripped, attrs) = extractTrailingAttributes(contentStr)
      if (attrs.isDefined) {
        // Rebuild cursor list from stripped content
        val newCursors = contentCursorsTrimmed.take(stripped.length)
        return (Heading(level, newCursors, attrs), 1)
      }
    }

    (Heading(level, contentCursorsTrimmed), 1)
  }
}
