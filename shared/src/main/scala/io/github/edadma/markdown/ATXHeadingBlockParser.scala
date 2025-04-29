package io.github.edadma.markdown

import scala.collection.mutable
import scala.language.postfixOps

object ATXHeadingBlockParser extends BlockParser {
  val name: String = "ATX headings"

  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.isEmpty) return false

    val line =
      if lines.head.last.char == '\n' then lines.head.dropRight(1)
      else lines.head

    // Check if line starts with at least one # followed by a space
    line.headOption.exists(c => !c.isLiteral && c.char == '#') && {
      val headingStart = line.takeWhile(c => !c.isLiteral && c.char == '#').size

      // Heading level must be 1-6, and must be followed by space/tab or end of line
      headingStart >= 1 && headingStart <= 6 && (
        line.size <= headingStart || // End of line
          line.drop(headingStart).headOption.exists(c => c.char == ' ' || c.char == '\t')
      )
    }
  }

  def parse(lines: List[LazyList[C]], linkRefs: mutable.Map[String, LinkReference]): (Block, Int) = {
    val line = lines.head

    // Count heading level (1-6 #s)
    var level = 0
    var pos   = 0

    // Count leading #s (up to 6)
    while (pos < line.size && pos < 6 && line(pos).char == '#' && !line(pos).isLiteral) {
      level += 1
      pos += 1
    }

    // Skip whitespace after #s
    while (pos < line.size && (line(pos).char == ' ' || line(pos).char == '\t')) {
      pos += 1
    }

    // Extract raw content (excluding newline)
    val contentCursorsReversed = line.drop(pos).takeWhile(c => c.char != '\n').toList.reverse

    val contentCursorsTrimmed =
      contentCursorsReversed dropWhile (_.char.isWhitespace) dropWhile (_.char == '#') dropWhile (_.char.isWhitespace) reverse

    (Heading(level, contentCursorsTrimmed), 1)
  }
}
