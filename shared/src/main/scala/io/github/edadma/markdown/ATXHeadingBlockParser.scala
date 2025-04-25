package io.github.edadma.markdown

import scala.collection.mutable

object ATXHeadingBlockParser extends BlockParser {
  def canStart(lines: List[LazyList[Cursor]]): Boolean = {
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

  def parse(lines: List[LazyList[Cursor]], linkRefs: mutable.Map[String, LinkReference]): (Block, Int) = {
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
    val contentCursors = line.drop(pos).takeWhile(c => c.char != '\n').toList

    // Convert to string for processing
    val contentString = contentCursors.map(_.char).mkString

    // Process according to spec:
    // 1. Remove trailing # sequence if it's preceded by optional spaces
    // 2. Remove all trailing spaces
    val trimmedContent = contentString
      .replaceAll("[ \t]+#+[ \t]*$", "") // Remove trailing sequence of hashes with spaces
      .stripTrailing()                   // Remove trailing spaces

    // Convert the content to a list of Text nodes
    val content = if (trimmedContent.nonEmpty) {
      List(Text(trimmedContent))
    } else {
      List.empty[Inline]
    }

    (Heading(level, content), 1)
  }
}
