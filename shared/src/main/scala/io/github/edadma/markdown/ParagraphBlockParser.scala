package io.github.edadma.markdown

// A parser for paragraph blocks - our first concrete implementation
object ParagraphBlockParser extends BlockParser {
  def canStart(line: LazyList[Cursor]): Boolean = {
    // A paragraph can start with any non-blank line
    !isBlankLine(line)
  }

  def parse(lines: List[LazyList[Cursor]]): (Block, Int) = {
    // Find the first blank line
    val paragraphLines = lines.takeWhile(line => !isBlankLine(line))

    // The actual number of lines consumed is the paragraph plus the blank line
    val linesConsumed = if (paragraphLines.length < lines.length) {
      paragraphLines.length + 1 // Include the blank line that terminated the paragraph
    } else {
      paragraphLines.length // The paragraph runs to the end
    }

    (Paragraph(paragraphLines.flatten), linesConsumed)
  }
}

// Function to check if a line is blank
private def isBlankLine(line: LazyList[Cursor]): Boolean = {
  // A blank line contains only whitespace or is empty (excluding newline)
  val contentChars = line.filter(_.char != '\n')
  contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
}
