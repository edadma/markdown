// File: parseDocument.scala
package io.github.edadma.markdown

def parseDocument(stream: LazyList[Cursor]): Document = {
  val blocks = parseBlocks(stream)
  Document(blocks)
}

private def parseBlocks(stream: LazyList[Cursor]): List[Block] = {
  // Group cursor stream into paragraphs while preserving cursors
  val paragraphs = collectParagraphs(stream)

  // Initially treat each paragraph as containing a single Text node
  // Later, we'll replace this with real inline parsing
  paragraphs.map { cursors =>
    // For now, just concatenate the characters from cursors into a string
    val text = cursors.map(_.char).mkString
    Paragraph(List(Text(text)))
  }
}

private def collectParagraphs(stream: LazyList[Cursor]): List[LazyList[Cursor]] = {
  // Group cursors into lines first
  val lines = groupIntoLines(stream)

  // Group lines into paragraphs (of cursors)
  var paragraphs: List[LazyList[Cursor]]       = Nil
  var currentParagraph: List[LazyList[Cursor]] = Nil

  for (line <- lines) {
    // Debug the line content
    // println(s"Line: '${line.map(_.char).mkString}'")
    // println(s"Is blank: ${isBlankLine(line)}")

    if (isBlankLine(line) && currentParagraph.nonEmpty) {
      // End of paragraph - concatenate the cursor lines
      val paragraphCursors = currentParagraph.reverse.foldLeft(LazyList.empty[Cursor])(_ ++ _)
      paragraphs = paragraphCursors :: paragraphs
      currentParagraph = Nil
    } else if (!isBlankLine(line)) {
      // Continue paragraph
      currentParagraph = line :: currentParagraph
    }
    // Skip blank lines that don't end paragraphs
  }

  // Add final paragraph if there is one
  if (currentParagraph.nonEmpty) {
    val paragraphCursors = currentParagraph.reverse.foldLeft(LazyList.empty[Cursor])(_ ++ _)
    paragraphs = paragraphCursors :: paragraphs
  }

  paragraphs.reverse
}

private def isBlankLine(line: LazyList[Cursor]): Boolean = {
  // A blank line contains only whitespace or is empty (excluding newline)
  val contentChars = line.filter(_.char != '\n')
  contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
}

private def groupIntoLines(stream: LazyList[Cursor]): List[LazyList[Cursor]] = {
  var lines: List[LazyList[Cursor]] = Nil
  var currentLine: List[Cursor]     = Nil

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
