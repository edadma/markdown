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
    if (isBlankLine(line) && currentParagraph.nonEmpty) {
      // End of paragraph - concatenate the cursor lines
      val paragraphCursors = currentParagraph.reverse.flatten
      paragraphs = paragraphCursors :: paragraphs
      currentParagraph = Nil
    } else if (!isBlankLine(line)) {
      // Continue paragraph
      currentParagraph = line :: currentParagraph
    }
  }

  // Add final paragraph if there is one
  if (currentParagraph.nonEmpty) {
    val paragraphCursors = currentParagraph.reverse.flatten
    paragraphs = paragraphCursors :: paragraphs
  }

  paragraphs.reverse
}

private def isBlankLine(line: LazyList[Cursor]): Boolean = {
  line.forall(c => c.char == ' ' || c.char == '\t')
}

private def groupIntoLines(stream: LazyList[Cursor]): List[LazyList[Cursor]] = {
  // Implementation to group cursor stream into lines of cursors
  // ...
}
