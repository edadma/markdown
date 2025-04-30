package io.github.edadma.markdown

import scala.collection.mutable

// A parser for paragraph blocks - our first concrete implementation
object ParagraphBlockParser extends BlockParser {
  val name: String = "paragraph blocks"

  def canStart(lines: List[LazyList[C]]): Boolean = {
    // A paragraph can start with any non-blank line
    !isBlankLine(lines.head) && !ATXHeadingBlockParser.canStart(lines)
  }

  def parse(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
  ): (Block, Int) = {

    // predicate: “this line still belongs to a paragraph”
    def isParaLine(line: LazyList[C]): Boolean =
      !isBlankLine(line) &&
        !ATXHeadingBlockParser.canStart(List(line)) &&
        !ThematicBreakBlockParser.canStart(List(line)) &&
        !HTMLBlockParser.canStart(List(line)) &&
        !IndentedCodeBlockParser.canStart(List(line)) &&
        !FencedCodeBlockParser.canStart(List(line)) &&
        !BlockQuoteParser.canStart(List(line)) &&
        !ListBlockParser.canStart(List(line))

    // split into the leading paragraph lines, and the remainder
    val (paraLines, rest) = lines.span(isParaLine)

    // if the next line is blank, consume it too (so paragraphs absorb the blank line)
    val linesConsumed = rest.headOption match {
      case Some(l) if isBlankLine(l) => paraLines.length + 1
      case _                         => paraLines.length
    }

    val content: List[Inline] =
      if (paraLines.nonEmpty) {
        // Start with all lines flattened
        val allChars = paraLines.flatten

        // Remove the last character if it's a newline
        if (allChars.lastOption.exists(_.char == '\n'))
          allChars.dropRight(1)
        else
          allChars
      } else {
        List.empty[Inline]
      }

    (Paragraph(content), linesConsumed)
  }
}

// Function to check if a line is blank
private def isBlankLine(line: LazyList[C]): Boolean = {
  // A blank line contains only whitespace or is empty (excluding newline)
  val contentChars = line.filter(_.char != '\n')
  contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
}
