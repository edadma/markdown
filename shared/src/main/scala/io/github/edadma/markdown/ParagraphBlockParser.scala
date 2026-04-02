package io.github.edadma.markdown

import scala.collection.mutable

object ParagraphBlockParser extends BlockParser {
  val name: String = "paragraph blocks"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    // A paragraph can start with any non-blank line
    !isBlankLine(lines.head) && !ATXHeadingBlockParser.canStart(lines, config)
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {

    def isParaLine(line: List[C]): Boolean =
      !isBlankLine(line) &&
        !ATXHeadingBlockParser.canStart(LazyList(line), config) &&
        !ThematicBreakBlockParser.canStart(LazyList(line), config) &&
        !HTMLBlockParser.canInterruptParagraph(LazyList(line), config) &&
        !FencedCodeBlockParser.canStart(LazyList(line), config) &&
        !BlockQuoteParser.canStart(LazyList(line), config) &&
        !ListBlockParser.canInterruptParagraph(LazyList(line), config) &&
        !MathBlockParser.canStart(LazyList(line), config)

    // split into the leading paragraph lines, and the remainder
    val (paraLines, rest) = lines.span(isParaLine)

    // if the next line is blank, consume it too (so paragraphs absorb the blank line)
    val linesConsumed = rest.headOption match {
      case Some(l) if isBlankLine(l) => paraLines.length + 1
      case _                         => paraLines.length
    }

    val content: List[C] =
      if (paraLines.nonEmpty) {
        // Start with all lines flattened
        val allChars = paraLines.flatMap(_.dropWhile(_.char.isSpaceChar)).toList

        // Remove the last character if it's a newline
        val noLastNewline =
          if (allChars.lastOption.exists(_.char == '\n'))
            allChars.dropRight(1)
          else
            allChars

        noLastNewline.reverse.dropWhile(_.char.isSpaceChar).reverse
      } else {
        List.empty[C]
      }

    (Paragraph(content), linesConsumed)
  }
}

// Function to check if a line is blank
private def isBlankLine(line: List[C]): Boolean = {
  // A blank line contains only whitespace or is empty (excluding newline)
  val contentChars = line.filter(_.char != '\n')
  contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
}
