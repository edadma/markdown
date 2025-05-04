package io.github.edadma.markdown

import scala.collection.mutable

object MathBlockParser extends BlockParser {
  val name: String = "math blocks"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (!config.mathEnabled) return false
    if (lines.isEmpty) return false

    val line = lines.head
    val text = line.takeWhile(_.char != '\n').map(_.char).mkString.trim

    text.startsWith("$$")
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    def string(chars: List[C]): String = chars.takeWhile(_.char != '\n').flatMap(c => {
      if c.isLiteral then List('\\', c.char)
      else List(c.char)
    }).mkString.trim

    val firstLine     = lines.head
    val firstLineText = string(firstLine)

    // Case 1: Single-line math block
    if (firstLineText.startsWith("$$") && firstLineText.endsWith("$$") && firstLineText.length > 4) {
      // Extract content between $$ markers
      val content = firstLineText.substring(2, firstLineText.length - 2).trim
      return (MathBlock(content), 1)
    }

    // Case 2: Multi-line math block
    if (firstLineText.startsWith("$$")) {
      val contentBuilder = new StringBuilder
      var lineCount      = 1
      var foundClosing   = false

      // Skip the opening $$ line
      var currentLines = lines.tail

      // Collect lines until closing $$
      contentBuilder ++= firstLineText.substring(2)

      while (currentLines.nonEmpty && !foundClosing) {
        val line = currentLines.head
        val text = string(line)

        if (text.endsWith("$$")) {
          // Found closing delimiter
          foundClosing = true
          contentBuilder.append('\n')
          contentBuilder ++= text.substring(0, text.length - 2)
        } else {
          // Add line to content
          if (contentBuilder.nonEmpty) contentBuilder.append('\n')
          contentBuilder.append(text)
        }

        lineCount += 1
        currentLines = currentLines.tail
      }

      return (MathBlock(contentBuilder.toString), lineCount)
    }

    // Not a math block
    (null, 0)
  }
}
