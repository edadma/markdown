package io.github.edadma.markdown

import scala.collection.mutable

object CollapsibleBlockParser extends BlockParser {
  val name: String = "container blocks"

  // Regex to match container syntax: ::: type [attributes]
  private val CollapsibleStartPattern = """^:::\s*(.*)$""".r
  private val CollapsibleEndPattern   = """^:::$""".r

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) false
    else {
      val lineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString.trim
      CollapsibleStartPattern.matches(lineText)
    }
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    val firstLine     = lines.head
    val firstLineText = firstLine.takeWhile(_.char != '\n').map(_.char).mkString.trim

    // Extract container type and title using pattern matching
    firstLineText match {
      case CollapsibleStartPattern(nullableTitle) =>
        // Parse optional title and check for "open" flag
        val titleText = Option(nullableTitle).map(_.trim).getOrElse("")
        val (title, isOpen) =
          if (titleText.isEmpty) {
            (None, false)
          } else {
            val open        = titleText.startsWith("open")
            val actualTitle = if (open) titleText.substring(4).trim else titleText
            (if (actualTitle.isEmpty) None else Some(actualTitle), open)
          }

        // Collect content lines until ending ":::"
        var contentLines = LazyList.empty[List[C]]
        var lineCount    = 1
        var currentLine  = 1 // Start from the second line
        var foundEnd     = false

        while (currentLine < lines.length && !foundEnd) {
          val line     = lines(currentLine)
          val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString.trim

          if (CollapsibleEndPattern.matches(lineText)) {
            foundEnd = true
          } else {
            contentLines = contentLines.appended(line)
          }

          lineCount += 1
          currentLine += 1
        }

        // Parse content as a nested document
        val blocks = processLines(contentLines, linkRefs, parentIndent + 2, config)

        (CollapsibleBlock(title, isOpen, blocks), lineCount)

      case _ =>
        // Should never happen due to canStart check
        (null, 0)
    }
  }
}
