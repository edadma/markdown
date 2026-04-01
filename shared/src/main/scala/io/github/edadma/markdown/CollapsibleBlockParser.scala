package io.github.edadma.markdown

import scala.collection.mutable

object CollapsibleBlockParser extends BlockParser {
  val name: String = "container blocks"

  // Regex to match container syntax: ::: type [attributes]
  private val CollapsibleStartPattern = """^:::\s+(.+)$""".r
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

    firstLineText match {
      case CollapsibleStartPattern(nullableTitle) =>
        // Parse title and open flag
        val titleText = Option(nullableTitle).map(_.trim).getOrElse("")
        val (title, isOpen) =
          if (titleText.isEmpty) {
            ("", false)
          } else {
            val open        = titleText.startsWith("open")
            val actualTitle = if (open) titleText.substring(4).trim else titleText
            (actualTitle, open)
          }
        val titleCursors = if (title.isEmpty) {
          List.empty[Inline]
        } else {
          // Get the title portion from the line's cursors
          val colonIdx = firstLine.indexWhere(_.char == ':')
          val afterColons = firstLine.drop(colonIdx).dropWhile(_.char == ':').dropWhile(_.char == ' ')
          val titleStart = if (isOpen) afterColons.dropWhile(c => "open".contains(c.char)).dropWhile(_.char == ' ') else afterColons
          titleStart.takeWhile(_.char != '\n').toList
        }

        // Scan for the matching outermost end marker
        var contentLines = LazyList.empty[List[C]]
        var lineCount    = 1
        var currentLine  = 1 // Start from the second line
        var nestingLevel = 1 // Start with nesting level 1 (we've already seen one opener)

        while (currentLine < lines.length && nestingLevel > 0) {
          val line     = lines(currentLine)
          val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString.trim

          // Check if this is a start or end marker
          if (CollapsibleStartPattern.matches(lineText)) {
            // Found nested section start, increase nesting level
            nestingLevel += 1
            contentLines = contentLines.appended(line)
          } else if (CollapsibleEndPattern.matches(lineText)) {
            // Found a section end, decrease nesting level
            nestingLevel -= 1

            // Only add this line to content if it's not our matching end marker
            if (nestingLevel > 0) {
              contentLines = contentLines.appended(line)
            }
          } else {
            // Regular content line
            contentLines = contentLines.appended(line)
          }

          lineCount += 1
          currentLine += 1
        }

        // Parse content as a nested document
        val blocks = processLines(contentLines, linkRefs, parentIndent + 2, config)

        (CollapsibleBlock(titleCursors, isOpen, blocks), lineCount)

      case _ =>
        // Should never happen due to canStart check
        (null, 0)
    }
  }
}
