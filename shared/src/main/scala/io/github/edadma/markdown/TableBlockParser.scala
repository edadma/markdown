package io.github.edadma.markdown

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object TableBlockParser extends BlockParser {
  val name: String = "table blocks"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (!config.tablesEnabled) return false
    if (lines.isEmpty || lines.tail.isEmpty) return false

    val firstLine  = lineToString(lines.head)
    val secondLine = lineToString(lines(1))

    // Check if first line contains pipes and second line looks like a delimiter row
    firstLine.contains('|') && isDelimiterRow(secondLine)
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Parse header row
    val headerCells = parseCellsFromLine(lines.head)
    val headerRow   = TableRow(headerCells)

    // Parse delimiter row and extract alignments
    val delimiterLine = lineToString(lines(1))
    val alignments    = parseDelimiterRow(delimiterLine)

    // Parse data rows (if any)
    var currentLine = 2
    val dataRows    = new mutable.ListBuffer[TableRow]

    while (currentLine < lines.size && isTableRow(lines(currentLine))) {
      dataRows += TableRow(parseCellsFromLine(lines(currentLine)))
      currentLine += 1
    }

    (Table(headerRow, dataRows.toList, alignments), currentLine)
  }

  // Convert a single line to a string for simple operations
  private def lineToString(line: List[C]): String =
    line.takeWhile(_.char != '\n').map(_.char).mkString

  // Check if a line is a delimiter row (contains only -, |, :, and whitespace)
  private def isDelimiterRow(line: String): Boolean = {
    if (!line.contains('|')) return false

    // Extract cell content
    val cells = splitCellsText(line)

    // Each cell in a delimiter row must contain at least one - and only -, :, and whitespace
    cells.forall(cell =>
      cell.trim.nonEmpty &&
        cell.contains('-') &&
        cell.forall(c => c == '-' || c == ':' || c.isWhitespace),
    )
  }

  // Check if a line is a table row (contains pipe character)
  private def isTableRow(line: List[C]): Boolean = {
    lineToString(line).contains('|')
  }

  // Parse cells from a line of cursors
  private def parseCellsFromLine(line: List[C]): List[TableCell] = {
    val lineContent = line.takeWhile(_.char != '\n').toList

    if (lineContent.isEmpty) return List()

    // Step 1: Find positions of all unescaped pipe characters
    val pipesPositions = findUnescapedPipePositions(lineContent)

    // Step 2: Split the line at these positions
    val cellRanges = getCellRanges(pipesPositions, lineContent.size)

    // Step 3: Extract cursor objects for each cell
    cellRanges.map { case (start, end) =>
      val cellCursors = lineContent.slice(start, end)

      // Trim whitespace from cell content
      val trimmedCursors = trimCursors(cellCursors)

      TableCell(trimmedCursors)
    }
  }

  // Find positions of all unescaped pipe characters
  private def findUnescapedPipePositions(lineContent: List[C]): List[Int] = {
    val positions = new mutable.ListBuffer[Int]

    // Add implicit beginning position if line doesn't start with pipe
    if (lineContent.isEmpty || lineContent.head.char != '|') {
      positions += -1
    }

    // Find all unescaped pipe characters
    for (i <- lineContent.indices) {
      val c = lineContent(i)

      if (c.char == '|' && !c.isLiteral) {
        positions += i
      }
    }

    // Add implicit ending position if line doesn't end with pipe
    if (lineContent.isEmpty || lineContent.last.char != '|') {
      positions += lineContent.size
    }

    positions.toList
  }

  // Get cell ranges based on pipe positions
  private def getCellRanges(pipePositions: List[Int], lineLength: Int): List[(Int, Int)] = {
    val ranges = new mutable.ListBuffer[(Int, Int)]

    for (i <- 0 until pipePositions.length - 1) {
      val start = pipePositions(i) + 1
      val end   = pipePositions(i + 1)

      if (start <= end) {
        ranges += ((start, end))
      }
    }

    ranges.toList
  }

  // Trim whitespace cursors from beginning and end of a cell
  private def trimCursors(cursors: List[C]): List[C] = {
    val firstNonWhitespace = cursors.indexWhere(c => !isWhitespace(c.char))
    val lastNonWhitespace  = cursors.lastIndexWhere(c => !isWhitespace(c.char))

    if (firstNonWhitespace == -1) {
      List.empty // All whitespace
    } else {
      cursors.slice(firstNonWhitespace, lastNonWhitespace + 1)
    }
  }

  // Check if a character is whitespace
  private def isWhitespace(c: Char): Boolean = c == ' ' || c == '\t'

  // Parse the delimiter row to determine column alignments
  private def parseDelimiterRow(line: String): List[TableAlignment] = {
    val cells = splitCellsText(line)

    cells.map { cell =>
      val trimmed = cell.trim
      if (trimmed.startsWith(":") && trimmed.endsWith(":")) TableAlignment.Center
      else if (trimmed.startsWith(":")) TableAlignment.Left
      else if (trimmed.endsWith(":")) TableAlignment.Right
      else TableAlignment.None
    }
  }

  // Split text into cells by handling pipes
  private def splitCellsText(line: String): List[String] = {
    // Handle optional leading/trailing pipes
    val processedLine = line.trim
    val content       = if (processedLine.startsWith("|")) processedLine.substring(1) else processedLine
    val trimmed       = if (content.endsWith("|")) content.substring(0, content.length - 1) else content

    // Split by pipes, handling escaped pipes
    val result  = new mutable.ListBuffer[String]
    var current = new StringBuilder
    var escaped = false

    for (c <- trimmed) {
      if (escaped) {
        current.append(c)
        escaped = false
      } else if (c == '\\') {
        escaped = true
      } else if (c == '|') {
        result += current.toString
        current = new StringBuilder
      } else {
        current.append(c)
      }
    }

    result += current.toString
    result.toList
  }
}
