package io.github.edadma.markdown

import scala.collection.mutable

// Represents data about the list as a whole
case class ListData(
    isOrdered: Boolean,
    bulletChar: Option[Char] = None, // For unordered lists: -, +, *
    startNumber: Option[Int] = None, // For ordered lists: the starting number
    delimiter: Option[Char] = None,  // For ordered lists: . or )
    isTight: Boolean = true,         // Default to tight list
    indent: Int,
)

object ListBlockParser extends BlockParser {
  val name: String = "list blocks"

  // Patterns for detecting list markers
  private val UnorderedListMarker = """^( {0,3})([-+*])(\s+)(.*)$""".r
  private val OrderedListMarker   = """^( {0,3})(\d{1,9})([.)])(\s+)(.*)$""".r
  // Empty list item markers (marker at end of line with no content)
  private val EmptyBullet  = """^( {0,3})([-+*])$""".r
  private val EmptyOrdered = """^( {0,3})(\d{1,9})([.)])$""".r
  private val MaxIndentTolerance  = 3

  /** Check if this list can interrupt a paragraph. Per CommonMark spec:
    * - An ordered list with start != 1 cannot interrupt a paragraph
    * - An empty list item cannot interrupt a paragraph
    */
  def canInterruptParagraph(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (!canStart(lines, config)) return false
    val lineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    // Check ordered list start number
    OrderedListMarker.findFirstMatchIn(lineText) match {
      case Some(m) =>
        val number = m.group(2).toInt
        val content = m.group(5)
        number == 1 && content.trim.nonEmpty // Must start at 1 and not be empty
      case None =>
        // Unordered list — check it's not an empty item
        UnorderedListMarker.findFirstMatchIn(lineText) match {
          case Some(m) => m.group(4).trim.nonEmpty
          case None    => false
        }
    }
  }

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) return false

    // Check if any marker character is escaped (isLiteral) — escaped markers don't start lists
    val trimmed = lines.head.dropWhile(c => c.char == ' ' && !c.isLiteral)
    if (trimmed.nonEmpty && trimmed.head.isLiteral) return false
    // For ordered lists (digit followed by . or )), check if the . or ) is literal
    val afterDigits = trimmed.dropWhile(c => c.char.isDigit && !c.isLiteral)
    if (afterDigits.nonEmpty && (afterDigits.head.char == '.' || afterDigits.head.char == ')') && afterDigits.head.isLiteral)
      return false

    val lineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    // Check if this line can start a list
    isListMarker(lineText)
  }

  private def isListMarker(line: String): Boolean = {
    // Check for unordered list marker (with content or empty)
    UnorderedListMarker.findFirstMatchIn(line).exists(m => m.group(3).nonEmpty) ||
    EmptyBullet.findFirstMatchIn(line).isDefined ||
    // Check for ordered list marker (with content or empty)
    OrderedListMarker.findFirstMatchIn(line).exists(m => m.group(4).nonEmpty) ||
    EmptyOrdered.findFirstMatchIn(line).isDefined
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Extract list type and properties from the first line
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val listData      = extractListData(firstLineText)

    // Collect list items and determine if the list is tight or loose
    val (items, linesConsumed, hasBlanksBetweenItems, itemBlanksFlags) =
      collectListItems(lines, listData, linkRefs, parentIndent, config)

    // A list is loose if:
    // 1. There are blank lines between items, OR
    // 2. Any item directly contains two block-level elements with a blank line between them
    val anyItemLoose = items.zip(itemBlanksFlags).exists { case (item, (atItemLevel, anywhere)) =>
      item.content.size >= 2 && (
        // Blank at exact item level → definitely between direct children
        (atItemLevel) ||
        // Multiple paragraphs always implies blank lines between them
        (anywhere && item.content.count(_.isInstanceOf[Paragraph]) >= 2) ||
        // Non-list/non-paragraph blocks (code, blockquote, html) with blanks → loose
        (anywhere && item.content.exists(b => !b.isInstanceOf[Paragraph] && !b.isInstanceOf[ListBlock]))
      )
    }
    val isTight = !hasBlanksBetweenItems && !anyItemLoose

    val finalListData = listData.copy(isTight = isTight)

    (ListBlock(finalListData, items), linesConsumed)
  }

  private def extractListData(line: String): ListData = {
    // Try to match unordered list marker (with content)
    UnorderedListMarker.findFirstMatchIn(line).map { m =>
      ListData(isOrdered = false, bulletChar = Some(m.group(2).charAt(0)), indent = m.group(1).length)
    }.orElse {
      // Try empty unordered marker
      EmptyBullet.findFirstMatchIn(line).map { m =>
        ListData(isOrdered = false, bulletChar = Some(m.group(2).charAt(0)), indent = m.group(1).length)
      }
    }.getOrElse {
      // Try ordered list marker (with content)
      OrderedListMarker.findFirstMatchIn(line).map { m =>
        ListData(isOrdered = true, startNumber = Some(m.group(2).toInt), delimiter = Some(m.group(3).charAt(0)), indent = m.group(1).length)
      }.getOrElse {
        // Must be an empty ordered marker
        val m = EmptyOrdered.findFirstMatchIn(line).get
        ListData(isOrdered = true, startNumber = Some(m.group(2).toInt), delimiter = Some(m.group(3).charAt(0)), indent = m.group(1).length)
      }
    }
  }

  private def collectListItems(
      lines: LazyList[List[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (List[ListItem], Int, Boolean, List[(Boolean, Boolean)]) = {
    val items                  = new mutable.ListBuffer[ListItem]
    val itemBlanksFlags        = new mutable.ListBuffer[(Boolean, Boolean)] // (atItemLevel, anywhere)
    var currentLines           = lines
    var totalLinesConsumed     = 0
    var hasBlanksBetweenItems  = false
    var previousItemEndedBlank = false

    // Process each list item
    while (currentLines.nonEmpty && isMatchingListItemStart(currentLines.head, listData) && !ThematicBreakBlockParser.canStart(currentLines, config)) {
      // If the previous item ended with blank lines, there's a blank between items
      if (items.nonEmpty && previousItemEndedBlank) {
        hasBlanksBetweenItems = true
      }

      // Parse a single list item
      val (item, linesConsumed, blankFlags) = parseListItem(currentLines, listData, linkRefs, parentIndent, config)

      items += item
      itemBlanksFlags += blankFlags
      totalLinesConsumed += linesConsumed

      // Check if the last consumed line(s) were blank (blank lines trailing this item)
      val consumedLines = currentLines.take(linesConsumed)
      previousItemEndedBlank = consumedLines.nonEmpty && isBlankLine(consumedLines.last)

      currentLines = currentLines.drop(linesConsumed)
    }

    (items.toList, totalLinesConsumed, hasBlanksBetweenItems, itemBlanksFlags.toList)
  }

  private def isMatchingListItemStart(line: List[C], listData: ListData): Boolean = {
    // Convert the line to String (excluding newline)
    val text = line.takeWhile(_.char != '\n').map(_.char).mkString

    if (!listData.isOrdered) {
      // For unordered lists, match bullet char with marker at 0-3 spaces
      UnorderedListMarker.findFirstMatchIn(text).exists { m =>
        m.group(2).charAt(0) == listData.bulletChar.get && m.group(1).length <= 3
      } ||
      EmptyBullet.findFirstMatchIn(text).exists { m =>
        m.group(2).charAt(0) == listData.bulletChar.get && m.group(1).length <= 3
      }
    } else {
      // For ordered lists, match delimiter with marker at 0-3 spaces
      OrderedListMarker.findFirstMatchIn(text).exists { m =>
        m.group(3).charAt(0) == listData.delimiter.get && m.group(1).length <= 3
      } ||
      EmptyOrdered.findFirstMatchIn(text).exists { m =>
        m.group(3).charAt(0) == listData.delimiter.get && m.group(1).length <= 3
      }
    }
  }

  private def parseListItem(
      lines: LazyList[List[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (ListItem, Int, (Boolean, Boolean)) = {
    // Get indentation information from first line
    val (markerIndent, contentIndent) = getIndentation(lines.head, listData)

    // Collect all lines for this list item, including continuation lines
    val (itemLines, linesConsumed, blankFlags) = collectItemLines(lines, markerIndent, contentIndent, listData, config)

    // Process the item content to remove marker and adjust indentation
    // but KEEP the original C objects to preserve escaping information
    val processedLines = processItemLines(itemLines, contentIndent, listData)

    // Use the existing block parsing machinery directly, without creating a new document
    val totalIndent = parentIndent + contentIndent

    // Pass the total indent to any recursive list parsing
    val itemBlocks = processLines(processedLines, linkRefs, totalIndent, config).map {
      case nestedList: ListBlock =>
        // Use the total combined indentation
        nestedList.copy(data = nestedList.data.copy(indent = nestedList.data.indent + totalIndent))
      case other => other
    }

    // Create the list item with the parsed blocks
    (ListItem(itemBlocks), linesConsumed, blankFlags)
  }

  private def getIndentation(line: List[C], listData: ListData): (Int, Int) = {
    val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

    // Calculate virtual width of a whitespace string starting at a given column
    def virtualWidth(s: String, startCol: Int): Int = {
      var col = startCol
      for (c <- s) {
        if (c == ' ') col += 1
        else if (c == '\t') col += 4 - (col % 4)
        else return col - startCol
      }
      col - startCol
    }

    if (listData.isOrdered) {
      OrderedListMarker.findFirstMatchIn(lineText) match {
        case Some(m) =>
          val leadingIndent = m.group(1).length
          val marker        = m.group(2) + m.group(3)
          val markerEnd     = leadingIndent + marker.length
          val spacesWidth   = virtualWidth(m.group(4), markerEnd)
          // Per spec: 1-4 spaces after marker count toward content indent; >4 means indented code (use 1)
          val effectiveSpaces = if (spacesWidth > 4) 1 else spacesWidth
          (leadingIndent, markerEnd + effectiveSpaces)
        case None =>
          // Empty ordered marker: N. or N) with no content — contentIndent = markerEnd + 1
          val m = EmptyOrdered.findFirstMatchIn(lineText).get
          val leadingIndent = m.group(1).length
          val markerEnd     = leadingIndent + m.group(2).length + 1 // number + delimiter
          (leadingIndent, markerEnd + 1)
      }
    } else {
      UnorderedListMarker.findFirstMatchIn(lineText) match {
        case Some(m) =>
          val leadingIndent = m.group(1).length
          val markerEnd     = leadingIndent + 1 // bullet is 1 char
          val spacesWidth   = virtualWidth(m.group(3), markerEnd)
          val effectiveSpaces = if (spacesWidth > 4) 1 else spacesWidth
          (leadingIndent, markerEnd + effectiveSpaces)
        case None =>
          // Empty bullet marker: *, -, + with no content — contentIndent = markerEnd + 1
          val m = EmptyBullet.findFirstMatchIn(lineText).get
          val leadingIndent = m.group(1).length
          (leadingIndent, leadingIndent + 2) // bullet(1) + 1 space
      }
    }
  }

  private def collectItemLines(
      lines: LazyList[List[C]],
      markerIndent: Int,
      contentIndent: Int,
      listData: ListData,
      config: MarkdownConfig,
  ): (LazyList[List[C]], Int, (Boolean, Boolean)) = {
    // A matching marker at indent < contentIndent is a sibling (ends this item).
    // A matching marker at indent >= contentIndent is nested (stays in this item).
    def isSiblingListItem(line: String): Boolean = {
      val lineIndent = countLeadingSpaces(line)

      // Must be at 0-3 spaces, less than content indent, and matching list type
      lineIndent <= 3 &&
      lineIndent < contentIndent &&
      isListMarker(line) &&
      (if (!listData.isOrdered)
         UnorderedListMarker.findFirstMatchIn(line).exists(_.group(2).charAt(0) == listData.bulletChar.get) ||
         EmptyBullet.findFirstMatchIn(line).exists(_.group(2).charAt(0) == listData.bulletChar.get)
       else
         OrderedListMarker.findFirstMatchIn(line).exists(_.group(3).charAt(0) == listData.delimiter.get) ||
         EmptyOrdered.findFirstMatchIn(line).exists(_.group(3).charAt(0) == listData.delimiter.get))
    }

    val itemLines = new mutable.ListBuffer[List[C]]
    itemLines += lines.head

    var count                   = 1
    var currentLines            = lines.tail
    var inItem                  = true
    var blanksAtItemLevel       = false // Blank followed by content at exactly contentIndent
    var blanksAnywhere          = false // Blank followed by any content within item
    var previousWasBlank        = false
    var inParagraph             = true // Track if we're in a paragraph (for lazy continuation)

    // Track if we've seen a non-blank line after the marker
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    var hasSeenContent = if (listData.isOrdered) {
      OrderedListMarker.findFirstMatchIn(firstLineText).exists(_.group(5).trim.nonEmpty)
    } else {
      UnorderedListMarker.findFirstMatchIn(firstLineText).exists(_.group(4).trim.nonEmpty)
    }

    // Function to check if a line is a potential nested list item
    def isPotentialNestedListItem(line: String, lineIndent: Int): Boolean = {
      // A nested list item must:
      // 1. Have proper indentation (greater than the content indent)
      // 2. Start with a list marker after the indentation
      if (lineIndent < contentIndent) return false

      val potentialMarker = line.substring(lineIndent)
      UnorderedListMarker.findFirstMatchIn(potentialMarker).exists { m =>
        m.group(3).nonEmpty // Must have whitespace after marker
      } ||
      OrderedListMarker.findFirstMatchIn(potentialMarker).exists { m =>
        m.group(4).nonEmpty // Must have whitespace after marker
      }
    }

    while (inItem && currentLines.nonEmpty) {
      val line     = currentLines.head
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      if (isBlankLine(line)) {
        inParagraph = false // Blank line ends paragraph

        // Handle a blank line
        itemLines += line
        count += 1
        currentLines = currentLines.tail

        // If we've seen content and this is a blank line, mark it
        if (hasSeenContent) {
          previousWasBlank = true
        }

        // Look ahead to see if this blank line ends the item
        if (currentLines.nonEmpty) {
          val nextLine = currentLines.head
          val nextText = nextLine.takeWhile(_.char != '\n').map(_.char).mkString

          if (isListItemStart(nextText, contentIndent)) {
            // Next line is a new list item at same nesting level - end this item
            inItem = false
            if (previousWasBlank) {
              blanksAtItemLevel = true
              blanksAnywhere = true
            }
          } else if (!isBlankLine(nextLine)) {
            // Next line is not blank - check if it's indented enough
            val nextIndent = countLeadingSpaces(nextText)

            if (nextIndent < markerIndent && !isListMarker(nextText)) {
              // Not indented enough and not a new list - end of item
              inItem = false
            }
          }
        }
      } else if (isListItemStart(lineText, contentIndent)) {
        // New list item at same level - end this item
        inItem = false
      } else {
        // Regular content line - check if it belongs to this item
        val lineIndent = countLeadingSpaces(lineText)

        if (isSiblingListItem(lineText)) {
          // This is a new item at the SAME list level - end current item
          inItem = false
        } else if (isPotentialNestedListItem(lineText, lineIndent)) {
          // This is likely a nested list item - include it in this item
          hasSeenContent = true

          if (previousWasBlank) {
            blanksAnywhere = true
            if (lineIndent == contentIndent) blanksAtItemLevel = true
          }

          previousWasBlank = false
          itemLines += line
          count += 1
          currentLines = currentLines.tail
        } else if (lineIndent >= contentIndent) {
          // Properly indented line - clearly part of this item
          inParagraph = true // Start/continue paragraph
          hasSeenContent = true

          if (previousWasBlank) {
            blanksAnywhere = true
            if (lineIndent == contentIndent) blanksAtItemLevel = true
          }

          previousWasBlank = false
          itemLines += line
          count += 1
          currentLines = currentLines.tail
        } else if (inParagraph && !ThematicBreakBlockParser.canStart(currentLines, config)) {
          // Lazy continuation: include non-marker line if it continues a paragraph
          // but not if the line is a thematic break (which can interrupt a list)
          inParagraph = true
          previousWasBlank = false
          itemLines += line
          count += 1
          currentLines = currentLines.tail
        } else {
          // Not indented enough - end of item
          inItem = false
        }
      }
    }

    (LazyList.from(itemLines.toList), count, (blanksAtItemLevel, blanksAnywhere))
  }

  // Helper function to determine if a line starts a new sibling list item
  // (marker at 0-3 spaces AND indent < contentIndent of current item)
  private def isListItemStart(lineText: String, contentIndent: Int): Boolean = {
    val leadingIndent = countLeadingSpaces(lineText)

    // Must be at 0-3 spaces and less than the current item's content indent
    if (leadingIndent > 3 || leadingIndent >= contentIndent) return false

    isListMarker(lineText)
  }

  // Helper function to count leading spaces
  private def countLeadingSpaces(text: String): Int = {
    var col = 0
    for (c <- text) {
      if (c == ' ') col += 1
      else if (c == '\t') col += 4 - (col % 4)
      else return col
    }
    col
  }

  private def processItemLines(
      itemLines: LazyList[List[C]],
      contentIndent: Int,
      listData: ListData,
  ): LazyList[List[C]] = {
    // Process first line - remove marker and appropriate whitespace
    val lineText = itemLines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val firstLineProcessed = {
      val isEmptyMarker =
        if (listData.isOrdered) OrderedListMarker.findFirstMatchIn(lineText).isEmpty
        else UnorderedListMarker.findFirstMatchIn(lineText).isEmpty

      if (isEmptyMarker) {
        // Empty marker line — produce a blank line so processLines yields no blocks
        List(C('\n', 0, 0, 0, false))
      } else {
        val markerEndCharPos =
          if (listData.isOrdered) {
            val m = OrderedListMarker.findFirstMatchIn(lineText).get
            m.start(4) // Position of first whitespace char after marker
          } else {
            val m = UnorderedListMarker.findFirstMatchIn(lineText).get
            m.start(3) // Position of first whitespace char after marker
          }
        // Drop chars up to marker end, then drop 1 virtual space-equivalent of whitespace
        val afterMarker = itemLines.head.drop(markerEndCharPos)
        dropIndent(afterMarker, 1, markerEndCharPos)
      }
    }

    // Process remaining lines - adjust indentation while preserving C objects
    val restProcessed = itemLines.tail.map { line =>
      if (isBlankLine(line)) {
        line // Keep blank lines as-is
      } else {
        // Remove indentation up to contentIndent, preserving original C objects
        val lineIndent = countLeadingSpaces(line.takeWhile(_.char != '\n').map(_.char).mkString)

        if (lineIndent < contentIndent) {
          // This is a lazy continuation line - keep it as is
          line
        } else {
          // Normal continuation line - remove indentation
          removeIndentation(line, contentIndent)
        }
      }
    }

    (firstLineProcessed #:: restProcessed).map(l => expandLeadingTabs(l, contentIndent))
  }

  // Helper method to remove indentation while preserving C objects
  private def removeIndentation(line: List[C], maxIndent: Int): List[C] = {
    var virtualCol    = 0
    var pos           = 0
    var continueWhile = true

    // Count virtual columns until we reach maxIndent
    while (continueWhile && pos < line.size && virtualCol < maxIndent) {
      if (line(pos).char == ' ') {
        virtualCol += 1
      } else if (line(pos).char == '\t') {
        // Tab advances to the next tab stop (multiples of 4)
        virtualCol = (virtualCol + 4) & ~3
      } else {
        // Non-whitespace character - stop counting
        continueWhile = false
      }
      pos += 1
    }

    // Return line with indentation removed
    line.drop(pos)
  }

  private def isBlankLine(line: List[C]): Boolean = {
    line.takeWhile(_.char != '\n').forall(c => c.char == ' ' || c.char == '\t')
  }
}
