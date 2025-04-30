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

  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.isEmpty) return false

    val lineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    // Check if this line can start a list
    isListMarker(lineText)
  }

  private def isListMarker(line: String): Boolean = {
    // Check for unordered list marker
    UnorderedListMarker.findFirstMatchIn(line).exists { m =>
      val whitespace = m.group(3)
      whitespace.nonEmpty // Marker must be followed by at least one space/tab
    } ||
    // Check for ordered list marker
    OrderedListMarker.findFirstMatchIn(line).exists { m =>
      val whitespace = m.group(4)
      whitespace.nonEmpty // Marker must be followed by at least one space/tab
    }
  }

  def parse(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int = 0,
  ): (Block, Int) = {
    // Extract list type and properties from the first line
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val listData      = extractListData(firstLineText)

    // Collect list items and determine if the list is tight or loose
    val (items, linesConsumed, hasBlanks) = collectListItems(lines, listData, linkRefs, parentIndent)

    // A list is loose if there are blank lines between items or items have multiple blocks
    val isTight = items.size == 1 || !hasBlanks

    val finalListData = listData.copy(isTight = isTight)

    (ListBlock(finalListData, items), linesConsumed)
  }

  private def extractListData(line: String): ListData = {
    // Try to match unordered list marker
    UnorderedListMarker.findFirstMatchIn(line).map { m =>
      val leading    = m.group(1).length
      val bulletChar = m.group(2).charAt(0)
      ListData(isOrdered = false, bulletChar = Some(bulletChar), indent = leading)
    }.getOrElse {
      // Must be an ordered list marker
      val matches   = OrderedListMarker.findFirstMatchIn(line).get
      val leading   = matches.group(1).length
      val number    = matches.group(2).toInt
      val delimiter = matches.group(3).charAt(0)
      ListData(isOrdered = true, startNumber = Some(number), delimiter = Some(delimiter), indent = leading)
    }
  }

  private def collectListItems(
      lines: List[LazyList[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int = 0,
  ): (List[ListItem], Int, Boolean) = {
    val items              = new mutable.ListBuffer[ListItem]
    var currentLines       = lines
    var totalLinesConsumed = 0
    var hasBlanks          = false

    // Process each list item
    while (currentLines.nonEmpty && isMatchingListItemStart(currentLines.head, listData)) {
      // Parse a single list item
      val (item, linesConsumed, itemHasBlanks) = parseListItem(currentLines, listData, linkRefs, parentIndent)

      if (itemHasBlanks) {
        hasBlanks = true
      }

      items += item
      totalLinesConsumed += linesConsumed
      currentLines = currentLines.drop(linesConsumed)

      // Check for blank lines BETWEEN items (the key fix)
      if (items.size > 0 && currentLines.nonEmpty && isMatchingListItemStart(currentLines.head, listData)) {
        // Look for blank lines between the end of current item and start of next item
        val blankLinesBetween =
          currentLines.takeWhile(line => isBlankLine(line) && !isMatchingListItemStart(line, listData)).size

        if (blankLinesBetween > 0) {
          hasBlanks = true
        }
      }
    }

    (items.toList, totalLinesConsumed, hasBlanks)
  }

  private def isMatchingListItemStart(line: LazyList[C], listData: ListData): Boolean = {
    // Convert the line to String (excluding newline)
    val text = line.takeWhile(_.char != '\n').map(_.char).mkString

    if (!listData.isOrdered) {
      // For unordered lists, we need to match bullet char but allow for
      // indentation variations within a reasonable range (0-3 spaces)
      UnorderedListMarker.findFirstMatchIn(text).exists { m =>
        val leading    = m.group(1).length
        val bulletChar = m.group(2).charAt(0)

        // Same bullet character and indentation within reasonable range
        bulletChar == listData.bulletChar.get &&
        math.abs(leading - listData.indent) <= 3
      }
    } else {
      // For ordered lists, allow similar indentation flexibility
      OrderedListMarker.findFirstMatchIn(text).exists { m =>
        val leading   = m.group(1).length
        val delimiter = m.group(3).charAt(0)

        delimiter == listData.delimiter.get &&
        math.abs(leading - listData.indent) <= 3
      }
    }
  }

  private def parseListItem(
      lines: List[LazyList[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int = 0,
  ): (ListItem, Int, Boolean) = {
    // Get indentation information from first line
    val (markerIndent, contentIndent) = getIndentation(lines.head, listData)

    // Collect all lines for this list item, including continuation lines
    val (itemLines, linesConsumed, hasBlanks) = collectItemLines(lines, markerIndent, contentIndent, listData)

    // Process the item content to remove marker and adjust indentation
    // but KEEP the original C objects to preserve escaping information
    val processedLines = processItemLines(itemLines, contentIndent, listData)

    // Use the existing block parsing machinery directly, without creating a new document
    val totalIndent = parentIndent + contentIndent

    // Pass the total indent to any recursive list parsing
    val itemBlocks = processLines(processedLines, linkRefs, totalIndent).map {
      case nestedList: ListBlock =>
        // Use the total combined indentation
        nestedList.copy(data = nestedList.data.copy(indent = nestedList.data.indent + totalIndent))
      case other => other
    }

    // Create the list item with the parsed blocks
    (ListItem(itemBlocks), linesConsumed, hasBlanks)
  }

  private def getIndentation(line: LazyList[C], listData: ListData): (Int, Int) = {
    val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

    if (listData.isOrdered) {
      val m             = OrderedListMarker.findFirstMatchIn(lineText).get
      val leadingIndent = m.group(1).length
      val marker        = m.group(2) + m.group(3) // number + delimiter
      val spaces        = m.group(4)

      // Content indent is marker indent + marker length + (1 or all spaces if > 4)
      val contentIndent = leadingIndent + marker.length + Math.min(spaces.length, 1)

      (leadingIndent, contentIndent)
    } else {
      val m             = UnorderedListMarker.findFirstMatchIn(lineText).get
      val leadingIndent = m.group(1).length
      val marker        = m.group(2) // bullet character
      val spaces        = m.group(3)

      // Content indent is marker indent + marker length + (1 or all spaces if > 4)
      val contentIndent = leadingIndent + marker.length + Math.min(spaces.length, 1)

      (leadingIndent, contentIndent)
    }
  }

  private def collectItemLines(
      lines: List[LazyList[C]],
      markerIndent: Int,
      contentIndent: Int,
      listData: ListData,
  ): (List[LazyList[C]], Int, Boolean) = {
    def isSameListItem(line: String, markerIndent: Int): Boolean = {
      val lineIndent = countLeadingSpaces(line)
      // If indentation is within 3 spaces of the original list marker indent,
      // and it matches the list marker type, consider it the same list level
      math.abs(lineIndent - markerIndent) <= 3 && isListMarker(line) &&
      (if (!listData.isOrdered)
         UnorderedListMarker.findFirstMatchIn(line).exists(_.group(2).charAt(0) == listData.bulletChar.get)
       else
         OrderedListMarker.findFirstMatchIn(line).exists(_.group(3).charAt(0) == listData.delimiter.get))
    }

    val itemLines = new mutable.ListBuffer[LazyList[C]]
    itemLines += lines.head

    var count                   = 1
    var currentLines            = lines.tail
    var inItem                  = true
    var blankLinesBetweenBlocks = false
    var previousWasBlank        = false
    var inParagraph             = true // Track if we're in a paragraph (for lazy continuation)

    // Track if we've seen a non-blank line after the marker
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    var hasSeenContent = if (listData.isOrdered) {
      val m = OrderedListMarker.findFirstMatchIn(firstLineText).get
      m.group(5).trim.nonEmpty // Check if there's content after the marker
    } else {
      val m = UnorderedListMarker.findFirstMatchIn(firstLineText).get
      m.group(4).trim.nonEmpty // Check if there's content after the marker
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

          if (isListItemStart(nextText, markerIndent)) {
            // Next line is a new list item at same nesting level - end this item
            inItem = false
            if (previousWasBlank) {
              blankLinesBetweenBlocks = true
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
      } else if (isListItemStart(lineText, markerIndent)) {
        // New list item at same level - end this item
        inItem = false
      } else {
        // Regular content line - check if it belongs to this item
        val lineIndent = countLeadingSpaces(lineText)

        if (isSameListItem(lineText, markerIndent)) {
          // This is a new item at the SAME list level - end current item
          inItem = false
        } else if (isPotentialNestedListItem(lineText, lineIndent)) {
          // This is likely a nested list item - include it in this item
          hasSeenContent = true

          // If previous line was blank, we have a blank line between blocks
          if (previousWasBlank) {
            blankLinesBetweenBlocks = true
          }

          previousWasBlank = false
          itemLines += line
          count += 1
          currentLines = currentLines.tail
        } else if (lineIndent >= contentIndent || (previousWasBlank && lineIndent > 0)) {
          // Properly indented line - clearly part of this item
          inParagraph = true // Start/continue paragraph
          // Line belongs to this item as content
          hasSeenContent = true

          // If previous line was blank, we have a blank line between blocks
          if (previousWasBlank) {
            blankLinesBetweenBlocks = true
          }

          previousWasBlank = false
          itemLines += line
          count += 1
          currentLines = currentLines.tail
        } else if (inParagraph) {
          // Lazy continuation: include non-marker line if it continues a paragraph
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

    (itemLines.toList, count, blankLinesBetweenBlocks)
  }

  // Helper function to determine if a line starts a list item at the specified indent level
  private def isListItemStart(lineText: String, markerIndent: Int): Boolean = {
    // For determining if a line starts a new list item at the same level as current list
    val leadingIndent = countLeadingSpaces(lineText)

    if (leadingIndent != markerIndent) return false

    UnorderedListMarker.findFirstMatchIn(lineText).exists { m =>
      val indent = m.group(1).length
      indent == markerIndent && m.group(3).nonEmpty // Marker followed by whitespace
    } ||
    OrderedListMarker.findFirstMatchIn(lineText).exists { m =>
      val indent = m.group(1).length
      indent == markerIndent && m.group(4).nonEmpty // Marker followed by whitespace
    }
  }

  // Helper function to count leading spaces
  private def countLeadingSpaces(text: String): Int = {
    text.takeWhile(_ == ' ').length
  }

  private def processItemLines(
      itemLines: List[LazyList[C]],
      contentIndent: Int,
      listData: ListData,
  ): List[LazyList[C]] = {
    // Process first line - remove marker and appropriate spaces
    val lineText = itemLines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val firstLineProcessed =
      if (listData.isOrdered) {
        val m            = OrderedListMarker.findFirstMatchIn(lineText).get
        val markerEndPos = m.end(4) // End position after marker and initial spaces
        itemLines.head.drop(markerEndPos)
      } else {
        val m            = UnorderedListMarker.findFirstMatchIn(lineText).get
        val markerEndPos = m.end(3) // End position after marker and initial spaces
        itemLines.head.drop(markerEndPos)
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

    firstLineProcessed :: restProcessed
  }

  // Helper method to remove indentation while preserving C objects
  private def removeIndentation(line: LazyList[C], maxIndent: Int): LazyList[C] = {
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

  private def isBlankLine(line: LazyList[C]): Boolean = {
    line.takeWhile(_.char != '\n').forall(c => c.char == ' ' || c.char == '\t')
  }
}
