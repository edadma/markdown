package io.github.edadma.markdown

import scala.collection.mutable

// Represents data about the list as a whole
case class ListData(
    isOrdered: Boolean,
    bulletChar: Option[Char] = None, // For unordered lists: -, +, *
    startNumber: Option[Int] = None, // For ordered lists: the starting number
    delimiter: Option[Char] = None,  // For ordered lists: . or )
    isTight: Boolean = true,         // Default to tight list
)

// A list item contains one or more blocks
case class ListItem(content: List[Block])

// The list block itself, containing list data and items
case class ListBlock(data: ListData, items: List[ListItem]) extends Block {
  override def processInlines(linkRefs: Map[String, LinkReference]): Block = {
    ListBlock(
      data,
      items.map(item => ListItem(item.content.map(_.processInlines(linkRefs)))),
    )
  }
}

object ListBlockParser extends BlockParser {
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
  ): (Block, Int) = {
    // Extract list type and properties from the first line
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    val listData      = extractListData(firstLineText)

    // Collect list items and determine if the list is tight or loose
    val (items, linesConsumed, hasBlanks) = collectListItems(lines, listData, linkRefs)

    // A list is loose if there are blank lines between items or items have multiple blocks
    val isTight = !hasBlanks && items.forall(item =>
      item.content.size == 1 && item.content.head.isInstanceOf[Paragraph],
    )

    val finalListData = listData.copy(isTight = isTight)

    (ListBlock(finalListData, items), linesConsumed)
  }

  private def extractListData(line: String): ListData = {
    // Try to match unordered list marker
    UnorderedListMarker.findFirstMatchIn(line).map { m =>
      val bulletChar = m.group(2).charAt(0)
      ListData(isOrdered = false, bulletChar = Some(bulletChar))
    }.getOrElse {
      // Must be an ordered list marker
      val matches   = OrderedListMarker.findFirstMatchIn(line).get
      val number    = matches.group(2).toInt
      val delimiter = matches.group(3).charAt(0)
      ListData(isOrdered = true, startNumber = Some(number), delimiter = Some(delimiter))
    }
  }

  private def collectListItems(
      lines: List[LazyList[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
  ): (List[ListItem], Int, Boolean) = {
    val items              = new mutable.ListBuffer[ListItem]
    var currentLines       = lines
    var totalLinesConsumed = 0
    var hasBlanks          = false

    // Process each list item
    while (currentLines.nonEmpty && isMatchingListItemStart(currentLines.head, listData)) {
      // Parse a single list item
      val (item, linesConsumed, itemHasBlanks) = parseListItem(currentLines, listData, linkRefs)

      if (itemHasBlanks) {
        hasBlanks = true
      }

      items += item
      totalLinesConsumed += linesConsumed
      currentLines = currentLines.drop(linesConsumed)
    }

    (items.toList, totalLinesConsumed, hasBlanks)
  }

  private def isMatchingListItemStart(line: LazyList[C], listData: ListData): Boolean = {
    val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

    if (listData.isOrdered) {
      // Check for ordered list marker with any number but matching delimiter
      OrderedListMarker.findFirstMatchIn(lineText).exists { m =>
        m.group(3).charAt(0) == listData.delimiter.get
      }
    } else {
      // Check for unordered list marker with matching bullet
      UnorderedListMarker.findFirstMatchIn(lineText).exists { m =>
        m.group(2).charAt(0) == listData.bulletChar.get
      }
    }
  }

  private def parseListItem(
      lines: List[LazyList[C]],
      listData: ListData,
      linkRefs: mutable.Map[String, LinkReference],
  ): (ListItem, Int, Boolean) = {
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    // Get indentation information
    val (markerIndent, contentIndent) = getIndentation(firstLineText, listData)

    // Collect all lines for this list item, including continuation lines
    val (itemLines, linesConsumed, hasBlanks) = collectItemLines(lines, markerIndent, contentIndent, listData)

    // Convert item lines to plain strings for easier processing
    val rawLines = itemLines.map(line => line.takeWhile(_.char != '\n').map(_.char).mkString)

    // Process the item content (remove marker and indentation)
    val processedText = processItemText(rawLines, firstLineText, contentIndent)

    // Parse the processed text
    val reader         = new InputReader(processedText)
    val (nestedDoc, _) = parseDocument(reader.stream)

    // Create the list item with the parsed blocks
    (ListItem(nestedDoc.children), linesConsumed, hasBlanks)
  }

  private def getIndentation(line: String, listData: ListData): (Int, Int) = {
    if (listData.isOrdered) {
      val m             = OrderedListMarker.findFirstMatchIn(line).get
      val leadingIndent = m.group(1).length
      val marker        = m.group(2) + m.group(3) // number + delimiter
      val spaces        = m.group(4)

      // Content indent is marker indent + marker length + (1 or all spaces if > 4)
      val contentIndent = leadingIndent + marker.length + Math.min(spaces.length, 1)

      (leadingIndent, contentIndent)
    } else {
      val m             = UnorderedListMarker.findFirstMatchIn(line).get
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
    val itemLines = new mutable.ListBuffer[LazyList[C]]
    itemLines += lines.head

    var count                   = 1
    var currentLines            = lines.tail
    var inItem                  = true
    var blankLinesBetweenBlocks = false
    var previousWasBlank        = false

    // Track if we've seen a non-blank line after the marker
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    var hasSeenContent = if (listData.isOrdered) {
      val m = OrderedListMarker.findFirstMatchIn(firstLineText).get
      m.group(5).trim.nonEmpty // Check if there's content after the marker
    } else {
      val m = UnorderedListMarker.findFirstMatchIn(firstLineText).get
      m.group(4).trim.nonEmpty // Check if there's content after the marker
    }

    while (inItem && currentLines.nonEmpty) {
      val line     = currentLines.head
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString

      if (isBlankLine(line)) {
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

          if (isMatchingListItemStart(nextLine, listData)) {
            // Next line is a new list item - end this item
            inItem = false
            if (previousWasBlank) {
              blankLinesBetweenBlocks = true
            }
          } else if (!isBlankLine(nextLine)) {
            // Next line is not blank - check if it's indented enough
            val nextIndent = nextText.takeWhile(_ == ' ').length

            if (nextIndent < markerIndent && !isListMarker(nextText)) {
              // Not indented enough and not a new list - end of item
              inItem = false
            }
          }
        }
      } else if (isMatchingListItemStart(line, listData)) {
        // New list item - end this item
        inItem = false
      } else {
        // Regular content line - check if it belongs to this item
        val lineIndent = lineText.takeWhile(_ == ' ').length

        if (lineIndent >= contentIndent || (previousWasBlank && lineIndent > 0)) {
          // Line belongs to this item
          hasSeenContent = true

          // If previous line was blank, we have a blank line between blocks
          if (previousWasBlank) {
            blankLinesBetweenBlocks = true
          }

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

  private def processItemText(
      rawLines: List[String],
      firstLine: String,
      contentIndent: Int,
  ): String = {
    // First line: remove marker and appropriate spaces
    val firstLineProcessed = if (firstLine.matches("^ {0,3}[-+*]\\s+.*$")) {
      val m = UnorderedListMarker.findFirstMatchIn(firstLine).get
      firstLine.substring(m.end(3)) // After marker and spaces
    } else {
      val m = OrderedListMarker.findFirstMatchIn(firstLine).get
      firstLine.substring(m.end(4)) // After marker and spaces
    }

    // Rest of lines: remove appropriate indentation
    val restLinesProcessed = rawLines.tail.map { line =>
      if (line.trim.isEmpty) {
        "" // Blank line
      } else {
        // Remove up to contentIndent spaces
        val actualIndent = line.takeWhile(_ == ' ').length
        val removeCount  = Math.min(actualIndent, contentIndent)
        line.substring(removeCount)
      }
    }

    // Combine all lines
    (firstLineProcessed :: restLinesProcessed).mkString("\n")
  }

  private def isBlankLine(line: LazyList[C]): Boolean = {
    line.takeWhile(_.char != '\n').forall(c => c.char == ' ' || c.char == '\t')
  }
}
