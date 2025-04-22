package io.github.edadma.markdown

import scala.util.control.Breaks._

def parseInline(cursors: LazyList[Cursor]): List[Inline] = {
  var pos                   = 0
  var inlines: List[Inline] = Nil
  val delimiterStack        = new DelimiterStack(cursors)

  // Helper to add an inline element
  def addInline(inlineNode: Inline): Unit = {
    inlines match {
      case Text(prevContent) :: rest if inlineNode.isInstanceOf[Text] =>
        inlines = Text(prevContent + inlineNode.asInstanceOf[Text].content) :: rest
      case _ =>
        inlines = inlineNode :: inlines
    }
  }

  // Original handleAutoLink function - preserved exactly as it was
  def handleAutoLink(): Unit = {
    // If we're here, we know the first character is '<'
    // First, check if there's anything after the '<'
    if (pos + 1 >= cursors.size) {
      handlePlainText()
      return
    }

    val startPos = pos
    pos += 1 // Skip the opening <

    // Try to find the closing '>'
    var closingFound = false
    var linkEnd      = pos

    while (pos < cursors.size && !closingFound) {
      if (cursors(pos).char == '>') {
        closingFound = true
        linkEnd = pos
      }
      pos += 1
    }

    // If no closing '>', treat as plain text
    if (!closingFound) {
      pos = startPos // Reset position
      handlePlainText()
      return
    }

    // Extract link text (excluding the angle brackets)
    val linkText = cursors.slice(startPos + 1, linkEnd).map(_.char).mkString

    // Basic validation
    val isValidLink =
      linkText.contains("://") ||                                      // Looks like a URL
        (linkText.contains("@") && linkText.exists(_.isLetterOrDigit)) // Looks like an email

    if (isValidLink) {
      val destination = if (linkText.contains("@")) {
        s"mailto:$linkText"
      } else {
        linkText
      }

      addInline(AutoLink(destination, linkText))
    } else {
      // If not a valid link, treat as plain text
      pos = startPos // Reset position
      handlePlainText()
    }
  }

  // Original handleRawHTML function - preserved exactly as it was
  def handleRawHTML(): Unit = {
    val startPos = pos
    pos += 1 // Skip the opening <

    // Check if this is a valid HTML tag start
    if (
      pos < cursors.size &&
      (cursors(pos).char.isLetter ||
        cursors(pos).char == '/' ||
        cursors(pos).char == '!' ||
        cursors(pos).char == '?' ||
        cursors(pos).char == '%')
    ) {
      var depth  = 1
      var tagEnd = pos

      // Find the closing >
      while (pos < cursors.size && depth > 0) {
        if (cursors(pos).char == '<') depth += 1
        if (cursors(pos).char == '>') depth -= 1

        if (depth == 0) {
          tagEnd = pos
          pos += 1   // Include the >
          depth = -1 // Signal successful parsing
        } else {
          pos += 1
        }
      }

      if (depth == -1) {
        // Successfully parsed HTML tag
        val htmlContent = cursors.slice(startPos, pos).map(_.char).mkString
        addInline(RawHTML(htmlContent))
      } else {
        // Incomplete tag, treat as plain text
        pos = startPos
        handlePlainText()
      }
    } else {
      // Not a valid HTML tag start
      pos = startPos
      handlePlainText()
    }
  }

  // Determine if delimiter can be opener/closer based on surrounding characters
//  def determineDelimiterStatus(
//      delimChar: Char,
//      length: Int,
//      beforeChar: Option[Char],
//      afterChar: Option[Char],
//  ): (Boolean, Boolean) = {
//    // Define what counts as whitespace and punctuation
//    def isWhitespace(c: Option[Char]): Boolean = c.exists(ch => ch.isWhitespace || ch == '\n')
//    def isPunctuation(c: Option[Char]): Boolean = c.exists(ch =>
//      (ch >= '!' && ch <= '/') || (ch >= ':' && ch <= '@') ||
//        (ch >= '[' && ch <= '`') || (ch >= '{' && ch <= '~'),
//    )
//
//    // A left-flanking delimiter run:
//    // - is not followed by Unicode whitespace
//    // - and either (a) is not followed by a Unicode punctuation character
//    //   or (b) is followed by punctuation and preceded by whitespace or punctuation
//    val isLeftFlanking = !isWhitespace(afterChar) && (
//      !isPunctuation(afterChar) ||
//        (isPunctuation(afterChar) && (isWhitespace(beforeChar) || isPunctuation(beforeChar)))
//    )
//
//    // A right-flanking delimiter run:
//    // - is not preceded by Unicode whitespace
//    // - and either (a) is not preceded by a Unicode punctuation character
//    //   or (b) is preceded by punctuation and followed by whitespace or punctuation
//    val isRightFlanking = !isWhitespace(beforeChar) && (
//      !isPunctuation(beforeChar) ||
//        (isPunctuation(beforeChar) && (isWhitespace(afterChar) || isPunctuation(afterChar)))
//    )
//
//    // For * delimiter:
//    // - Can open if left-flanking
//    // - Can close if right-flanking
//    if (delimChar == '*') {
//      return (isLeftFlanking, isRightFlanking)
//    }
//
//    // For _ delimiter:
//    // - Can open if left-flanking AND
//    //   (a) not right-flanking OR (b) right-flanking and preceded by punctuation
//    // - Can close if right-flanking AND
//    //   (a) not left-flanking OR (b) left-flanking and followed by punctuation
//    val canBeOpener = isLeftFlanking && (
//      !isRightFlanking || (isRightFlanking && isPunctuation(beforeChar))
//    )
//
//    val canBeCloser = isRightFlanking && (
//      !isLeftFlanking || (isLeftFlanking && isPunctuation(afterChar))
//    )
//
//    (canBeOpener, canBeCloser)
//  }

  def determineDelimiterStatus(
      delimChar: Char,
      length: Int,
      beforeChar: Option[Char],
      afterChar: Option[Char],
  ): (Boolean, Boolean) = {
    // Define what counts as whitespace and punctuation
    def isWhitespace(c: Option[Char]): Boolean = {
      val result = c.isEmpty || c.exists(ch => ch.isWhitespace || ch == '\n')
      logger.debug(s"isWhitespace($c) = $result")
      result
    }

    def isPunctuation(c: Option[Char]): Boolean = {
      val result = c.exists(ch =>
        (ch >= '!' && ch <= '/') || (ch >= ':' && ch <= '@') ||
          (ch >= '[' && ch <= '`') || (ch >= '{' && ch <= '~'),
      )
      logger.debug(s"isPunctuation($c) = $result")
      result
    }

    logger.debug(s"Determining delimiter status for '$delimChar'")
    logger.debug(s"Before character: $beforeChar")
    logger.debug(s"After character: $afterChar")

    // A left-flanking delimiter run:
    // - is not followed by Unicode whitespace
    // - and either (a) is not followed by a Unicode punctuation character
    //   or (b) is followed by punctuation and preceded by whitespace or punctuation
    val isLeftFlanking = !isWhitespace(afterChar) && (
      !isPunctuation(afterChar) ||
        (isPunctuation(afterChar) && (isWhitespace(beforeChar) || isPunctuation(beforeChar)))
    )

    // A right-flanking delimiter run:
    // - is not preceded by Unicode whitespace
    // - and either (a) is not preceded by a Unicode punctuation character
    //   or (b) is preceded by punctuation and followed by whitespace or punctuation
    val isRightFlanking = !isWhitespace(beforeChar) && (
      !isPunctuation(beforeChar) ||
        (isPunctuation(beforeChar) && (isWhitespace(afterChar) || isPunctuation(afterChar)))
    )

    logger.debug(s"Left-flanking: $isLeftFlanking")
    logger.debug(s"Right-flanking: $isRightFlanking")

    // For * delimiter:
    // - Can open if left-flanking
    // - Can close if right-flanking
    if (delimChar == '*') {
      return (isLeftFlanking, isRightFlanking)
    }

    // For _ delimiter:
    // - Can open if left-flanking AND (not right-flanking OR preceded by punctuation)
    // - Can close if right-flanking AND (not left-flanking OR followed by punctuation)
    val canBeOpener = isLeftFlanking && (
      !isRightFlanking || isPunctuation(beforeChar)
    )

    val canBeCloser = isRightFlanking && (
      !isLeftFlanking || isPunctuation(afterChar)
    )

    logger.debug(s"Can be opener: $canBeOpener")
    logger.debug(s"Can be closer: $canBeCloser")

    (canBeOpener, canBeCloser)
  }

  // Process emphasis delimiter (* or _)
  def handleEmphasisDelimiter(delimChar: Char): Unit = {
    val startPos = pos

    // Count consecutive delimiters
    val length = countConsecutive(pos, delimChar)
    pos += length - 1 // -1 because loop will increment by 1

    // Determine if this can be opener/closer
    val beforeChar = if (startPos > 0) Some(cursors(startPos - 1).char) else None
    val afterChar  = if (pos + 1 < cursors.size) Some(cursors(pos + 1).char) else None

    val (canOpen, canClose) = determineDelimiterStatus(
      delimChar,
      length,
      beforeChar,
      afterChar,
    )

    // Add text node and update delimiter stack
    val textNode = Text(String.valueOf(delimChar) * length)
    addInline(textNode)

    val delimType = if (delimChar == '*') Asterisk else Underscore
    delimiterStack.push(startPos, delimType, length, canOpen, canClose)
  }

  // Count consecutive characters of the same type
  def countConsecutive(startPos: Int, c: Char): Int = {
    var count = 0
    var i     = startPos
    while (i < cursors.size && cursors(i).char == c && !cursors(i).isLiteral) {
      count += 1
      i += 1
    }
    count
  }

  // Handle closing bracket
  def handleCloseBracket(): Unit = {
    // Look for matching opening bracket and possibly create link/image
    val (updatedInlines, newPos) = delimiterStack.lookForLinkOrImage(inlines, pos)
    inlines = updatedInlines
    pos = newPos
  }

  // Handle code spans
  def handleCodeSpan(): Unit = {
    val startPos         = pos
    val openingBackticks = countConsecutive(pos, '`')
    pos += openingBackticks - 1 // -1 to account for loop increment

    // Find matching closing backticks
    var foundClosing = false
    var startContent = pos + 1
    var endContent   = startContent

    breakable {
      var searchPos = startContent
      while (searchPos < cursors.size) {
        if (cursors(searchPos).char == '`' && !cursors(searchPos).isLiteral) {
          val closingCount = countConsecutive(searchPos, '`')
          if (closingCount == openingBackticks) {
            foundClosing = true
            endContent = searchPos
            pos = searchPos + closingCount - 1 // -1 for loop increment
            break
          } else {
            searchPos += closingCount
          }
        } else {
          searchPos += 1
        }
      }
    }

    if (foundClosing) {
      // Extract content between backticks
      val content = cursors.slice(startContent, endContent).map(_.char).mkString

      // Process content according to spec
      val processedContent = {
        val contentWithSpaces = content.replace('\n', ' ')
        if (
          contentWithSpaces.nonEmpty &&
          contentWithSpaces.startsWith(" ") &&
          contentWithSpaces.endsWith(" ") &&
          contentWithSpaces.trim.nonEmpty
        ) {
          contentWithSpaces.substring(1, contentWithSpaces.length - 1)
        } else {
          contentWithSpaces
        }
      }

      addInline(CodeSpan(processedContent))
    } else {
      // No matching closing backticks, treat as plain text
      addInline(Text(cursors.slice(startPos, startPos + openingBackticks).map(_.char).mkString))
      pos = startPos // Reset position (will be incremented in loop)
    }
  }

  // Handle line breaks
  def handleLineBreak(): Unit = {
    // Check if it's a hard line break (preceded by two or more spaces)
    var isHardBreak  = false
    var spacesToTrim = 0

    // Check for trailing spaces (>=2)
    if (pos > 0) {
      var spacesCount = 0
      var i           = pos - 1
      while (i >= 0 && cursors(i).char == ' ') {
        spacesCount += 1
        i -= 1
      }

      if (spacesCount >= 2) {
        isHardBreak = true
        spacesToTrim = spacesCount

        // If we have trailing spaces, we need to remove them from the previous text node
        if (spacesToTrim > 0 && inlines.nonEmpty && inlines.head.isInstanceOf[Text]) {
          val textNode   = inlines.head.asInstanceOf[Text]
          val newContent = textNode.content.dropRight(spacesToTrim)

          // Replace the text node with a trimmed version
          inlines = inlines.tail
          if (newContent.nonEmpty) {
            inlines = Text(newContent) :: inlines
          }
        }
      }
    }

    // If previous cursor was a backslash, we need a hard break
    if (pos > 0 && cursors(pos - 1).char == '\\' && !cursors(pos - 1).isLiteral) {
      isHardBreak = true

      // Remove the backslash from the previous Text node
      if (inlines.nonEmpty && inlines.head.isInstanceOf[Text]) {
        val textNode   = inlines.head.asInstanceOf[Text]
        val newContent = textNode.content.dropRight(1)

        // Replace the text node with a new version without the backslash
        inlines = inlines.tail
        if (newContent.nonEmpty) {
          inlines = Text(newContent) :: inlines
        }
      }
    }

    // Add the appropriate line break
    if (isHardBreak) {
      addInline(HardLineBreak())
    } else {
      addInline(SoftLineBreak())
    }
  }

  // Plain text handling
  def handlePlainText(): Unit = {
    val startPos = pos

    // Find next special character
    var textEnd = startPos

    breakable {
      while (textEnd < cursors.size) {
        val c = cursors(textEnd)
        if (
          (c.char == '`' || c.char == '*' || c.char == '_' ||
            c.char == '[' || c.char == '!' || c.char == ']' ||
            c.char == '<' || c.char == '\n') && !c.isLiteral
        ) {
          break
        }
        textEnd += 1
      }
    }

    // Extract the text content
    if (textEnd > startPos) {
      val textContent = cursors.slice(startPos, textEnd).map(_.char).mkString
      addInline(Text(textContent))
      pos = textEnd - 1 // -1 to account for loop increment
    }
  }

  // Main loop
  while (pos < cursors.size) {
    val cursor = cursors(pos)

    if (cursor.isLiteral) {
      // If the character is literal, handle it as plain text
      addInline(Text(cursor.char.toString))
    } else if (cursor.char == '`') {
      handleCodeSpan()
    } else if (cursor.char == '*' || cursor.char == '_') {
      handleEmphasisDelimiter(cursor.char)
    } else if (cursor.char == '[') {
      // Add to delimiter stack
      addInline(Text("["))
      delimiterStack.push(pos, OpenBracket, 1, true, false)
    } else if (
      cursor.char == '!' && pos + 1 < cursors.size &&
      cursors(pos + 1).char == '[' && !cursors(pos + 1).isLiteral
    ) {
      // Image opening
      addInline(Text("!["))
      delimiterStack.push(pos, OpenImage, 1, true, false)
      pos += 1 // Skip the next '['
    } else if (cursor.char == ']') {
      handleCloseBracket()
    } else if (cursor.char == '\n') {
      handleLineBreak()
    } else if (
      cursor.char == '<' && pos + 1 < cursors.size &&
      (
        // Scheme-based URLs
        (cursors(pos + 1).char.isLetter &&
          cursors.slice(pos + 1, pos + 9).map(_.char).mkString.contains("://")) ||

          // Email addresses
          (cursors(pos + 1).char.isLetter &&
            cursors.slice(pos + 1, pos + 20)
              .takeWhile(_.char != '>')
              .count(_.char == '@') == 1)
      )
    ) {
      handleAutoLink()
    } else if (cursor.char == '<') {
      handleRawHTML()
    } else {
      handlePlainText()
    }

    pos += 1
  }

  // At the end of the document, process any remaining emphasis delimiters
  val processedInlines = delimiterStack.processEmphasis(inlines)

  // If we processed any emphasis, use those inlines, otherwise use what we've built
  val finalInlines = if (processedInlines.nonEmpty) processedInlines else inlines

  // Return inlines in correct order
  finalInlines.reverse
}
