package io.github.edadma.markdown

import scala.collection.mutable

def parseInline(cursors: mutable.Buffer[Cursor]): List[Inline] = {
  var pos                   = 0
  var inlines: List[Inline] = Nil

  // Helper to add an inline element
  def addInline(inlineNode: Inline): Unit = {
    inlines match {
      case Text(prevContent) :: rest if inlineNode.isInstanceOf[Text] =>
        inlines = Text(prevContent + inlineNode.asInstanceOf[Text].content) :: rest
      case _ =>
        inlines = inlineNode :: inlines
    }
  }

  def handlePlainText(): Unit = {
    val startPos = pos

    // Find next special character
    var textEnd     = startPos
    var shouldBreak = false

    while (shouldBreak || textEnd < cursors.size) {
      val c = cursors(textEnd)
      if (
        (c.char == '`' || c.char == '*' || c.char == '_' ||
          c.char == '[' || c.char == '!' || c.char == ']' ||
          c.char == '<' || c.char == '\n') && !c.isLiteral
      ) {
        shouldBreak = true
      }
      textEnd += 1
    }

    // Extract the text content
    if (textEnd > startPos) {
      val textContent = cursors.slice(startPos, textEnd).map(_.char).mkString
      addInline(Text(textContent))
      pos = textEnd - 1 // -1 to account for loop increment
    }
  }

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

  // Process plain text until we hit a special character
  def processTextUntil(stopChars: Set[Char]): Unit = {
    val startPos = pos
    var textEnd  = startPos

    while (
      textEnd < cursors.size &&
      !stopChars.contains(cursors(textEnd).char) &&
      !cursors(textEnd).isLiteral
    ) {
      textEnd += 1
    }

    // Extract the text content
    if (textEnd > startPos) {
      val textContent = cursors.slice(startPos, textEnd).map(_.char).mkString
      addInline(Text(textContent))
      pos = textEnd - 1 // -1 to account for loop increment
    }
  }

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

  // Handle code spans (highest precedence)
  def handleCodeSpan(): Unit = {
    val startPos         = pos
    val openingBackticks = countConsecutive(pos, '`')
    pos += openingBackticks - 1 // -1 to account for loop increment

    // Find matching closing backticks
    var foundClosing = false
    var startContent = pos + 1
    var endContent   = startContent

    var searchPos = startContent
    while (searchPos < cursors.size && !foundClosing) {
      if (cursors(searchPos).char == '`' && !cursors(searchPos).isLiteral) {
        val closingCount = countConsecutive(searchPos, '`')
        if (closingCount == openingBackticks) {
          foundClosing = true
          endContent = searchPos
          pos = searchPos + closingCount - 1 // -1 for loop increment
        } else {
          searchPos += closingCount
        }
      } else {
        searchPos += 1
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

  // Handle line breaks
  def handleLineBreak(): Unit = {
    // Check for hard break (preceded by 2+ spaces or backslash)
    var isHardBreak = false

    // Check for trailing spaces
    if (pos > 0 && inlines.headOption.exists(_.isInstanceOf[Text])) {
      val textNode    = inlines.head.asInstanceOf[Text]
      var spacesCount = 0
      var i           = textNode.content.length - 1
      while (i >= 0 && textNode.content(i) == ' ') {
        spacesCount += 1
        i -= 1
      }

      if (spacesCount >= 2) {
        isHardBreak = true
        // Trim trailing spaces from previous text node
        inlines = inlines.tail
        if (i >= 0) {
          inlines = Text(textNode.content.substring(0, i + 1)) :: inlines
        }
      }
    }

    // Check for backslash
    if (pos > 0 && cursors(pos - 1).char == '\\' && !cursors(pos - 1).isLiteral) {
      isHardBreak = true

      // Remove backslash from previous text node if it exists
      if (inlines.headOption.exists(_.isInstanceOf[Text])) {
        val textNode = inlines.head.asInstanceOf[Text]
        inlines = inlines.tail
        if (textNode.content.length > 1) {
          inlines = Text(textNode.content.dropRight(1)) :: inlines
        }
      }
    }

    // Add appropriate line break
    if (isHardBreak) {
      addInline(HardLineBreak())
    } else {
      addInline(SoftLineBreak())
    }
  }

  // Main loop that processes characters one by one
  while (pos < cursors.size) {
    val cursor = cursors(pos)

    if (cursor.isLiteral) {
      // Literally interpret this character (e.g., from a backslash escape)
      addInline(Text(cursor.char.toString))
      pos += 1
    } else {
      // Process based on character type
      cursor.char match {
        case '`' =>
          // Code spans (highest precedence)
          handleCodeSpan()
          pos += 1

        case '<'
            if pos + 1 < cursors.size &&
              ((cursors(pos + 1).char.isLetter &&
                cursors.slice(pos + 1, Math.min(pos + 20, cursors.size)).map(_.char).mkString.contains("://")) ||
                (cursors(pos + 1).char.isLetter &&
                  cursors.slice(pos + 1, Math.min(pos + 20, cursors.size))
                    .takeWhile(_.char != '>')
                    .count(_.char == '@') == 1)) =>
          // Autolinks (also high precedence)
          handleAutoLink()
          pos += 1

        case '<' =>
          // Raw HTML (also high precedence)
          handleRawHTML()
          pos += 1

        case '\n' =>
          // Line break
          handleLineBreak()
          pos += 1

        case _ =>
          // Regular text - process until we hit a special character
          processTextUntil(Set('`', '<', '*', '_', '!', '[', ']', '\n'))
          pos += 1
      }
    }
  }

  // Return the inlines in correct order
  inlines.reverse
}
