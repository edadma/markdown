package io.github.edadma.markdown

def parseInline(cursors: LazyList[Cursor]): List[Inline] = {
  var pos                   = 0
  var inlines: List[Inline] = Nil

  // Add an inline element to our result list
  def addInline(inlineNode: Inline): Unit = {
    inlines = inlineNode :: inlines
  }

  // Process a code span starting at the current position
  def handleCodeSpan(): Unit = {
    val startPos         = pos
    val openingBackticks = countConsecutive(pos, '`')
    pos += openingBackticks

    // Find matching closing backticks
    val startContent = pos
    var foundClosing = false
    var endContent   = pos

    while (pos < cursors.size && !foundClosing) {
      if (cursors(pos).char == '`') {
        val closingBackticks = countConsecutive(pos, '`')
        if (closingBackticks == openingBackticks) {
          endContent = pos
          pos += closingBackticks
          foundClosing = true
        } else {
          pos += closingBackticks
        }
      } else {
        pos += 1
      }
    }

    if (foundClosing) {
      // Extract content between backticks
      val content = cursors.slice(startContent, endContent).map(_.char).mkString

      // Process content according to spec
      val processedContent = {
        val contentWithSpaces = content.replace('\n', ' ')
        if (
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
      pos -= 1 // Adjust for the loop increment
    } else {
      // No matching closing backticks
      // Reset position and collect unmatched backticks and subsequent text as a single node
      pos = startPos

      // Find where the next delimiter character or end of input is
      var textEnd            = startPos + openingBackticks
      var continueCollecting = true

      while (textEnd < cursors.size && continueCollecting) {
        val c = cursors(textEnd)
        if ((c.char == '`' || c.char == '[' || c.char == '!' || c.char == '\n') && !c.isLiteral) {
          continueCollecting = false
        } else {
          textEnd += 1
        }
      }

      // Create text node with backticks and subsequent text
      val textContent = cursors.slice(startPos, textEnd).map(_.char).mkString
      addInline(Text(textContent))

      // Update position
      pos = textEnd - 1 // Adjust for the loop increment
    }
  }

  // Look for a run of characters and return its length
  def countConsecutive(startPos: Int, c: Char): Int = {
    var count = 0
    var i     = startPos
    while (i < cursors.size && cursors(i).char == c) {
      count += 1
      i += 1
    }
    count
  }

  // Direct implementation of link parsing without using delimiter stack
  def handleLink(): Unit = {
    val startPos = pos
    pos += 1 // Skip the opening [

    // Collect the link text
    val textStart = pos
    var textEnd   = pos
    var depth     = 1 // Track nested brackets

    // Find the closing bracket
    while (pos < cursors.size && depth > 0) {
      val c = cursors(pos)
      if (c.char == '[' && !c.isLiteral) depth += 1
      else if (c.char == ']' && !c.isLiteral) depth -= 1

      if (depth > 0) pos += 1
    }

    if (depth == 0) {
      // Found closing bracket
      textEnd = pos
      pos += 1 // Skip the closing bracket

      // Check if we have a link destination
      if (pos < cursors.size && cursors(pos).char == '(' && !cursors(pos).isLiteral) {
        // Parse link destination
        pos += 1 // Skip the opening paren

        // Skip whitespace
        while (pos < cursors.size && cursors(pos).char.isWhitespace) {
          pos += 1
        }

        // Collect destination
        val destStart          = pos
        var destEnd            = pos
        var parenDepth         = 0
        var continueCollecting = true

        while (pos < cursors.size && continueCollecting) {
          val c = cursors(pos)

          if (c.char == '(' && !c.isLiteral) {
            parenDepth += 1
            pos += 1
          } else if (c.char == ')' && !c.isLiteral) {
            if (parenDepth == 0) {
              // End of destination
              continueCollecting = false
            } else {
              parenDepth -= 1
              pos += 1
            }
          } else if (c.char.isWhitespace && parenDepth == 0) {
            // Whitespace outside parentheses marks end of destination
            continueCollecting = false
          } else {
            pos += 1
          }
        }

        destEnd = pos

        // Extract the destination
        val destination = cursors.slice(destStart, destEnd).map(_.char).mkString

        // Check for a title
        var title: Option[String] = None

        // Skip whitespace
        while (pos < cursors.size && cursors(pos).char.isWhitespace) {
          pos += 1
        }

        // Check for a title
        if (pos < cursors.size && (cursors(pos).char == '"' || cursors(pos).char == '\'' || cursors(pos).char == '(')) {
          val titleDelim   = cursors(pos).char
          val closingDelim = if (titleDelim == '(') ')' else titleDelim

          pos += 1 // Skip opening delimiter
          val titleStart = pos

          // Find closing delimiter
          while (
            pos < cursors.size &&
            cursors(pos).char != closingDelim &&
            cursors(pos).char != '\n'
          ) {
            pos += 1
          }

          if (pos < cursors.size && cursors(pos).char == closingDelim) {
            title = Some(cursors.slice(titleStart, pos).map(_.char).mkString)
            pos += 1 // Skip closing delimiter
          }
        }

        // Skip to closing paren
        while (pos < cursors.size && cursors(pos).char != ')') {
          pos += 1
        }

        if (pos < cursors.size && cursors(pos).char == ')') {
          pos += 1 // Skip closing paren

          // Extract the link text
          val linkText = cursors.slice(textStart, textEnd).map(_.char).mkString

          // Parse the link text recursively
          val textReader  = new InputReader(linkText)
          val textStream  = textReader.stream.takeWhile(_ != EndOfInput)
          val textInlines = parseInline(textStream)

          // Create the link node
          addInline(Link(destination, title, textInlines))
        } else {
          // No closing paren, treat as plain text
          handlePlainText(startPos)
        }
      } else {
        // No link destination, treat as plain text
        handlePlainText(startPos)
      }
    } else {
      // No closing bracket, treat as plain text
      handlePlainText(startPos)
    }
  }

  def handlePlainText(revertPos: Int): Unit = {
    // Reset position and treat as plain text
    pos = revertPos
    addInline(Text(cursors(pos).char.toString))
  }

  def handleImage(): Unit = {
    val startPos = pos
    pos += 2 // Skip the ![ prefix

    // Collect the image alt text
    val textStart = pos
    var textEnd   = pos
    var depth     = 1 // Track nested brackets

    // Find the closing bracket
    while (pos < cursors.size && depth > 0) {
      val c = cursors(pos)
      if (c.char == '[' && !c.isLiteral) depth += 1
      else if (c.char == ']' && !c.isLiteral) depth -= 1

      if (depth > 0) pos += 1
    }

    if (depth == 0) {
      // Found closing bracket
      textEnd = pos
      pos += 1 // Skip the closing bracket

      // Check if we have an image destination
      if (pos < cursors.size && cursors(pos).char == '(' && !cursors(pos).isLiteral) {
        // Parse image destination
        pos += 1 // Skip the opening paren

        // Skip whitespace
        while (pos < cursors.size && cursors(pos).char.isWhitespace) {
          pos += 1
        }

        // Collect destination
        val destStart          = pos
        var destEnd            = pos
        var parenDepth         = 0
        var continueCollecting = true

        while (pos < cursors.size && continueCollecting) {
          val c = cursors(pos)

          if (c.char == '(' && !c.isLiteral) {
            parenDepth += 1
            pos += 1
          } else if (c.char == ')' && !c.isLiteral) {
            if (parenDepth == 0) {
              // End of destination
              continueCollecting = false
            } else {
              parenDepth -= 1
              pos += 1
            }
          } else if (c.char.isWhitespace && parenDepth == 0) {
            // Whitespace outside parentheses marks end of destination
            continueCollecting = false
          } else {
            pos += 1
          }
        }

        destEnd = pos

        // Extract the destination
        val destination = cursors.slice(destStart, destEnd).map(_.char).mkString

        // Check for a title
        var title: Option[String] = None

        // Skip whitespace
        while (pos < cursors.size && cursors(pos).char.isWhitespace) {
          pos += 1
        }

        // Check for a title
        if (pos < cursors.size && (cursors(pos).char == '"' || cursors(pos).char == '\'' || cursors(pos).char == '(')) {
          val titleDelim   = cursors(pos).char
          val closingDelim = if (titleDelim == '(') ')' else titleDelim

          pos += 1 // Skip opening delimiter
          val titleStart = pos

          // Find closing delimiter
          while (
            pos < cursors.size &&
            cursors(pos).char != closingDelim &&
            cursors(pos).char != '\n'
          ) {
            pos += 1
          }

          if (pos < cursors.size && cursors(pos).char == closingDelim) {
            title = Some(cursors.slice(titleStart, pos).map(_.char).mkString)
            pos += 1 // Skip closing delimiter
          }
        }

        // Skip to closing paren
        while (pos < cursors.size && cursors(pos).char != ')') {
          pos += 1
        }

        if (pos < cursors.size && cursors(pos).char == ')') {
          pos += 1 // Skip closing paren

          // Extract the alt text
          val altText = cursors.slice(textStart, textEnd).map(_.char).mkString

          // Parse the alt text recursively
          val textReader  = new InputReader(altText)
          val textStream  = textReader.stream.takeWhile(_ != EndOfInput)
          val textInlines = parseInline(textStream)

          // Create the image node
          addInline(Image(destination, title, textInlines))
        } else {
          // No closing paren, treat as plain text
          handlePlainText(startPos)
        }
      } else {
        // No image destination, treat as plain text
        handlePlainText(startPos)
      }
    } else {
      // No closing bracket, treat as plain text
      handlePlainText(startPos)
    }
  }

  // Main function for text collection
  def collectText(startPos: Int): (String, Int) = {
    var textEnd            = startPos
    var continueCollecting = true

    // Find next special character
    while (textEnd < cursors.size && continueCollecting) {
      val c = cursors(textEnd)
      if ((c.char == '`' || c.char == '[' || c.char == '!' || c.char == '\n') && !c.isLiteral) {
        continueCollecting = false
      } else {
        textEnd += 1
      }
    }

    // Extract the text content
    val rawContent = cursors.slice(startPos, textEnd).map(_.char).mkString

    // Return the content and updated position
    (rawContent, textEnd)
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
          inlines = Text(newContent) :: inlines
        }
      }
    }

    // If previous cursor was a backslash, we need a hard break
    if (pos > 0 && cursors(pos - 1).char == '\\' && !cursors(pos - 1).isLiteral) {
      isHardBreak = true
    }

    // Add the appropriate line break
    if (isHardBreak) {
      addInline(HardLineBreak())
    } else {
      addInline(SoftLineBreak())
    }
  }

  // Main loop - process each cursor
  while (pos < cursors.size) {
    val cursor = cursors(pos)

    if (cursor.char == '`' && !cursor.isLiteral) {
      handleCodeSpan()
    } else if (cursor.char == '[' && !cursor.isLiteral) {
      handleLink()
    } else if (
      cursor.char == '!' && pos + 1 < cursors.size &&
      cursors(pos + 1).char == '[' && !cursor.isLiteral
    ) {
      handleImage()
    } else if (cursor.char == '\n') {
      handleLineBreak()
    } else {
      // Plain text - collect consecutive text characters
      val (textContent, newPos) = collectText(pos)
      addInline(Text(textContent))

      // Update position (subtract 1 because loop will increment)
      pos = newPos - 1
    }

    pos += 1
  }

  // Remove EndOfInput and return inlines in correct order
  inlines.reverse
}
