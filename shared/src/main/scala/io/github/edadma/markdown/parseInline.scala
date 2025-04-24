package io.github.edadma.markdown

import io.github.edadma.dllist.DLList

import scala.annotation.tailrec

// Standalone inline parsing function
def parseInline(inlines: List[Inline]): List[Inline] = {
  // Create initial DLList with character nodes
  val inlineNodes = DLList[Inline](inlines*)

  // Initialize empty delimiter stack (will be used for emphasis/links)
//  val delimiterStack = DLList[DelimiterInfo]()

  def processCodeSpan(node: inlineNodes.Node): inlineNodes.Node = {
    logger.debug(s"Starting processCodeSpan on node: ${node.element}")
    // Count the consecutive backticks in the opening delimiter
    val openingNode  = node
    var openingCount = 0
    var current      = node

    // Count consecutive backticks in the opening delimiter
    while (
      current.notAfterEnd &&
      current.element.isInstanceOf[Cursor] &&
      current.element.asInstanceOf[Cursor].char == '`' &&
      !current.element.asInstanceOf[Cursor].isLiteral
    ) {
      openingCount += 1
      current = current.following
    }

    logger.debug(s"Found opening delimiter with $openingCount backticks")

    // If we found an opening delimiter, look for a matching closing one
    if (openingCount > 0) {
      // Remember where content starts
      val contentStart = current
      var foundClosing = false
      var reachedEnd   = false

      // Look for closing delimiter
      while (current.notAfterEnd && !foundClosing && !reachedEnd) {
        logger.debug(s"Checking node for closing: ${current.element}")

        if (
          current.element.isInstanceOf[Cursor] &&
          current.element.asInstanceOf[Cursor].char == '`'
        ) {
          // Count consecutive backticks to see if we have a match
          var closingCount = 0
          var closingStart = current

          @tailrec
          def closeCount(): Unit = {
            if current.notAfterEnd &&
              current.element.isInstanceOf[Cursor] &&
              current.element.asInstanceOf[Cursor].char == '`' &&
              closingCount < openingCount
            then
              if current.element.asInstanceOf[Cursor].isLiteral && closingCount == 0 then
                closingStart = current.follow(current.element.asInstanceOf[Cursor].copy())
                current.element = current.element.asInstanceOf[Cursor].copy(char = '\\', isLiteral = false)
                closingCount += 1
                current = current.following.following
                closeCount()
              else if current.element.asInstanceOf[Cursor].isLiteral then
                current = current.following
              else
                closingCount += 1
                current = current.following
                closeCount()
          }

          closeCount()

          logger.debug(s"Found potential closing delimiter with $closingCount backticks")

          // If counts match, we found our closing delimiter
          if (closingCount == openingCount) {
            foundClosing = true
            val contentEnd = closingStart

            // Extract and process content
            val content = extractAndProcessCodeSpanContent(contentStart, contentEnd)
            logger.debug(s"Extracted code span content: '$content'")

            // Replace the opening node with a CodeSpan and unlink everything in between
            openingNode.element = CodeSpan(content)

            // Unlink everything from after opening delimiter to end of closing delimiter
            if (openingNode.following != current) {
              openingNode.following.unlinkUntil(current)
            }

            // Return the CodeSpan node for continued processing
            return openingNode
          }
          // If counts don't match, continue searching
        } else if (current.following.isAfterEnd) {
          // Check if the next node would be the end sentinel
          // This is the fix - set a flag to exit the loop when we're at the last node
          logger.debug("Reached end of input while searching for closing delimiter")
          reachedEnd = true
        } else {
          current = current.following
        }
      }

      logger.debug("No matching closing delimiter found, returning original node")
      // If no matching closing delimiter found, just return the original node unchanged
      // The opening backticks will be treated as regular text
      return node
    }

    // If we somehow got here, just return the original node
    node
  }

  // Helper function to extract and process code span content according to spec
  def extractAndProcessCodeSpanContent(start: DLList[Inline]#Node, end: DLList[Inline]#Node): String = {
    // Build the content string from nodes between start and end
    val builder = new StringBuilder
    var current = start

    while (current != end) {
      current.element match {
        case c: Cursor =>
          // Convert newlines to spaces
          if (c.char == '\n') {
            builder.append(' ')
          } else {
            builder.append(c.char)
          }
        case t: Text => builder.append(t.content)
        case _       => // Other inline elements shouldn't be here, but ignore if they are
      }
      current = current.following
    }

    val content = builder.toString

    // Handle special case: if content begins and ends with a space, and isn't all spaces,
    // remove one space from each end
    if (
      content.nonEmpty &&
      content.startsWith(" ") &&
      content.endsWith(" ") &&
      content.trim.nonEmpty
    ) {
      content.substring(1, content.length - 1)
    } else {
      content
    }
  }

  def processLineBreak(node: inlineNodes.Node): inlineNodes.Node = {
    logger.debug(s"Processing line break at node: ${node.element}")

    // Check for hard break - backslash escape
    if (
      node.preceding.notBeforeStart &&
      node.preceding.element.isInstanceOf[Cursor] &&
      node.preceding.element.asInstanceOf[Cursor].char == '\\' &&
      !node.preceding.element.asInstanceOf[Cursor].isLiteral
    ) {

      logger.debug("Found hard break with backslash")

      // Remove the backslash
      val backslashNode = node.preceding
      backslashNode.unlink

      // Replace the newline with a HardLineBreak
      node.element = HardLineBreak()
      return node
    }

    // Check for hard break - two or more spaces
    var spaceCount = 0
    var current    = node.preceding

    // Count trailing spaces before the newline
    while (
      current.notBeforeStart &&
      current.element.isInstanceOf[Cursor] &&
      current.element.asInstanceOf[Cursor].char == ' ' &&
      !current.element.asInstanceOf[Cursor].isLiteral
    ) {
      spaceCount += 1
      current = current.preceding
    }

    if (spaceCount >= 2) {
      logger.debug(s"Found hard break with $spaceCount spaces")

      // Replace the newline with a HardLineBreak
      node.element = HardLineBreak()

      // Remove the trailing spaces
      var spacesToRemove = spaceCount
      var looping        = true

      while (looping && spacesToRemove > 0 && node.preceding.notBeforeStart) {
        if (
          node.preceding.element.isInstanceOf[Cursor] &&
          node.preceding.element.asInstanceOf[Cursor].char == ' '
        ) {
          node.preceding.unlink
          spacesToRemove -= 1
        } else {
          looping = false // If we hit a non-space, stop removing
        }
      }

      return node
    }

    // If we get here, it's a soft break
    logger.debug("Creating soft line break")
    node.element = SoftLineBreak()
    return node
  }

  def processHtmlOrAutolink(node: inlineNodes.Node): inlineNodes.Node = {
    logger.debug(s"Starting HTML/autolink processing on node: ${node.element}")

    // Find the closing '>' if it exists
    val openingNode = node
    var current     = node.following
    var content     = new StringBuilder()

    // Look ahead to find a potential closing '>'
    while (
      current.notAfterEnd &&
      !(current.element.isInstanceOf[Cursor] &&
        current.element.asInstanceOf[Cursor].char == '>' &&
        !current.element.asInstanceOf[Cursor].isLiteral)
    ) {

      current.element match {
        case c: Cursor =>
          // For autolinks, we can't have line endings
          if (c.char == '\n') {
            logger.debug("Line ending found in potential autolink/HTML - treating as literal")
            return node // Return original node unchanged
          }
          content.append(c.char)
        case _ => return node // Non-cursor element found, not a valid autolink/HTML
      }
      current = current.following
    }

    // If we didn't find a closing '>', return original
    if (
      current.isAfterEnd ||
      !(current.element.isInstanceOf[Cursor] &&
        current.element.asInstanceOf[Cursor].char == '>')
    ) {
      logger.debug("No closing '>' found")
      return node
    }

    val contentStr = content.toString()

    // Check for URI autolink
    if (isAbsoluteUri(contentStr)) {
      logger.debug(s"Found URI autolink: $contentStr")
      openingNode.element = AutoLink(contentStr, contentStr)

      // Remove everything between opening < and closing >
      if (openingNode.following != current.following) {
        openingNode.following.unlinkUntil(current.following)
      }

      return openingNode
    }

    // Check for email autolink
    else if (isEmailAddress(contentStr)) {
      logger.debug(s"Found email autolink: $contentStr")
      openingNode.element = AutoLink(s"mailto:$contentStr", contentStr)

      // Remove everything between opening < and closing >
      if (openingNode.following != current.following) {
        openingNode.following.unlinkUntil(current.following)
      }

      return openingNode
    }

    // Check for HTML tag
    else if (isHtmlTag(contentStr)) {
      logger.debug(s"Found HTML tag: $contentStr")
      openingNode.element = RawHTML(s"<$contentStr>")

      // Remove everything between opening < and closing >
      if (openingNode.following != current.following) {
        openingNode.following.unlinkUntil(current.following)
      }

      return openingNode
    }

    // Not a valid autolink or HTML tag, treat as literal
    logger.debug("Not a valid autolink or HTML tag")
    return node
  }

  // Main processing loop - single pass through the document
  if (inlineNodes.nonEmpty) {
    var current = inlineNodes.headNode

    while (current.notAfterEnd) {
      current.element match {
        case c: Cursor if !c.isLiteral =>
          c.char match {
            case '`' =>
              // Process code span (highest precedence)
              val oldCurrent = current // Remember the current node

              current = processCodeSpan(current)

              if (current == oldCurrent) {
                current = current.following
              }

            case '<' =>
              val oldCurrent = current

              current = processHtmlOrAutolink(current)

              if (current == oldCurrent) {
                current = current.following
              }

//            case '*' | '_' =>
//              // Add to delimiter stack for emphasis processing
//              val delimiterInfo = analyzeDelimiter(current, inlineNodes)
//              delimiterStack.append(delimiterInfo)
//              current = current.following
//
//            case '[' =>
//              // Add to delimiter stack as potential link opener
//              delimiterStack.append(DelimiterInfo(current, '[', 1, isActive = true, canOpen = true, canClose = false))
//              current = current.following
//
//            case '!' if current.following.notAfterEnd &&
//              current.following.element.isInstanceOf[Cursor] &&
//              current.following.element.asInstanceOf[Cursor].char == '[' =>
//              // Add to delimiter stack as potential image opener
//              delimiterStack.append(DelimiterInfo(current, '!', 1, isActive = true, canOpen = true, canClose = false))
//              current = current.following
//
//            case ']' =>
//              // Look for link or image
//              current = lookForLinkOrImage(current, inlineNodes, delimiterStack)

            case '\n' =>
              // Process line break
              current = processLineBreak(current)

            case _ =>
              // Regular character, just move on
              current = current.following
          }

        case _ =>
          // Literal character or already processed node, just move on
          current = current.following
      }
    }

    // After processing all nodes, handle any remaining emphasis delimiters
//    processEmphasis(inlineNodes, delimiterStack, null) // null means process all delimiters
  }

  // Convert remaining character sequences to Text nodes
  consolidateCharacters(inlineNodes)

  // Return as List
  inlineNodes.toList
}

private def consolidateCharacters(nodes: DLList[Inline]): Unit = {
  if nodes.nonEmpty then
    var currentNode = nodes.headNode

    while (currentNode != null && !currentNode.isAfterEnd) {
      currentNode.element match
        case Cursor(char, _, _, _, _) =>
          val startNode = currentNode
          val sb        = new StringBuilder()

          // Collect consecutive C nodes
          while (currentNode.notAfterEnd && currentNode.element.isInstanceOf[Cursor]) {
            sb.append(currentNode.element.asInstanceOf[Cursor].char)
            currentNode = currentNode.following
          }

          // Replace with a single Text node
          startNode.element = Text(sb.toString)

          // Remove extra nodes
          if (startNode.following != currentNode) {
            startNode.following.unlinkUntil(currentNode)
          }
        case _ => currentNode = currentNode.following
    }
}

// Helper to check if string is a valid absolute URI according to spec
def isAbsoluteUri(str: String): Boolean = {
  // Simplified implementation - we need to match:
  // - A scheme (2-32 chars, ASCII letter followed by letters/digits/+/-/.)
  // - Followed by a colon
  // - Followed by zero or more non-control, non-space, non-< non-> chars
  val schemeRegex = "^[a-zA-Z][a-zA-Z0-9+.\\-]{1,31}:"
  str.matches(schemeRegex + ".*") &&
  !str.contains(" ") &&
  !str.contains("\t") &&
  !str.contains("\n") &&
  !str.contains("<") &&
  !str.contains(">")
}

// Helper to check if string is a valid email address according to spec
def isEmailAddress(str: String): Boolean = {
  // This is a simplified version - the real implementation would use
  // the HTML5 email regex mentioned in the spec
  val emailRegex =
    "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
  str.matches(emailRegex)
}

// Helper to check if string represents a valid HTML tag according to spec
// Helper to check if string represents a valid HTML tag according to spec
def isHtmlTag(str: String): Boolean = {
  logger.debug(s"Checking if '$str' is an HTML tag")

  // Tag name: ASCII letter followed by ASCII letters, digits, or hyphens
  val tagNameRegex = "[a-zA-Z][a-zA-Z0-9\\-]*"

  // Attribute name: ASCII letter, _, or : followed by ASCII letters, digits, _, ., :, or -
  val attrNameRegex = "[a-zA-Z_:][a-zA-Z0-9_.:\\-]*"

  // Attribute value: unquoted, single-quoted, or double-quoted
  val unquotedAttrValueRegex     = "[^\"'=<>`\\s]+"
  val singleQuotedAttrValueRegex = "'[^']*'"
  val doubleQuotedAttrValueRegex = "\"[^\"]*\""
  val attrValueRegex             = s"($unquotedAttrValueRegex|$singleQuotedAttrValueRegex|$doubleQuotedAttrValueRegex)"

  // Attribute: whitespace, name, optional value
  val attrRegex = s"\\s+$attrNameRegex(?:\\s*=\\s*$attrValueRegex)?"

  // 1. Open tag: <tagname attr* optional-/>, where attributes are optional
  // Note: we're NOT expecting the closing '>' as part of the input string
  val openTagRegex = s"^$tagNameRegex(?:$attrRegex)*\\s*/?$$".r

  // 2. Closing tag: </tagname>
  // Note: we're NOT expecting the closing '>' as part of the input string
  val closeTagRegex = s"^/$tagNameRegex\\s*$$".r

  // 3. HTML comment: <!-- anything -- (no closing > expected in the string)
  val commentRegex = """^!--(?:|(?:.|\n)*?--)$$""".r

  // 4. Processing instruction: <?anything? (no closing > expected)
  val piRegex = """^\?(?:.|\n)*?\?$$""".r

  // 5. Declaration: <!NAME anything (no closing > expected)
  val declRegex = """^![A-Z][^>]*$$""".r

  // 6. CDATA section: <![CDATA[ anything ]] (no closing > expected)
  val cdataRegex = """^!\[CDATA\[(?:.|\n)*?\]\]$$""".r

  val isOpenTag  = openTagRegex.matches(str)
  val isCloseTag = closeTagRegex.matches(str)
  val isComment  = commentRegex.matches(str)
  val isPI       = piRegex.matches(str)
  val isDecl     = declRegex.matches(str)
  val isCdata    = cdataRegex.matches(str)

  // For debugging
  if (isOpenTag) logger.debug(s"'$str' matched as open tag")
  if (isCloseTag) logger.debug(s"'$str' matched as close tag")
  if (isComment) logger.debug(s"'$str' matched as comment")
  if (isPI) logger.debug(s"'$str' matched as processing instruction")
  if (isDecl) logger.debug(s"'$str' matched as declaration")
  if (isCdata) logger.debug(s"'$str' matched as CDATA")

  val result = isOpenTag || isCloseTag || isComment || isPI || isDecl || isCdata
  logger.debug(s"HTML tag check result for '$str': $result")

  result
}
