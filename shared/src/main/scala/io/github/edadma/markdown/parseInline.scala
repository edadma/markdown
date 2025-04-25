package io.github.edadma.markdown

import io.github.edadma.dllist.DLList

import scala.annotation.tailrec
import scala.collection.mutable

case class DelimiterInfo(
    node: DLList[Inline]#Node,    // Reference to the node in the input list
    delimiterChar: Char,          // The delimiter character: *, _, [, or !
    length: Int,                  // Number of consecutive delimiters (1 or 2 for emphasis)
    var isActive: Boolean = true, // Whether this delimiter can still be matched
    canOpen: Boolean,             // Whether this can open emphasis/links
    canClose: Boolean,            // Whether this can close emphasis/links
)

// Standalone inline parsing function
def parseInline(inlines: List[Inline]): List[Inline] = {
  // Create initial DLList with character nodes
  val inlineNodes    = DLList[Inline](inlines*)
  val delimiterStack = new mutable.Stack[DelimiterInfo]

  def analyzeDelimiter(node: DLList[Inline]#Node, inlineNodes: DLList[Inline]): DelimiterInfo = {
    val delimiterChar = node.element.asInstanceOf[Cursor].char
    var count         = 0
    var current       = node

    logger.debug(s"Analyzing delimiter starting at: ${current.element}")

    // Count consecutive delimiters
    while (
      current.notAfterEnd &&
      current.element.isInstanceOf[Cursor] &&
      current.element.asInstanceOf[Cursor].char == delimiterChar &&
      !current.element.asInstanceOf[Cursor].isLiteral
    ) {
      count += 1
      current = current.following
    }

    // Get characters before and after the delimiter run
    val beforeChar = if (node.preceding.notBeforeStart) getCharFromNode(node.preceding) else '\n'
    val afterChar  = if (current.notAfterEnd) getCharFromNode(current) else '\n'

    logger.debug(s"Delimiter run: '$delimiterChar' x $count, before: '$beforeChar', after: '$afterChar'")

    // Determine if left/right flanking
    val isLeftFlanking = !isUnicodeWhitespace(afterChar) &&
      (!isUnicodePunctuation(afterChar) || isUnicodeWhitespace(beforeChar) || isUnicodePunctuation(beforeChar))

    val isRightFlanking = !isUnicodeWhitespace(beforeChar) &&
      (!isUnicodePunctuation(beforeChar) || isUnicodeWhitespace(afterChar) || isUnicodePunctuation(afterChar))

    logger.debug(s"Flanking analysis: left=$isLeftFlanking, right=$isRightFlanking")

    // Apply rules from spec section 6.2 to determine open/close capabilities
    val canOpen = delimiterChar match {
      case '*' => isLeftFlanking
      case '_' => isLeftFlanking && (!isRightFlanking || isUnicodePunctuation(beforeChar))
      case '[' => true
      case '!' => true
      case _   => false
    }

    val canClose = delimiterChar match {
      case '*' => isRightFlanking
      case '_' => isRightFlanking && (!isLeftFlanking || isUnicodePunctuation(afterChar))
      case ']' => true
      case _   => false
    }

    logger.debug(s"Delimiter capabilities: canOpen=$canOpen, canClose=$canClose")

    DelimiterInfo(node, delimiterChar, count, true, canOpen, canClose)
  }

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

            case '*' | '_' =>
              // Add to delimiter stack for emphasis processing
              val delimiterInfo = analyzeDelimiter(current, inlineNodes)
              logger.debug(s"Adding delimiter: char=${delimiterInfo.delimiterChar}, " +
                s"length=${delimiterInfo.length}, canOpen=${delimiterInfo.canOpen}, " +
                s"canClose=${delimiterInfo.canClose}")
              delimiterStack.push(delimiterInfo)

              // Skip ahead past all the delimiters
              val nextNode = current.following.skipForward(delimiterInfo.length - 1)
              current = if (nextNode.notAfterEnd) nextNode else current.following

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
    processEmphasis(inlineNodes, delimiterStack, None)
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

// Get character from a node
def getCharFromNode(node: DLList[Inline]#Node): Char = {
  node.element match {
    case c: Cursor                     => c.char
    case t: Text if t.content.nonEmpty => t.content(0)
    case _                             => ' ' // Default for other node types
  }
}

// Check for Unicode whitespace
def isUnicodeWhitespace(c: Char): Boolean = {
  c.isWhitespace || c == '\n' || c == '\r' || c == '\t'
}

// Check for Unicode punctuation
def isUnicodePunctuation(c: Char): Boolean = {
  // Check Unicode category for punctuation
  val chartype = Character.getType(c)
  chartype == Character.CONNECTOR_PUNCTUATION ||
  chartype == Character.DASH_PUNCTUATION ||
  chartype == Character.END_PUNCTUATION ||
  chartype == Character.FINAL_QUOTE_PUNCTUATION ||
  chartype == Character.INITIAL_QUOTE_PUNCTUATION ||
  chartype == Character.OTHER_PUNCTUATION ||
  chartype == Character.START_PUNCTUATION
}

// Extract inlines between nodes
def extractInlinesBetween(start: DLList[Inline]#Node, end: DLList[Inline]#Node): List[Inline] = {
  var result  = List[Inline]()
  var current = start

  while (current != end) {
    result = result :+ current.element
    current = current.following
  }

  result
}

// Check if node contains open parenthesis
def isOpenParen(node: DLList[Inline]#Node): Boolean = {
  node.element match {
    case c: Cursor => c.char == '('
    case _         => false
  }
}

def processEmphasis(
    inlineNodes: DLList[Inline],
    delimiterStack: mutable.Stack[DelimiterInfo],
    stackBottom: Option[DelimiterInfo],
): Unit = {
  import scala.collection.mutable

  logger.debug(s"Processing emphasis, stack size: ${delimiterStack.size}")

  // Dump all delimiters in the stack for debugging
  delimiterStack.zipWithIndex.foreach { case (d, i) =>
    logger.debug(f"  Delimiter[$i]: char=${d.delimiterChar}, length=${d.length}, " +
      f"active=${d.isActive}, canOpen=${d.canOpen}, canClose=${d.canClose}")
  }

  // Track openers bottom for each delimiter type
  // Key: (delimiter char, length mod 3, can opener also be closer)
  val openersBottom = mutable.Map[(Char, Int, Boolean), Int]().withDefaultValue(-1)

  // Process from the beginning of the document (bottom of stack) upward
  // Convert the stack to a list to process in document order
  val delimiterList = delimiterStack.toList.reverse

  logger.debug(s"Processing ${delimiterList.size} delimiters in document order")

  // Start with the first potential closer
  var currentPosition = 0

  // Process until we run out of closers or reach the end of the list
  while (currentPosition < delimiterList.size) {
    logger.debug(s"Current position: $currentPosition")

    // Find next potential closer moving forward in the document
    var closer: Option[DelimiterInfo] = None
    var currentIdx                    = currentPosition

    while (currentIdx < delimiterList.size && closer.isEmpty) {
      val candidate = delimiterList(currentIdx)
      if (
        (candidate.delimiterChar == '*' || candidate.delimiterChar == '_') &&
        candidate.canClose &&
        candidate.isActive
      ) {
        closer = Some(candidate)
        logger.debug(s"Found potential closer at list index $currentIdx")
      } else {
        currentIdx += 1
      }
    }

    if (closer.isEmpty) {
      // No more potential closers
      logger.debug("No more potential closers found")
      currentPosition = delimiterList.size // Exit the loop
    } else {
      val closerInfo = closer.get
      val closerIdx  = delimiterList.indexOf(closerInfo)
      val closerChar = closerInfo.delimiterChar
      val closerMod  = closerInfo.length % 3

      logger.debug(
        s"Processing closer at list index $closerIdx: char=${closerInfo.delimiterChar}, length=${closerInfo.length}",
      )

      // Find matching opener (searching backward from the closer)
      var opener: Option[DelimiterInfo] = None
      var openerIdx                     = closerIdx - 1

      // Look for openers before the closer
      while (openerIdx >= 0 && opener.isEmpty) {
        val candidate = delimiterList(openerIdx)
        logger.debug(s"Checking potential opener at list index $openerIdx: " +
          s"char=${candidate.delimiterChar}, canOpen=${candidate.canOpen}, length=${candidate.length}")

        if (
          candidate.isActive &&
          candidate.delimiterChar == closerChar &&
          candidate.canOpen &&
          isValidEmphasisPair(candidate, closerInfo)
        ) {
          opener = Some(candidate)
          logger.debug(s"Found matching opener at list index $openerIdx")
        } else {
          openerIdx -= 1
        }
      }

      if (opener.isEmpty) {
        // No matching opener, move to next position
        logger.debug(s"No matching opener found for closer at list index $closerIdx")
        currentPosition = closerIdx + 1
      } else {
        // We found emphasis!
        val openerInfo = opener.get
        val openerIdx  = delimiterList.indexOf(openerInfo)

        // Determine if it's emphasis or strong emphasis based on delimiter length
        // Strong emphasis requires at least 2 delimiters
        val strongEmphasis = (openerInfo.length >= 2 && closerInfo.length >= 2)
        val useDelimiters  = if (strongEmphasis) 2 else 1
        val emphasisType   = if (strongEmphasis) "strong" else "em"

        logger.debug(
          s"Creating $emphasisType emphasis between indexes $openerIdx and $closerIdx " +
            s"(using $useDelimiters delimiters from each)",
        )

        // Create emphasis node
        createEmphasisNode(openerInfo, closerInfo, emphasisType, useDelimiters, inlineNodes)

        // Mark these delimiters as inactive or update their lengths in the original stack
        val openerStackIdx = delimiterStack.indexWhere(d => d.node == openerInfo.node)
        val closerStackIdx = delimiterStack.indexWhere(d => d.node == closerInfo.node)

        if (openerStackIdx >= 0) {
          if (openerInfo.length > useDelimiters) {
            // Reduce the length of the opener
            delimiterStack(openerStackIdx) = DelimiterInfo(
              openerInfo.node,
              openerInfo.delimiterChar,
              openerInfo.length - useDelimiters,
              isActive = true,
              openerInfo.canOpen,
              openerInfo.canClose,
            )
            logger.debug(
              s"Reduced opener length at stack index $openerStackIdx to ${openerInfo.length - useDelimiters}",
            )
          } else {
            // Mark as inactive if fully used
            delimiterStack(openerStackIdx).isActive = false
            logger.debug(s"Marked opener at stack index $openerStackIdx as inactive")
          }
        }

        if (closerStackIdx >= 0) {
          if (closerInfo.length > useDelimiters) {
            // Reduce the length of the closer
            delimiterStack(closerStackIdx) = DelimiterInfo(
              closerInfo.node,
              closerInfo.delimiterChar,
              closerInfo.length - useDelimiters,
              isActive = true,
              closerInfo.canOpen,
              closerInfo.canClose,
            )
            logger.debug(
              s"Reduced closer length at stack index $closerStackIdx to ${closerInfo.length - useDelimiters}",
            )
          } else {
            // Mark as inactive if fully used
            delimiterStack(closerStackIdx).isActive = false
            logger.debug(s"Marked closer at stack index $closerStackIdx as inactive")
          }
        }

        // Start again from the beginning since we modified the document
        currentPosition = 0
        logger.debug("Restarting from the beginning after creating emphasis")
      }
    }
  }

  // Clean up - remove all inactive delimiters
  logger.debug("Cleaning up inactive delimiters")
  delimiterStack.filterInPlace(_.isActive)

  logger.debug(s"Emphasis processing completed, stack size: ${delimiterStack.size}")
}

private def isValidEmphasisPair(opener: DelimiterInfo, closer: DelimiterInfo): Boolean = {
  // Rule 9: Sum of delimiter runs can't be multiple of 3 unless both are
  if (
    (opener.length + closer.length) % 3 == 0 &&
    opener.length                   % 3 != 0 && closer.length % 3 != 0
  ) {
    logger.debug(
      f"Invalid emphasis pair: sum=${opener.length + closer.length} is multiple of 3 but individual lengths are not",
    )
    false
  } else {
    true
  }
}

// Check if a delimiter pair can form valid emphasis/strong emphasis
private def isValidEmphasisDelimiterPair(opener: DelimiterInfo, closer: DelimiterInfo): Boolean = {
  // Rule 9 from spec: Sum of delimiter runs can't be multiple of 3 unless both are
  if (
    opener.canOpen && closer.canClose &&
    (opener.length + closer.length) % 3 == 0 &&
    opener.length                   % 3 != 0 && closer.length % 3 != 0
  ) {
    return false
  }

  true
}

// Determine whether to create emphasis or strong emphasis
private def determineEmphasisType(opener: DelimiterInfo, closer: DelimiterInfo): String = {
  // If both opener and closer have length >= 2, it's strong emphasis
  if (opener.length >= 2 && closer.length >= 2) {
    "strong"
  } else {
    "em"
  }
}

// Create emphasis or strong emphasis node
private def createEmphasisNode(
    opener: DelimiterInfo,
    closer: DelimiterInfo,
    emphasisType: String,
    delimiterCount: Int,
    inlineNodes: DLList[Inline],
): Unit = {
  val openerNode = opener.node

  // For strong emphasis, we need to skip the opening delimiter characters (**)
  var contentStart = openerNode
  for (i <- 0 until delimiterCount) {
    contentStart = contentStart.following
  }

  // Find the closing delimiter start position
  var closerStart = closer.node
  for (i <- 0 until closer.length - delimiterCount) {
    closerStart = closerStart.following
  }

  // Extract contents between the end of the opening delimiter and the start of the closing delimiter
  val contents = extractInlinesBetween(contentStart, closerStart)
  logger.debug(s"Creating $emphasisType with raw contents: $contents")

  // Convert Cursor objects to Text objects in the contents
  val processedContents = consolidateTextInContents(contents)
  logger.debug(s"Processed contents: $processedContents")

  // Create the appropriate node
  val emphNode = emphasisType match {
    case "em"     => Emphasis(processedContents)
    case "strong" => Strong(processedContents)
  }

  // Replace the first character of the opener with the emphasis node
  openerNode.element = emphNode

  // Remove used delimiter characters from the opener (if any)
  var current = openerNode.following
  for (i <- 1 until delimiterCount) {
    val next = current.following
    current.unlink
    current = next
  }

  // Remove content nodes between delimiters
  current = contentStart
  while (current != closerStart && current.notAfterEnd) {
    val next = current.following
    current.unlink
    current = next
  }

  // Remove used delimiter characters from the closer
  for (i <- 0 until delimiterCount) {
    val next = closerStart.following
    closerStart.unlink
    closerStart = next
  }

  logger.debug(s"Created $emphasisType node")
}

// Helper method to consolidate Cursor objects into Text objects within inline content
private def consolidateTextInContents(inlines: List[Inline]): List[Inline] = {
  if (inlines.isEmpty) {
    return inlines
  }

  val result      = new scala.collection.mutable.ListBuffer[Inline]()
  val currentText = new StringBuilder()

  // Process each inline element
  inlines.foreach {
    case c: Cursor =>
      // Add character to current text buffer
      currentText.append(c.char)

    case other: Inline =>
      // If we have accumulated text, add it as a Text node
      if (currentText.nonEmpty) {
        result += Text(currentText.toString)
        currentText.clear()
      }

      // Process any nested inlines recursively
      other match {
        case Emphasis(children) =>
          result += Emphasis(consolidateTextInContents(children))
        case Strong(children) =>
          result += Strong(consolidateTextInContents(children))
        case Link(dest, title, children) =>
          result += Link(dest, title, consolidateTextInContents(children))
        case Image(dest, title, children) =>
          result += Image(dest, title, consolidateTextInContents(children))
        case _ =>
          result += other
      }
  }

  // Add any remaining text
  if (currentText.nonEmpty) {
    result += Text(currentText.toString)
  }

  result.toList
}

// Update delimiters after creating emphasis
private def updateDelimiters(
    opener: DelimiterInfo,
    closer: DelimiterInfo,
    useDelimiters: Int,
    delimiterStack: mutable.Stack[DelimiterInfo],
): Unit = {
  // Instead of modifying case class fields, we'll replace the objects in the stack

  // Handle opener
  val openerIdx = delimiterStack.indexOf(opener)
  if (openerIdx >= 0) { // Make sure opener is still in the stack
    if (opener.length > useDelimiters) {
      // Replace with new opener that has fewer delimiters
      val newOpener = DelimiterInfo(
        opener.node,
        opener.delimiterChar,
        opener.length - useDelimiters,
        opener.isActive,
        opener.canOpen,
        opener.canClose,
      )

      // Remove and insert at same position
      delimiterStack.remove(openerIdx)
      // Insert at the same position - a bit awkward with Stack but workable
      val tempStack = mutable.Stack[DelimiterInfo]()
      while (delimiterStack.size > openerIdx) {
        tempStack.push(delimiterStack.pop())
      }
      delimiterStack.push(newOpener)
      while (tempStack.nonEmpty) {
        delimiterStack.push(tempStack.pop())
      }

      logger.debug(s"Opener has ${newOpener.length} delimiters left")
    } else {
      // Remove opener from stack if fully used
      delimiterStack.remove(openerIdx)
      logger.debug("Removed opener from stack")
    }
  }

  // Handle closer - may need to adjust index if opener was removed
  val closerIdx = delimiterStack.indexOf(closer)
  if (closerIdx >= 0) { // Make sure closer is still in the stack
    if (closer.length > useDelimiters) {
      // Replace with new closer that has fewer delimiters
      val newCloser = DelimiterInfo(
        closer.node,
        closer.delimiterChar,
        closer.length - useDelimiters,
        closer.isActive,
        closer.canOpen,
        closer.canClose,
      )

      // Remove and insert at same position
      delimiterStack.remove(closerIdx)
      // Insert at the same position
      val tempStack = mutable.Stack[DelimiterInfo]()
      while (delimiterStack.size > closerIdx) {
        tempStack.push(delimiterStack.pop())
      }
      delimiterStack.push(newCloser)
      while (tempStack.nonEmpty) {
        delimiterStack.push(tempStack.pop())
      }

      logger.debug(s"Closer has ${newCloser.length} delimiters left")
    } else {
      // Remove closer from stack
      delimiterStack.remove(closerIdx)
      logger.debug("Removed closer from stack")
    }
  }
}
