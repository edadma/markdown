package io.github.edadma.markdown

import io.github.edadma.dllist.{DLList, DLListNode}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

case class DelimiterInfo(
    node: DLListNode[Inline],     // Reference to the node in the input list
    delimiterChar: Char,          // The delimiter character: *, _, [, or !
    length: Int,                  // Number of consecutive delimiters (1 or 2 for emphasis)
    var isActive: Boolean = true, // Whether this delimiter can still be matched
    canOpen: Boolean,             // Whether this can open emphasis/links
    canClose: Boolean,            // Whether this can close emphasis/links
)

def parseInline(
    inlines: List[Inline],
    linkRefs: immutable.Map[String, LinkReference],
    config: MarkdownConfig,
): List[Inline] = {
  val inlineNodes    = DLList[Inline](inlines*)
  val delimiterStack = new mutable.Stack[DelimiterInfo]

  // Main processing loop - single pass through the document
  if (inlineNodes.nonEmpty) {
    var current = inlineNodes.headNode

    while (current.notAfterEnd) {
      current.element match {
        case c: C if !c.isLiteral =>
          c.char match {
            case '$' if !c.isLiteral =>
              // Process math expression
              val oldCurrent = current // Remember the current node

              current = processMathSpan(current)

              if (current == oldCurrent) {
                current = current.following
              }
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
              val delimiterInfo = analyzeDelimiter(current)
              logger.debug(s"Adding delimiter: char=${delimiterInfo.delimiterChar}, " +
                s"length=${delimiterInfo.length}, canOpen=${delimiterInfo.canOpen}, " +
                s"canClose=${delimiterInfo.canClose}")
              delimiterStack.push(delimiterInfo)

              // Skip ahead past all the delimiters
              val nextNode = current.following.skipForward(delimiterInfo.length - 1)
              current = if (nextNode.notAfterEnd) nextNode else current.following

            case '[' =>
              // If this '[' is really an image opener (i.e. ![ ), unlink the '!' and record an image delimiter
              val prev = current.preceding
              val (delimChar, unlinkPrev) =
                if (
                  prev.notBeforeStart &&
                  prev.element.isInstanceOf[C] &&
                  prev.element.asInstanceOf[C].char == '!' &&
                  !prev.element.asInstanceOf[C].isLiteral
                ) {
                  // it's an image: unlink the '!'
                  ('!', true)
                } else {
                  ('[', false)
                }

              if (unlinkPrev) prev.unlink

              // Push exactly one delimiter, either for '[' or for '!'
              val delimiterInfo = DelimiterInfo(
                current,
                delimChar,
                1,
                isActive = true,
                canOpen = true,
                canClose = false,
              )
              delimiterStack.push(delimiterInfo)

              current = current.following

            case ']' =>
              // Look for link or image
              logger.debug(s"Found closing bracket, looking for link or image")
              current = lookForLinkOrImage(current, inlineNodes, delimiterStack, linkRefs, config)

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
    if (delimiterStack.nonEmpty) {
      logger.debug(s"Processing ${delimiterStack.size} delimiters")
      processEmphasis(delimiterStack)
    } else {
      logger.debug("No delimiters to process")
    }
  }

  // Convert remaining character sequences to Text nodes
  consolidateCharacters(inlineNodes)

  // Return as List
  decodeHtmlEntities(inlineNodes.toList)
}

private def decodeHtmlEntities(inlines: List[Inline]): List[Inline] = {
  inlines map {
    case Text(content)     => Text(decodeHtmlEntities(content))
    case Emphasis(inlines) => Emphasis(decodeHtmlEntities(inlines))
    case Strong(inlines)   => Strong(decodeHtmlEntities(inlines))
    case Link(destination, title, inlines) =>
      Link(decodeHtmlEntities(destination), title.map(t => decodeHtmlEntities(t)), decodeHtmlEntities(inlines))
    case Image(destination, title, inlines) =>
      Image(decodeHtmlEntities(destination), title.map(t => decodeHtmlEntities(t)), decodeHtmlEntities(inlines))
    case inline => inline
  }
}

private def decodeHtmlEntities(input: String): String = {
  // Regex pattern to match HTML entities:
  // - Named entities: &name;
  // - Decimal entities: &#number;
  // - Hex entities: &#xhex;
  val entityPattern = """&(#[xX]([0-9a-fA-F]+)|#([0-9]+)|([a-zA-Z][a-zA-Z0-9]*));""".r

  // Replace all entities in the input string
  entityPattern.replaceAllIn(
    input,
    matchResult => {
      val entity     = matchResult.group(0)         // The entire entity (e.g., "&amp;")
      val hexValue   = Option(matchResult.group(2)) // Hex value (e.g., "26" from "&#x26;")
      val decValue   = Option(matchResult.group(3)) // Decimal value (e.g., "38" from "&#38;")
      val namedValue = Option(matchResult.group(4)) // Named entity (e.g., "amp" from "&amp;")

      // Replace based on entity type
      if (hexValue.isDefined) {
        // Handle hex entities (e.g., &#x26;)
        try {
          val codePoint = Integer.parseInt(hexValue.get, 16)
          new String(Character.toChars(codePoint))
        } catch {
          case _: Exception => entity // Return original if parsing fails
        }
      } else if (decValue.isDefined) {
        // Handle decimal entities (e.g., &#38;)
        try {
          val codePoint = Integer.parseInt(decValue.get)
          new String(Character.toChars(codePoint))
        } catch {
          case _: Exception => entity // Return original if parsing fails
        }
      } else if (namedValue.isDefined) {
        // Handle named entities (e.g., &amp;)
        HTMLEntities get namedValue.get match
          case Some(replacement) => replacement
          case None              => entity
      } else {
        // If no replacement is found, return the original entity
        entity
      }
    },
  )
}

def analyzeDelimiter(node: DLListNode[Inline]): DelimiterInfo = {
  val delimiterChar = node.element.asInstanceOf[C].char
  var count         = 0
  var current       = node

  logger.debug(s"Analyzing delimiter starting at: ${current.element}")

  // Count consecutive delimiters
  while (
    current.notAfterEnd &&
    current.element.isInstanceOf[C] &&
    current.element.asInstanceOf[C].char == delimiterChar &&
    !current.element.asInstanceOf[C].isLiteral
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

def processCodeSpan(node: DLListNode[Inline]): DLListNode[Inline] = {
  logger.debug(s"Starting processCodeSpan on node: ${node.element}")
  // Count the consecutive backticks in the opening delimiter
  val openingNode  = node
  var openingCount = 0
  var current      = node

  // Count consecutive backticks in the opening delimiter
  while (
    current.notAfterEnd &&
    current.element.isInstanceOf[C] &&
    current.element.asInstanceOf[C].char == '`' &&
    !current.element.asInstanceOf[C].isLiteral
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
        current.element.isInstanceOf[C] &&
        current.element.asInstanceOf[C].char == '`'
      ) {
        // Count consecutive backticks to see if we have a match
        var closingCount = 0
        var closingStart = current

        @tailrec
        def closeCount(): Unit = {
          if current.notAfterEnd &&
            current.element.isInstanceOf[C] &&
            current.element.asInstanceOf[C].char == '`' &&
            closingCount < openingCount
          then
            if current.element.asInstanceOf[C].isLiteral && closingCount == 0 then
              closingStart = current.follow(current.element.asInstanceOf[C].copy())
              current.element = current.element.asInstanceOf[C].copy(char = '\\', isLiteral = false)
              closingCount += 1
              current = current.following.following
              closeCount()
            else if current.element.asInstanceOf[C].isLiteral then
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
def extractAndProcessCodeSpanContent(start: DLListNode[Inline], end: DLListNode[Inline]): String = {
  // Build the content string from nodes between start and end
  val builder = new StringBuilder
  var current = start

  while (current != end) {
    current.element match {
      case c: C =>
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

def processLineBreak(node: DLListNode[Inline]): DLListNode[Inline] = {
  logger.debug(s"Processing line break at node: ${node.element}")

  // Check for hard break - backslash escape
  if (
    node.preceding.notBeforeStart &&
    node.preceding.element.isInstanceOf[C] &&
    node.preceding.element.asInstanceOf[C].char == '\\' &&
    !node.preceding.element.asInstanceOf[C].isLiteral
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
    current.element.isInstanceOf[C] &&
    current.element.asInstanceOf[C].char == ' ' &&
    !current.element.asInstanceOf[C].isLiteral
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
      node.preceding.element match {
        case C(' ', _, _, _, _) =>
          node.preceding.unlink
          spacesToRemove -= 1
        case _ =>
          looping = false // If we hit a non-space, stop removing
      }
    }

    return node
  }

  // If we get here, it's a soft break
  logger.debug("Creating soft line break")
  node.element = SoftLineBreak()
  node
}

def processHtmlOrAutolink(node: DLListNode[Inline]): DLListNode[Inline] = {
  logger.debug(s"Starting HTML/autolink processing on node: ${node.element}")

  // Find the closing '>' if it exists
  val openingNode = node
  var current     = node.following
  val content     = new StringBuilder()

  // Look ahead to find a potential closing '>'
  while (
    current.notAfterEnd &&
    !(current.element.isInstanceOf[C] &&
      current.element.asInstanceOf[C].char == '>' &&
      !current.element.asInstanceOf[C].isLiteral)
  ) {

    current.element match {
      case c: C =>
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
    !(current.element.isInstanceOf[C] &&
      current.element.asInstanceOf[C].char == '>')
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
  node
}

private def consolidateCharacters(nodes: DLList[Inline]): Unit = {
  if nodes.nonEmpty then
    var currentNode = nodes.headNode

    while (currentNode != null && !currentNode.isAfterEnd) {
      currentNode.element match
        case _: C =>
          val startNode = currentNode
          val sb        = new StringBuilder()

          // Collect consecutive C nodes
          while (currentNode.notAfterEnd && currentNode.element.isInstanceOf[C]) {
            sb.append(currentNode.element.asInstanceOf[C].char)
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
  val commentRegex = """^!--(?:|(?:.|\n)*?--)$""".r

  // 4. Processing instruction: <?anything? (no closing > expected)
  val piRegex = """^\?(?:.|\n)*?\?$""".r

  // 5. Declaration: <!NAME anything (no closing > expected)
  val declRegex = """^![A-Z][^>]*$""".r

  // 6. CDATA section: <![CDATA[ anything ]] (no closing > expected)
  val cdataRegex = """^!\[CDATA\[(?:.|\n)*?]]$""".r

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
def getCharFromNode(node: DLListNode[Inline]): Char = {
  node.element match {
    case c: C                          => c.char
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
def extractInlinesBetween(start: DLListNode[Inline], end: DLListNode[Inline]): List[Inline] = {
  logger.debug(s"extractInlinesBetween from $start to $end")

  if (start == null || end == null) {
    logger.debug("Received null node in extractInlinesBetween - returning empty list")
    return List.empty
  }

  var result  = List[Inline]()
  var current = start

  // Check if current is valid and not at the end node yet
  while (current != null && current != end && current.notAfterEnd) {
    result = result :+ current.element
    current = current.following
  }

  result
}

def processEmphasis(delimiterStack: mutable.Stack[DelimiterInfo]): Unit = {
  if (delimiterStack.isEmpty) {
    logger.debug("No delimiters to process in processEmphasis")
    return
  }

  logger.debug(s"==== processEmphasis START ====")
  logger.debug(s"Processing emphasis, stack size: ${delimiterStack.size}")

  // Dump all delimiters in the stack for debugging
  delimiterStack.zipWithIndex.foreach { case (d, i) =>
    logger.debug(f"  Delimiter[$i]: char=${d.delimiterChar}, length=${d.length}, " +
      f"active=${d.isActive}, canOpen=${d.canOpen}, canClose=${d.canClose}, " +
      f"node=${d.node}, nodeValid=${isNodeValid(d.node)}")
  }

  // Convert stack to list for easier index-based access
  // We'll treat it as if it were in document order (first to last)
  val delimiterList = delimiterStack.toList.reverse

  logger.debug(s"Processing ${delimiterList.size} delimiters in document order")

  // Start with the first position
  var currentPosition = 0

  // Process until we run out of closers or reach the end of the list
  while (currentPosition < delimiterList.size) {
    logger.debug(s"Current position: $currentPosition")

    // Find next potential closer moving forward in the document
    var closer: Option[DelimiterInfo] = None
    var closerIdx                     = currentPosition

    while (closerIdx < delimiterList.size && closer.isEmpty) {
      val candidate = delimiterList(closerIdx)
      logger.debug(s"Examining candidate at index $closerIdx: " +
        s"char=${candidate.delimiterChar}, active=${candidate.isActive}, " +
        s"canClose=${candidate.canClose}, nodeValid=${isNodeValid(candidate.node)}")

      if (
        candidate.isActive &&
        (candidate.delimiterChar == '*' || candidate.delimiterChar == '_') &&
        candidate.canClose &&
        isNodeValid(candidate.node)
      ) {
        closer = Some(candidate)
        logger.debug(s"Found potential closer at list index $closerIdx")
      } else {
        if (!candidate.isActive) logger.debug(s"  Skipping - inactive")
        else if (!candidate.canClose) logger.debug(s"  Skipping - can't close")
        else if (!isNodeValid(candidate.node)) logger.debug(s"  Skipping - invalid node")
        else logger.debug(s"  Skipping - other reason")

        closerIdx += 1
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

      logger.debug(
        s"Processing closer at list index $closerIdx: char=${closerInfo.delimiterChar}, " +
          s"length=${closerInfo.length}, node=${closerInfo.node}",
      )

      // Find matching opener (searching backward from the closer)
      var opener: Option[DelimiterInfo] = None
      var openerIdx                     = closerIdx - 1

      // Look for openers before the closer
      while (openerIdx >= 0 && opener.isEmpty) {
        val candidate = delimiterList(openerIdx)
        logger.debug(s"Checking potential opener at list index $openerIdx: " +
          s"char=${candidate.delimiterChar}, canOpen=${candidate.canOpen}, " +
          s"length=${candidate.length}, node=${candidate.node}, " +
          s"nodeValid=${isNodeValid(candidate.node)}")

        if (
          candidate.isActive &&
          candidate.delimiterChar == closerChar &&
          candidate.canOpen &&
          isValidEmphasisPair(candidate, closerInfo) &&
          isNodeValid(candidate.node)
        ) {
          opener = Some(candidate)
          logger.debug(s"Found matching opener at list index $openerIdx")
        } else {
          if (!candidate.isActive) logger.debug(s"  Skipping - inactive")
          else if (candidate.delimiterChar != closerChar) logger.debug(s"  Skipping - different delimiter char")
          else if (!candidate.canOpen) logger.debug(s"  Skipping - can't open")
          else if (!isValidEmphasisPair(candidate, closerInfo)) logger.debug(s"  Skipping - invalid pair")
          else if (!isNodeValid(candidate.node)) logger.debug(s"  Skipping - invalid node")
          else logger.debug(s"  Skipping - other reason")

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

        // Determine if it's emphasis or strong emphasis based on Rule 13
        // We use as many delimiters as possible up to 2 (for strong emphasis)
        val strongEmphasis = openerInfo.length >= 2 && closerInfo.length >= 2
        val useDelimiters  = if (strongEmphasis) 2 else 1
        val emphasisType   = if (strongEmphasis) "strong" else "em"

        logger.debug(
          s"Creating $emphasisType emphasis between indexes $openerIdx and $closerIdx " +
            s"(using $useDelimiters delimiters from each)",
        )
        logger.debug(s"Opener node before: ${openerInfo.node}, valid=${isNodeValid(openerInfo.node)}")
        logger.debug(s"Closer node before: ${closerInfo.node}, valid=${isNodeValid(closerInfo.node)}")

        // Create emphasis node
        createEmphasisNode(openerInfo, closerInfo, emphasisType, useDelimiters, delimiterStack)

        // After creating an emphasis node, we need to update delimiters and mark any stale ones inactive
        logger.debug(s"Checking all delimiters for stale references")
        for (i <- delimiterStack.indices) {
          if (delimiterStack(i).isActive) {
            val isValid = isNodeValid(delimiterStack(i).node)
            logger.debug(s"Delimiter at stack index $i: valid=$isValid, " +
              s"char=${delimiterStack(i).delimiterChar}")

            if (!isValid) {
              delimiterStack(i).isActive = false
              logger.debug(s"Marked delimiter at stack index $i as inactive due to stale reference")
            }
          }
        }

        // Once we've created an emphasis node, we MUST restart from the beginning
        // This implements the bottom-up approach from the spec
        currentPosition = 0
        logger.debug("Restarting from the beginning after creating emphasis")
      }
    }
  }

  // Clean up - remove all inactive delimiters
  logger.debug("Cleaning up inactive delimiters")
  val beforeSize = delimiterStack.size
  delimiterStack.filterInPlace(_.isActive)
  val afterSize = delimiterStack.size
  logger.debug(s"Removed ${beforeSize - afterSize} inactive delimiters")

  logger.debug(s"Emphasis processing completed, stack size: ${delimiterStack.size}")
  logger.debug(s"==== processEmphasis END ====")
}

// Helper method to check if a node is still valid
private def isNodeValid(node: DLListNode[Inline]): Boolean = {
  if (node == null) return false

  try {
    // Check if the node is still linked to the list
    // An unlinked node will have null for both the prev and next references
    // or will throw an exception when accessing elements
    val _ = node.element
    val _ = node.following
    val _ = node.preceding
    true
  } catch {
    case _: Exception => false
  }
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

// Create emphasis or strong emphasis node
private def createEmphasisNode(
    opener: DelimiterInfo,
    closer: DelimiterInfo,
    emphasisType: String,
    delimiterCount: Int,
    delimiterStack: mutable.Stack[DelimiterInfo],
): Unit = {
  logger.debug(s"==== createEmphasisNode START ====")
  logger.debug(s"Creating $emphasisType with $delimiterCount delimiters")

  if (opener == null || closer == null) {
    logger.debug("Null opener or closer in createEmphasisNode - returning")
    return
  }

  if (opener.node == null || closer.node == null) {
    logger.debug("Opener or closer has null node - returning")
    return
  }

  // Log the full node structure to help with debugging
  logger.debug("--- Node structure visualization ---")
  var vizNode = opener.node
  var nodeIdx = 0
  while (vizNode != null && vizNode.notAfterEnd && nodeIdx < 20) {
    logger.debug(s"Node[$nodeIdx]: ${vizNode.element}")
    vizNode = vizNode.following
    nodeIdx += 1
  }
  logger.debug("--- End node structure visualization ---")

  val openerNode = opener.node
  logger.debug(s"Opener node element: ${openerNode.element}")

  // For the emphasis, we need to skip the opening delimiter characters used for this emphasis
  var contentStart = openerNode
  logger.debug(s"Initial contentStart: $contentStart, element: ${contentStart.element}")

  for (i <- 0 until delimiterCount) {
    contentStart = contentStart.following
    logger.debug(s"Advancing contentStart[$i]: $contentStart, element: ${contentStart.element}")
  }

  // Skip any remaining opener delimiter characters too
  if (opener.length > delimiterCount) {
    logger.debug(s"Skipping ${opener.length - delimiterCount} additional opener delimiters")
    for (i <- 0 until opener.length - delimiterCount) {
      contentStart = contentStart.following
      logger.debug(s"After skipping additional delimiter: contentStart = ${contentStart.element}")
    }
  }

  // Locate the actual content end (the node right BEFORE the first closer delimiter)
  var closerFirstNode = closer.node
  logger.debug(s"Initial closerFirstNode: ${closerFirstNode.element}")

  // The content ends right before the first closer delimiter
  var contentEnd = closerFirstNode
  logger.debug(s"Content should end at: ${contentEnd.element}")

  // Log all nodes that will be included in content
  logger.debug("--- Content nodes visualization ---")
  var contentNode = contentStart
  var contentIdx  = 0
  while (contentNode != contentEnd && contentNode.notAfterEnd && contentIdx < 20) {
    logger.debug(s"ContentNode[$contentIdx]: ${contentNode.element}")
    contentNode = contentNode.following
    contentIdx += 1
  }
  logger.debug("--- End content nodes visualization ---")

  // Extract content - now with correct boundaries
  logger.debug(s"Extracting contents between $contentStart and $contentEnd")
  val contents = extractInlinesBetween(contentStart, contentEnd)
  logger.debug(s"Creating $emphasisType with raw contents: $contents")

  // Process contents to convert cursors to text
  val processedContents = consolidateTextInContents(contents)
  logger.debug(s"Processed contents: $processedContents")

  // Create the appropriate node
  val emphNode = emphasisType match {
    case "em"     => Emphasis(processedContents)
    case "strong" => Strong(processedContents)
  }

  // SPECIAL HANDLING FOR ***asdf*** CASE:
  var finalNode = emphNode

  // If we have 1 delimiter remaining on both sides after creating strong emphasis,
  // we should create an emphasis node that wraps the strong node
  if (
    opener.length > delimiterCount && closer.length > delimiterCount &&
    opener.length - delimiterCount == 1 && closer.length - delimiterCount == 1 &&
    emphasisType == "strong"
  ) {
    // For triple asterisk case - wrap the strong node in an emphasis node
    logger.debug("Triple asterisk case detected - wrapping Strong node in Emphasis")
    finalNode = Emphasis(List(emphNode))
  }

  // Replace the first character of the opener with the emphasis node
  logger.debug(s"Replacing opener node element with $finalNode")
  openerNode.element = finalNode
  logger.debug(s"Opener node after replacement: ${openerNode.element}")

  // Keep track of nodes to remove
  val nodesToRemove = new mutable.ArrayBuffer[DLListNode[Inline]]()

  // Add opener delimiter nodes to remove (excluding the first one which holds the emphasis node)
  var current = openerNode.following

  // In triple asterisk case, remove ALL delimiters
  val delimitersToRemove = if (finalNode.isInstanceOf[Emphasis] && emphasisType == "strong") {
    opener.length // Remove all delimiters
  } else {
    opener.length // Remove used delimiters
  }

  for (i <- 1 until delimitersToRemove) {
    if (current != null && current.notAfterEnd) {
      nodesToRemove += current
      logger.debug(s"Will remove opener delimiter[$i]: ${current.element}")
      current = current.following
    }
  }

  // Add content nodes to be removed
  current = contentStart
  while (current != contentEnd && current != null && current.notAfterEnd) {
    nodesToRemove += current
    logger.debug(s"Will remove content node: ${current.element}")
    current = current.following
  }

  // Add closer delimiter nodes to remove
  current = contentEnd

  // In triple asterisk case, remove ALL delimiters
  val closerDelimitersToRemove = if (finalNode.isInstanceOf[Emphasis] && emphasisType == "strong") {
    closer.length // Remove all delimiters
  } else {
    closer.length // Remove used delimiters
  }

  for (i <- 0 until closerDelimitersToRemove) {
    if (current != null && current.notAfterEnd) {
      nodesToRemove += current
      logger.debug(s"Will remove closer delimiter[$i]: ${current.element}")
      current = current.following
    }
  }

  // Remove all tracked nodes
  logger.debug(s"Removing ${nodesToRemove.size} nodes total")
  for ((node, i) <- nodesToRemove.zipWithIndex) {
    // Mark any delimiters in the stack inactive if they're being removed
    for (j <- delimiterStack.indices) {
      if (delimiterStack(j).isActive && (delimiterStack(j).node eq node)) {
        logger.debug(s"Found delimiter at stack index $j being removed - marking inactive")
        delimiterStack(j).isActive = false
      }
    }

    logger.debug(s"Unlinking node[$i]: $node, element: ${node.element}")
    try {
      node.unlink
    } catch {
      case e: Exception => logger.debug(s"Failed to unlink node: ${e.getMessage}")
    }
  }

  // Mark the original opener and closer as inactive in the stack
  val openerStackIdx = delimiterStack.indexWhere(d => d eq opener)
  if (openerStackIdx >= 0) {
    delimiterStack(openerStackIdx).isActive = false
    logger.debug(s"Marked original opener at stack index $openerStackIdx as inactive")
  }

  val closerStackIdx = delimiterStack.indexWhere(d => d eq closer)
  if (closerStackIdx >= 0) {
    delimiterStack(closerStackIdx).isActive = false
    logger.debug(s"Marked original closer at stack index $closerStackIdx as inactive")
  }

  logger.debug(s"Created ${if (finalNode.isInstanceOf[Emphasis]) "nested emphasis with strong" else emphasisType} node")
  logger.debug(s"==== createEmphasisNode END ====")
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
    case c: C =>
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

def lookForLinkOrImage(
    current: DLListNode[Inline],
    inlineNodes: DLList[Inline],
    delimiterStack: mutable.Stack[DelimiterInfo],
    linkRefs: immutable.Map[String, LinkReference],
    config: MarkdownConfig,
): DLListNode[Inline] = {
  logger.debug(s"lookForLinkOrImage at node: ${current.element}")

  // Find opening delimiter ([ or ![) on the stack using tail recursion
  // Find opening delimiter ([ or ![) on the stack
  @tailrec
  def findOpenerOf(char: Char, idx: Int): Option[DelimiterInfo] = {
    if (idx < 0) None
    else {
      val d = delimiterStack(idx)
      if (d.delimiterChar == char && d.isActive && isNodeValid(d.node)) Some(d)
      else findOpenerOf(char, idx - 1)
    }
  }

  // Try to close an image first (looking for '!['), otherwise a link '['
  val (openerInfo, isImage) = findOpenerOf('!', delimiterStack.size - 1) match {
    case Some(imgDelim) =>
      (imgDelim, true)
    case None =>
      findOpenerOf('[', delimiterStack.size - 1) match {
        case Some(linkDelim) =>
          (linkDelim, false)
        case None =>
          // No matching opener at all → literal `]`
          logger.debug("No opener found for ]")
          return current.following
      }
  }

  // If we found one, but it's not active, remove it and return literal `]`
  if (!openerInfo.isActive) {
    logger.debug("Found inactive opener")
    delimiterStack.remove(delimiterStack.indexOf(openerInfo))
    return current.following
  }

  // Parse ahead to see what kind of link/image we have
  val next = current.following

  // Case 1: Inline link/image [foo](url "title")
  if (isInlineLinkStart(next)) {
    logger.debug("Detected inline link/image")
    processInlineLink(openerInfo, current, next, isImage, delimiterStack, config)
  }

  // Case 2: Full reference link/image [foo][bar]
  else if (isFullReferenceLinkStart(next)) {
    logger.debug("Detected full reference link/image")
    processReferenceLink(openerInfo, current, next, isImage, delimiterStack, linkRefs, config)
  }

  // Case 3: Collapsed reference link/image [foo][]
  else if (isCollapsedReferenceLinkStart(next)) {
    logger.debug("Detected collapsed reference link/image")
    processCollapsedReferenceLink(openerInfo, current, next, isImage, delimiterStack, linkRefs, config)
  }

  // Case 4: Shortcut reference link/image [foo]
  else {
    logger.debug("Checking for shortcut reference link/image")
    processShortcutReferenceLink(openerInfo, current, isImage, delimiterStack, linkRefs, config)
  }
}

// Helper to check if we're at the start of an inline link
private def isInlineLinkStart(node: DLListNode[Inline]): Boolean = {
  if (node.isAfterEnd) return false

  node.element match {
    case c: C if !c.isLiteral && c.char == '(' => true
    case _                                     => false
  }
}

// Helper to check if we're at the start of a full reference link
private def isFullReferenceLinkStart(node: DLListNode[Inline]): Boolean = {
  if (node.isAfterEnd) return false

  node.element match {
    case c: C if !c.isLiteral && c.char == '[' => true
    case _                                     => false
  }
}

// Helper to check if we're at the start of a collapsed reference link
private def isCollapsedReferenceLinkStart(node: DLListNode[Inline]): Boolean = {
  if (node.isAfterEnd) return false

  // Check for []
  if (
    node.element.isInstanceOf[C] &&
    !node.element.asInstanceOf[C].isLiteral &&
    node.element.asInstanceOf[C].char == '['
  ) {

    val next = node.following
    if (
      !next.isAfterEnd &&
      next.element.isInstanceOf[C] &&
      !next.element.asInstanceOf[C].isLiteral &&
      next.element.asInstanceOf[C].char == ']'
    ) {
      return true
    }
  }

  false
}

// Update the processInlineLink function
private def processInlineLink(
    opener: DelimiterInfo,
    closeBracket: DLListNode[Inline],
    openParen: DLListNode[Inline],
    isImage: Boolean,
    delimiterStack: mutable.Stack[DelimiterInfo],
    config: MarkdownConfig,
): DLListNode[Inline] = {
  logger.debug("Processing inline link")

  // Extract link destination and title
  val (destination, title, afterLinkEnd) = parseInlineLinkDestination(openParen)

  if (destination == null) {
    // Not a valid link destination
    logger.debug("Invalid link destination")
    delimiterStack.remove(delimiterStack.indexOf(opener))
    return closeBracket.following
  }

  // Extract raw link text
  val linkText = extractInlinesBetween(opener.node.following, closeBracket)

  // Process emphasis and other formatting within the link text
  val processedLinkText = parseInline(linkText, Map(), config)

  val linkNode = if (isImage)
    Image(destination, title, processedLinkText)
  else
    Link(destination, title, processedLinkText)

  logger.debug(s"Created ${if (isImage) "image" else "link"} node with destination: $destination")

  // Replace opener node with link node
  opener.node.element = linkNode

  // Remove everything between opener and end position
  if (opener.node.following != afterLinkEnd) {
    opener.node.following.unlinkUntil(afterLinkEnd)
  }

  // Remove opener from stack
  delimiterStack.remove(delimiterStack.indexOf(opener))

  // If link (not image), set all previous [ delimiters inactive
  if (!isImage) {
    deactivateLinkDelimiters(delimiterStack)
  }

  opener.node.following
}

// Parse link destination and title from an inline link
private def parseInlineLinkDestination(openParen: DLListNode[Inline]): (String, Option[String], DLListNode[Inline]) = {
  logger.debug("Parsing inline link destination")

  @scala.annotation.tailrec
  def skipWhitespace(node: DLListNode[Inline]): DLListNode[Inline] = {
    if (
      node.notAfterEnd && node.element.isInstanceOf[C] &&
      (node.element.asInstanceOf[C].char == ' ' ||
        node.element.asInstanceOf[C].char == '\t' ||
        node.element.asInstanceOf[C].char == '\n')
    ) {
      skipWhitespace(node.following)
    } else {
      node
    }
  }

  // Skip initial whitespace
  var current = skipWhitespace(openParen.following)

  // Check for angle-bracketed destination
  val destination      = new StringBuilder
  var useAngleBrackets = false

  if (
    current.notAfterEnd && current.element.isInstanceOf[C] &&
    !current.element.asInstanceOf[C].isLiteral &&
    current.element.asInstanceOf[C].char == '<'
  ) {

    useAngleBrackets = true
    current = current.following

    // Parse until closing angle bracket using tail recursion
    @scala.annotation.tailrec
    def parseAngleBracketedDestination(node: DLListNode[Inline], dest: StringBuilder): (String, DLListNode[Inline]) = {
      if (node.isAfterEnd) {
        (null, openParen.following) // No closing bracket found
      } else if (
        node.element.isInstanceOf[C] &&
        !node.element.asInstanceOf[C].isLiteral &&
        node.element.asInstanceOf[C].char == '>'
      ) {
        (dest.toString, node.following) // Found closing bracket
      } else if (node.element.isInstanceOf[C]) {
        val c = node.element.asInstanceOf[C]
        // Check for invalid characters
        if (c.char == '\n' || (c.char == '<' && !c.isLiteral) || (c.char == '>' && !c.isLiteral)) {
          (null, openParen.following) // Invalid character
        } else {
          dest.append(c.char)
          parseAngleBracketedDestination(node.following, dest)
        }
      } else {
        (null, openParen.following) // Non-cursor element
      }
    }

    val (destContent, afterDestNode) = parseAngleBracketedDestination(current, destination)

    if (destContent == null) {
      return (null, None, openParen.following)
    }

    current = afterDestNode
  }
  // No angle brackets - parse until whitespace or closing paren
  else {
    // Parse regular destination using tail recursion
    @scala.annotation.tailrec
    def parseRegularDestination(
        node: DLListNode[Inline],
        dest: StringBuilder,
        openParens: Int,
    ): (String, DLListNode[Inline]) = {
      if (node.isAfterEnd) {
        (dest.toString, node) // End of input
      } else if (node.element.isInstanceOf[C]) {
        val c = node.element.asInstanceOf[C]

        // End destination at whitespace or closing paren (if no open parens)
        if (
          (c.char == ' ' || c.char == '\t' || c.char == '\n') ||
          (c.char == ')' && openParens == 0 && !c.isLiteral)
        ) {
          (dest.toString, node)
        } else {
          // Track nested parens
          val newOpenParens = if (!c.isLiteral) {
            if (c.char == '(') openParens + 1
            else if (c.char == ')' && openParens > 0) openParens - 1
            else openParens
          } else {
            openParens
          }

          dest.append(c.char)
          parseRegularDestination(node.following, dest, newOpenParens)
        }
      } else {
        (dest.toString, node) // Non-cursor element
      }
    }

    val (destContent, afterDestNode) = parseRegularDestination(current, destination, 0)
    current = afterDestNode
  }

  // If destination is empty, it's invalid
  if (destination.isEmpty) {
    return (null, None, openParen.following)
  }

  // Skip whitespace after destination
  current = skipWhitespace(current)

  // Check for title
  val (title, afterTitle) =
    if (
      current.notAfterEnd && current.element.isInstanceOf[C] &&
      !current.element.asInstanceOf[C].isLiteral &&
      (current.element.asInstanceOf[C].char == '"' ||
        current.element.asInstanceOf[C].char == '\'' ||
        current.element.asInstanceOf[C].char == '(')
    ) {

      val titleDelim   = current.element.asInstanceOf[C].char
      val closingDelim = if (titleDelim == '(') ')' else titleDelim
      val titleContent = new StringBuilder

      // Parse title using tail recursion
      @scala.annotation.tailrec
      def parseTitle(node: DLListNode[Inline], content: StringBuilder): (Option[String], DLListNode[Inline]) = {
        if (node.isAfterEnd) {
          (None, current.following) // No closing delimiter
        } else if (
          node.element.isInstanceOf[C] &&
          !node.element.asInstanceOf[C].isLiteral &&
          node.element.asInstanceOf[C].char == closingDelim
        ) {
          (Some(content.toString), node.following) // Found closing delimiter
        } else if (node.element.isInstanceOf[C]) {
          content.append(node.element.asInstanceOf[C].char)
          parseTitle(node.following, content)
        } else {
          (None, current.following) // Non-cursor element
        }
      }

      parseTitle(current.following, titleContent)
    } else {
      (None, current)
    }

  if (title.isDefined) {
    current = afterTitle
  }

  // Skip whitespace after title
  current = skipWhitespace(current)

  // Check for closing paren
  if (
    current.isAfterEnd ||
    !current.element.isInstanceOf[C] ||
    current.element.asInstanceOf[C].isLiteral ||
    current.element.asInstanceOf[C].char != ')'
  ) {
    // No closing paren
    return (null, None, openParen.following)
  }

  // Move past closing paren
  current = current.following

  (destination.toString, title, current)
}

// Process reference link [foo][bar]
private def processReferenceLink(
    opener: DelimiterInfo,
    closeBracket: DLListNode[Inline],
    labelStart: DLListNode[Inline],
    isImage: Boolean,
    delimiterStack: mutable.Stack[DelimiterInfo],
    linkRefs: immutable.Map[String, LinkReference],
    config: MarkdownConfig,
): DLListNode[Inline] = {
  logger.debug("Processing reference link")

  // Extract label
  val (label, afterLabelEnd) = extractReferenceLabel(labelStart)

  if (label == null) {
    // Not a valid reference label
    logger.debug("Invalid reference label")
    delimiterStack.remove(delimiterStack.indexOf(opener))
    return closeBracket.following
  }

  // Look up reference in linkRefs
  val normalizedLabel = normalizeLabel(label)
  val reference       = linkRefs.get(normalizedLabel)

  if (reference.isEmpty) {
    // Reference not found
    logger.debug(s"Reference not found for label: $normalizedLabel")
    delimiterStack.remove(delimiterStack.indexOf(opener))
    return closeBracket.following
  }

  // Create link/image node with everything between opener and closeBracket
  val linkText = extractInlinesBetween(opener.node.following, closeBracket)

  // Process emphasis within the link text (with stack_bottom = opener)
  val processedLinkText = parseInline(linkText, Map(), config)

  val linkNode = if (isImage)
    Image(reference.get.destination, reference.get.title, processedLinkText)
  else
    Link(reference.get.destination, reference.get.title, processedLinkText)

  logger.debug(s"Created ${if (isImage) "image" else "link"} node with reference: ${reference.get.destination}")

  // Replace opener node with link node
  opener.node.element = linkNode

  // Remove everything between opener and end position
  if (opener.node.following != afterLabelEnd) {
    opener.node.following.unlinkUntil(afterLabelEnd)
  }

  // Remove opener from stack
  delimiterStack.remove(delimiterStack.indexOf(opener))

  // If link (not image), set all previous [ delimiters inactive
  if (!isImage) {
    deactivateLinkDelimiters(delimiterStack)
  }

  opener.node.following
}

// Process collapsed reference link [foo][]
private def processCollapsedReferenceLink(
    opener: DelimiterInfo,
    closeBracket: DLListNode[Inline],
    labelStart: DLListNode[Inline],
    isImage: Boolean,
    delimiterStack: mutable.Stack[DelimiterInfo],
    linkRefs: immutable.Map[String, LinkReference],
    config: MarkdownConfig,
): DLListNode[Inline] = {
  logger.debug("Processing collapsed reference link")

  // Extract text between opener and closeBracket to use as label
  val linkText  = extractInlinesBetween(opener.node.following, closeBracket)
  val labelText = inlinesToPlainText(linkText)

  // Skip the empty label []
  val afterEmptyLabel = labelStart.following.following

  // Look up reference in linkRefs
  val normalizedLabel = normalizeLabel(labelText)
  val reference       = linkRefs.get(normalizedLabel)

  if (reference.isEmpty) {
    // Reference not found
    logger.debug(s"Reference not found for label: $normalizedLabel")
    delimiterStack.remove(delimiterStack.indexOf(opener))
    return closeBracket.following
  }

  // Process emphasis within the link text (with stack_bottom = opener)
  val processedLinkText = parseInline(linkText, Map(), config)

  val linkNode = if (isImage)
    Image(reference.get.destination, reference.get.title, processedLinkText)
  else
    Link(reference.get.destination, reference.get.title, processedLinkText)

  logger.debug(s"Created ${if (isImage) "image" else "link"} node with reference: ${reference.get.destination}")

  // Replace opener node with link node
  opener.node.element = linkNode

  // Remove everything between opener and end position
  if (opener.node.following != afterEmptyLabel) {
    opener.node.following.unlinkUntil(afterEmptyLabel)
  }

  // Remove opener from stack
  delimiterStack.remove(delimiterStack.indexOf(opener))

  // If link (not image), set all previous [ delimiters inactive
  if (!isImage) {
    deactivateLinkDelimiters(delimiterStack)
  }

  opener.node.following
}

// Process shortcut reference link [foo]
private def processShortcutReferenceLink(
    opener: DelimiterInfo,
    closeBracket: DLListNode[Inline],
    isImage: Boolean,
    delimiterStack: mutable.Stack[DelimiterInfo],
    linkRefs: immutable.Map[String, LinkReference],
    config: MarkdownConfig,
): DLListNode[Inline] = {
  logger.debug("Processing shortcut reference link")

  // Extract text between opener and closeBracket to use as label
  val linkText  = extractInlinesBetween(opener.node.following, closeBracket)
  val labelText = inlinesToPlainText(linkText)

  // Look up reference in linkRefs
  val normalizedLabel = normalizeLabel(labelText)
  val reference       = linkRefs.get(normalizedLabel)

  if (reference.isEmpty) {
    // Reference not found
    logger.debug(s"Reference not found for label: $normalizedLabel")
    delimiterStack.remove(delimiterStack.indexOf(opener))
    return closeBracket.following
  }

  // Process emphasis within the link text (with stack_bottom = opener)
  val processedLinkText = parseInline(linkText, Map(), config)

  val linkNode = if (isImage)
    Image(reference.get.destination, reference.get.title, processedLinkText)
  else
    Link(reference.get.destination, reference.get.title, processedLinkText)

  logger.debug(s"Created ${if (isImage) "image" else "link"} node with reference: ${reference.get.destination}")

  // Replace opener node with link node
  opener.node.element = linkNode

  // Remove everything between opener and the next position
  if (opener.node.following != closeBracket.following) {
    opener.node.following.unlinkUntil(closeBracket.following)
  }

  // Remove opener from stack
  delimiterStack.remove(delimiterStack.indexOf(opener))

  // If link (not image), set all previous [ delimiters inactive
  if (!isImage) {
    deactivateLinkDelimiters(delimiterStack)
  }

  opener.node.following
}

// Extract reference label [foo]
private def extractReferenceLabel(labelStart: DLListNode[Inline]): (String, DLListNode[Inline]) = {
  logger.debug("Extracting reference label")

  // Parse label using tail recursion
  @scala.annotation.tailrec
  def parseLabel(node: DLListNode[Inline], label: StringBuilder): (String, DLListNode[Inline]) = {
    if (node.isAfterEnd) {
      (null, labelStart.following) // End of input without closing bracket
    } else if (
      node.element.isInstanceOf[C] &&
      !node.element.asInstanceOf[C].isLiteral &&
      node.element.asInstanceOf[C].char == ']'
    ) {
      // Found closing bracket
      if (label.isEmpty || label.length > 999) {
        (null, labelStart.following) // Empty or too long label
      } else {
        (label.toString, node.following) // Valid label
      }
    } else if (node.element.isInstanceOf[C]) {
      val c = node.element.asInstanceOf[C]

      // Reference labels cannot contain [ unless escaped
      if (c.char == '[' && !c.isLiteral) {
        (null, labelStart.following)
      } else {
        // Append character to label
        label.append(c.char)
        parseLabel(node.following, label)
      }
    } else {
      // Non-cursor element - need to convert to text
      node.element match {
        case Text(content)     => label.append(content)
        case CodeSpan(content) => label.append(content)
        case _                 => label.append(node.element.toString) // Simple fallback
      }
      parseLabel(node.following, label)
    }
  }

  // Skip the opening [
  parseLabel(labelStart.following, new StringBuilder)
}

// Normalize label for lookup
private def normalizeLabel(label: String): String = {
  // Unicode case fold, collapse whitespace
  label.trim.toLowerCase.replaceAll("\\s+", " ")
}

// Set all [ delimiters inactive
private def deactivateLinkDelimiters(delimiterStack: mutable.Stack[DelimiterInfo]): Unit = {
  for (i <- delimiterStack.indices) {
    if (delimiterStack(i).isActive && delimiterStack(i).delimiterChar == '[') {
      delimiterStack(i).isActive = false
      logger.debug(s"Deactivated link delimiter at stack index $i")
    }
  }
}

// In parseInline.scala, add handling for $ delimiters

private def processMathSpan(node: DLListNode[Inline]): DLListNode[Inline] = {
  logger.debug(s"Starting processMathSpan on node: ${node.element}")

  // The opening node is already a $
  val openingNode = node
  var current     = node.following

  // Find the closing $
  while (current.notAfterEnd) {
    if (
      current.element.isInstanceOf[C] &&
      current.element.asInstanceOf[C].char == '$' &&
      !current.element.asInstanceOf[C].isLiteral
    ) {

      // Check if this is actually a closing $ and not part of a currency symbol
      // We need to check surrounding characters to avoid treating $20 as math
      val canBeMathCloser = isMathDelimiter(current)

      if (canBeMathCloser) {
        // Extract content between $ signs
        val mathContent = extractMathContent(openingNode.following, current)

        // Replace opening node with Math node
        openingNode.element = MathExpr(mathContent)

        // Remove everything between the opening and closing $
        if (openingNode.following != current.following) {
          openingNode.following.unlinkUntil(current.following)
        }

        return openingNode
      }
    }
    current = current.following
  }

  // No matching closing $, return original node
  node
}

// Helper to determine if a $ is a math delimiter or part of currency
private def isMathDelimiter(node: DLListNode[Inline]): Boolean = {
  // Get characters before and after the $
  val prevChar = if (node.preceding.notBeforeStart) {
    getCharFromNode(node.preceding)
  } else ' '

  val nextChar = if (node.following.notAfterEnd) {
    getCharFromNode(node.following)
  } else ' '

  // Not a math delimiter if:
  // 1. $ followed by a digit (likely currency)
  // 2. $ preceded by a digit without space (likely currency)

  !(nextChar.isDigit ||
    (prevChar.isDigit && !Character.isWhitespace(prevChar)))
}

// Extract math content between $ delimiters
private def extractMathContent(start: DLListNode[Inline], end: DLListNode[Inline]): String = {
  val builder = new StringBuilder
  var current = start

  while (current != end) {
    current.element match {
      case c: C    => builder.append(c.char)
      case t: Text => builder.append(t.content)
      case _       => // Skip other inline elements
    }
    current = current.following
  }

  builder.toString.trim
}
