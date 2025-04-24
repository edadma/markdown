package io.github.edadma.markdown

import io.github.edadma.dllist.DLList

import scala.annotation.tailrec

// Standalone inline parsing function
def parseInline(inlines: List[Inline]): List[Inline] = {
  // Create initial DLList with character nodes
  val inlineNodes = DLList[Inline](inlines*)

  // Initialize empty delimiter stack (will be used for emphasis/links)
//  val delimiterStack = DLList[DelimiterInfo]()

//  def processCodeSpan(node: inlineNodes.Node, nodes: DLList[Inline]): inlineNodes.Node = {
//    // Count the consecutive backticks in the opening delimiter
//    val openingNode  = node
//    var openingCount = 0
//    var current      = node
//
//    // Count consecutive backticks in the opening delimiter
//    while (
//      current.notAfterEnd &&
//      current.element.isInstanceOf[Cursor] &&
//      current.element.asInstanceOf[Cursor].char == '`' &&
//      !current.element.asInstanceOf[Cursor].isLiteral
//    ) {
//      openingCount += 1
//      current = current.following
//    }
//
//    // If we found an opening delimiter, look for a matching closing one
//    if (openingCount > 0) {
//      // Remember where content starts
//      val contentStart = current
//      var foundClosing = false
//
//      // Look for closing delimiter
//      while (current.notAfterEnd && !foundClosing) {
//        if (
//          current.element.isInstanceOf[Cursor] &&
//          current.element.asInstanceOf[Cursor].char == '`' &&
//          !current.element.asInstanceOf[Cursor].isLiteral
//        ) {
//
//          // Count consecutive backticks to see if we have a match
//          var closingCount = 0
//          var closingStart = current
//
//          while (
//            current.notAfterEnd &&
//            current.element.isInstanceOf[Cursor] &&
//            current.element.asInstanceOf[Cursor].char == '`' &&
//            !current.element.asInstanceOf[Cursor].isLiteral
//          ) {
//            closingCount += 1
//            current = current.following
//          }
//
//          // If counts match, we found our closing delimiter
//          if (closingCount == openingCount) {
//            foundClosing = true
//            val contentEnd = closingStart
//
//            // Extract and process content
//            val content = extractAndProcessCodeSpanContent(contentStart, contentEnd)
//
//            // Replace the opening node with a CodeSpan and unlink everything in between
//            openingNode.element = CodeSpan(content)
//
//            // Unlink everything from after opening delimiter to end of closing delimiter
//            if (openingNode.following != current) {
//              openingNode.following.unlinkUntil(current)
//            }
//
//            // Return the CodeSpan node for continued processing
//            return openingNode
//          }
//          // If counts don't match, continue searching
//        } else {
//          current = current.following
//        }
//      }
//
//      // If no matching closing delimiter found, just return the original node unchanged
//      // The opening backticks will be treated as regular text
//      return node
//    }
//
//    // If we somehow got here, just return the original node
//    node
//  }
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
            println(inlineNodes)
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

//            case '<' =>
//              // Process HTML tag or autolink
//              current = processHtmlOrAutolink(current, inlineNodes)
//
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
//
//            case '\n' =>
//              // Process line break
//              current = processLineBreak(current, inlineNodes)

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
