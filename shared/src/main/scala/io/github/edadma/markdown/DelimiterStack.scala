package io.github.edadma.markdown

import scala.collection.mutable
import scala.util.control.Breaks._

// Delimiter types
sealed trait DelimiterType
case object Asterisk    extends DelimiterType // *
case object Underscore  extends DelimiterType // _
case object OpenBracket extends DelimiterType // [
case object OpenImage   extends DelimiterType // ![

// Delimiter node in the stack
class Delimiter(
    val position: Int,
    val delimiterType: DelimiterType,
    var length: Int,
    val canOpen: Boolean,
    val canClose: Boolean,
    var active: Boolean = true,
    var previous: Option[Delimiter] = None,
    var next: Option[Delimiter] = None,
)

class DelimiterStack(cursors: LazyList[Cursor]) {
  // Bottom and top of the stack
  private var bottom: Option[Delimiter] = None
  private var top: Option[Delimiter]    = None

  // Push a new delimiter onto the stack
  def push(position: Int, delimiterType: DelimiterType, length: Int, canOpen: Boolean, canClose: Boolean): Delimiter = {
    logger.debug(s"Pushing delimiter: type=$delimiterType, length=$length, canOpen=$canOpen, canClose=$canClose")

    val newDelimiter = Delimiter(position, delimiterType, length, canOpen, canClose)

    // Link with existing top
    if (top.isDefined) {
      val currentTop = top.get
      newDelimiter.previous = Some(currentTop)
      currentTop.next = Some(newDelimiter)
    }

    // If stack was empty, set bottom
    if (bottom.isEmpty) {
      bottom = Some(newDelimiter)
    }

    // Update top
    top = Some(newDelimiter)
    newDelimiter
  }

  // Remove a delimiter from the stack
  def remove(delimiter: Delimiter): Unit = {
    // Update previous node's next pointer
    delimiter.previous.foreach(prev => prev.next = delimiter.next)

    // Update next node's previous pointer
    delimiter.next.foreach(next => next.previous = delimiter.previous)

    // Update top if needed
    if (top.contains(delimiter)) {
      top = delimiter.previous
    }

    // Update bottom if needed
    if (bottom.contains(delimiter)) {
      bottom = delimiter.next
    }
  }

  // Look for link or image when ] is encountered
  def lookForLinkOrImage(inlines: List[Inline], curPos: Int): (List[Inline], Int) = {
    logger.debug("Looking for link or image after closing bracket")

    // Start from the top of the stack and look for an opening [ or ![
    var current                   = top
    var opener: Option[Delimiter] = None

    while (current.isDefined && opener.isEmpty) {
      val delimiter = current.get

      if (
        (delimiter.delimiterType == OpenBracket ||
          delimiter.delimiterType == OpenImage) && delimiter.active
      ) {
        opener = Some(delimiter)
      }

      current = delimiter.previous
    }

    if (opener.isEmpty || !opener.get.active) {
      // No active opener found - return literal text
      logger.debug("No active opener found, returning literal ']'")
      return (Text("]") :: inlines, curPos)
    }

    // Position after the closing bracket
    var pos = curPos + 1

    // Check for link/image syntax after the ]
    if (pos < cursors.size && cursors(pos).char == '(' && !cursors(pos).isLiteral) {
      logger.debug("Found opening parenthesis, parsing inline link/image")

      // Inline link/image
      pos += 1 // Skip the opening (

      // Skip whitespace
      while (pos < cursors.size && cursors(pos).char.isWhitespace) {
        pos += 1
      }

      // Parse destination
      val destStart  = pos
      var destEnd    = pos
      var parenDepth = 0

      while (pos < cursors.size) {
        val c = cursors(pos)

        if (c.char == '(' && !c.isLiteral) {
          parenDepth += 1
          pos += 1
        } else if (c.char == ')' && !c.isLiteral) {
          if (parenDepth == 0) {
            // End of destination/link
            destEnd = pos
            break
          } else {
            parenDepth -= 1
            pos += 1
          }
        } else if (c.char.isWhitespace && parenDepth == 0) {
          // Whitespace marks end of destination
          destEnd = pos
          break
        } else {
          pos += 1
        }
      }

      if (destEnd > destStart) {
        // We found a destination
        val destination = cursors.slice(destStart, destEnd).map(_.char).mkString
        logger.debug(s"Found destination: $destination")

        // Skip whitespace
        while (pos < cursors.size && cursors(pos).char.isWhitespace) {
          pos += 1
        }

        // Check for title
        var title: Option[String] = None

        if (
          pos < cursors.size &&
          (cursors(pos).char == '"' || cursors(pos).char == '\'' || cursors(pos).char == '(')
        ) {

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
            logger.debug(s"Found title: $title")
          }
        }

        // Skip to closing paren
        while (pos < cursors.size && cursors(pos).char != ')') {
          pos += 1
        }

        if (pos < cursors.size && cursors(pos).char == ')') {
          pos += 1 // Skip closing paren

          // Extract link text content from inlines
          // We need to find all inlines that were added since the opener

          // Calculate the position of the opener in the stream
          val openerPos = opener.get.position

          // Collect inlines to use as link content (working backwards)
          var linkInlines: List[Inline] = Nil
          var remainingInlines          = inlines
          var linkTextChars             = 0

          // We need to determine how many inlines to include in the link
          // We work backwards to find the content that was added after the opener position
          while (remainingInlines.nonEmpty && linkTextChars <= openerPos) {
            remainingInlines.head match {
              case Text(content) =>
                // Add length of text node to our running total
                linkTextChars += content.length

                if (linkTextChars > openerPos) {
                  // This text node contains the opener
                  // We only want the part after the opener bracket
                  val textAfterOpener = content.substring(
                    Math.min(content.length, content.length - (linkTextChars - openerPos) + 1),
                  )
                  if (textAfterOpener.nonEmpty) {
                    linkInlines = Text(textAfterOpener) :: linkInlines
                  }
                } else {
                  // This text node is entirely within the link text
                  linkInlines = remainingInlines.head :: linkInlines
                }

              case _ =>
                // Non-text inlines that were added after the opener are part of the link
                linkInlines = remainingInlines.head :: linkInlines
                linkTextChars += 1 // Approximate length of non-text nodes
            }

            remainingInlines = remainingInlines.tail
          }

          // Create the appropriate node type
          val newNode: Inline = if (opener.get.delimiterType == OpenImage) {
            logger.debug(s"Creating image node with ${linkInlines.length} inlines")
            Image(destination, title, linkInlines)
          } else {
            // For links, we need to deactivate all previous [ delimiters
            logger.debug(s"Creating link node with ${linkInlines.length} inlines")
            val linkNode = Link(destination, title, linkInlines)
            deactivateLinkOpeners()
            linkNode
          }

          // Remove the opening delimiter
          remove(opener.get)

          // Return the new node plus any inlines that weren't part of the link
          return (newNode :: remainingInlines, pos - 1)
        }
      }
    }

    // Reference link processing would go here (omitted for brevity)

    // If we get here, it was not a valid link
    logger.debug("No valid link syntax found")
    remove(opener.get)
    return (Text("]") :: inlines, curPos)
  }

  // Process all emphasis delimiters in the stack
//  def processEmphasis(inlines: List[Inline], stackBottom: Option[Delimiter] = None): List[Inline] = {
//    logger.debug(s"Processing inlines: $inlines")
//    logger.debug(s"Stack bottom: $stackBottom")
//
//    // If stack is empty or we're at the bottom, return inlines unchanged
//    if (bottom.isEmpty || (stackBottom.isDefined && stackBottom.get == bottom.get)) {
//      logger.debug("Returning inlines unchanged - empty stack or at bottom")
//      return inlines
//    }
//
//    // Convert to mutable buffer for processing
//    val inlinesBuffer = new mutable.ArrayBuffer[Inline]()
//    inlinesBuffer.appendAll(inlines)
//
//    // Map from delimiter positions to locations in the inlines buffer
//    val positionMap = new mutable.HashMap[Delimiter, (Int, Int)]()
//
//    // Fill the position map by scanning through inlines
//    mapDelimitersToPositions(inlinesBuffer, positionMap)
//    logger.debug(s"Position map: $positionMap")
//
//    // Track openers_bottom for each delimiter type/length/can_open combination
//    val openersBottom = new mutable.HashMap[(DelimiterType, Int, Boolean), Delimiter]()
//
//    // Start with the first delimiter above stackBottom
//    var current = if (stackBottom.isDefined) stackBottom.get.next else bottom
//
//    // Process all potential closers
//    while (current.isDefined) {
//      val closer = current.get
//
//      // Only process * and _ delimiters that can close
//      if (
//        (closer.delimiterType == Asterisk || closer.delimiterType == Underscore) &&
//        closer.canClose
//      ) {
//
//        // Look for a matching opener
//        val opener = findMatchingOpener(closer, openersBottom, stackBottom)
//
//        if (opener.isDefined) {
//          // Found a matching opener! Create emphasis
//          val useDelims = if (opener.get.length >= 2 && closer.length >= 2) 2 else 1
//
//          // Create the emphasis node based on the matching pair
//          val emphasisSuccess = createEmphasisNode(inlinesBuffer, positionMap, opener.get, closer, useDelims)
//
//          if (emphasisSuccess) {
//            // Update delimiters
//            opener.get.length -= useDelims
//            closer.length -= useDelims
//
//            // Clean up used delimiters
//            if (opener.get.length == 0) {
//              remove(opener.get)
//            }
//            if (closer.length == 0) {
//              val nextDelim = closer.next
//              remove(closer)
//              current = nextDelim
//              // Continue without incrementing current
//              if (current.isDefined) {
//                // Skip to next iteration of while loop
//                current = current // Dummy to avoid syntax error
//              } else {
//                // Break out of loop
//                current = None
//              }
//            }
//          } else {
//            current = current.get.next
//          }
//        } else {
//          // No matching opener
//          // Set this as the bottom for this type of delimiter
//          val key = (closer.delimiterType, closer.length % 3, closer.canOpen)
//          openersBottom(key) = closer
//
//          // If it can't be an opener, remove it
//          if (!closer.canOpen) {
//            val nextDelim = closer.next
//            remove(closer)
//            current = nextDelim
//          } else {
//            current = current.get.next
//          }
//        }
//      } else {
//        // Not a closer we can process, move to next
//        current = current.get.next
//      }
//    }
//
//    // Clean up - remove any remaining delimiters above stackBottom
//    removeDelimitersAbove(stackBottom)
//
//    // Return the transformed inlines
//    inlinesBuffer.toList
//  }

  def processEmphasis(inlines: List[Inline], stackBottom: Option[Delimiter] = None): List[Inline] = {
    // If stack is empty or we're at the bottom, return inlines unchanged
    if (bottom.isEmpty || (stackBottom.isDefined && stackBottom.get == bottom.get)) {
      return inlines
    }

    // Convert to mutable structure for easier modification
    val result = mutable.Buffer.from(inlines)

    // Create a map to track which text nodes contain which delimiters
    // Key: (Node index, Character offset)
    // Value: Delimiter
    val textNodeMap = new mutable.HashMap[(Int, Int), Delimiter]()

    // Fill the map by iterating through the text nodes
    var currentPos = 0
    for (i <- result.indices) {
      result(i) match {
        case textNode: Text =>
          // Check for delimiters in this text node
          var current = bottom
          while (current.isDefined) {
            val delimiter = current.get
            if (currentPos <= delimiter.position && delimiter.position < currentPos + textNode.content.length) {
              // This delimiter is in this text node
              val offset = delimiter.position - currentPos
              textNodeMap((i, offset)) = delimiter
            }
            current = current.get.next
          }
          currentPos += textNode.content.length
        case _ =>
          // Non-text nodes contribute to position but don't contain delimiters
          // For simplicity, we'll just count them as having a length of 1
          currentPos += 1
      }
    }

    // We'll use a bottom marker for each delimiter type/length/canOpen combination
    val openersBottom = new mutable.HashMap[(DelimiterType, Int, Boolean), Option[Delimiter]]()

    // Start processing from the first delimiter above stackBottom
    var currentDelimiter = if (stackBottom.isDefined) stackBottom.get.next else bottom

    // Process all potential closers
    while (currentDelimiter.isDefined) {
      val closer = currentDelimiter.get

      // Only process * and _ delimiters that can close
      if (
        (closer.delimiterType == Asterisk || closer.delimiterType == Underscore) &&
        closer.canClose && closer.active
      ) {

        // Determine the key for the opener-bottom tracking
        val closerLen = closer.length % 3
        val key       = (closer.delimiterType, closerLen, closer.canOpen)

        // Get the appropriate opener bottom for this type
        val openerBottom = openersBottom.getOrElse(key, stackBottom)

        // Look for matching opener
        var opener = findOpener(closer, openerBottom)

        if (opener.isDefined) {
          // Found a matching opener
          val useDelims = if (opener.get.length >= 2 && closer.length >= 2) 2 else 1

          // Apply the emphasis
          val success = applyEmphasis(result, textNodeMap, opener.get, closer, useDelims)

          if (success) {
            // Update delimiters and continue processing
            opener.get.length -= useDelims
            closer.length -= useDelims

            // If either delimiter is used up, remove it from stack
            if (opener.get.length == 0) {
              remove(opener.get)
            }

            if (closer.length == 0) {
              val next = closer.next
              remove(closer)
              currentDelimiter = next
            } else {
              currentDelimiter = currentDelimiter.flatMap(_.next)
            }
          } else {
            // Failed to apply emphasis, mark as bottom and continue
            openersBottom(key) = Some(closer)
            if (!closer.canOpen) {
              val next = closer.next
              remove(closer)
              currentDelimiter = next
            } else {
              currentDelimiter = currentDelimiter.flatMap(_.next)
            }
          }
        } else {
          // No matching opener found
          openersBottom(key) = Some(closer)
          if (!closer.canOpen) {
            // If it can't be an opener either, remove it
            val next = closer.next
            remove(closer)
            currentDelimiter = next
          } else {
            currentDelimiter = currentDelimiter.flatMap(_.next)
          }
        }
      } else {
        // Not a closer we can process, move to next
        currentDelimiter = currentDelimiter.flatMap(_.next)
      }
    }

    // Remove any remaining delimiters
    removeDelimitersAbove(stackBottom)

    result.toList
  }

  // Find a matching opener for a closer
  private def findOpener(closer: Delimiter, openerBottom: Option[Delimiter]): Option[Delimiter] = {
    var candidate = closer.previous

    while (
      candidate.isDefined &&
      (openerBottom.isEmpty || candidate.get != openerBottom.get)
    ) {

      val opener = candidate.get

      if (
        opener.delimiterType == closer.delimiterType &&
        opener.canOpen && opener.active
      ) {

        // Rule 9: If both opener and closer can form emphasis
        val sumDelims          = opener.length + closer.length
        val bothMultipleOf3    = opener.length % 3 == 0 && closer.length % 3 == 0
        val neitherMultipleOf3 = opener.length % 3 != 0 && closer.length % 3 != 0

        // Check if we can match these delimiters
        if (!(sumDelims % 3 == 0 && neitherMultipleOf3)) {
          return Some(opener)
        }
      }

      candidate = opener.previous
    }

    None
  }

  // Apply emphasis between opener and closer
  private def applyEmphasis(
      nodes: mutable.Buffer[Inline],
      textNodeMap: mutable.HashMap[(Int, Int), Delimiter],
      opener: Delimiter,
      closer: Delimiter,
      useDelims: Int,
  ): Boolean = {
    try {
      // Find the nodes containing our delimiters
      val openerPos = textNodeMap.find(_._2 == opener).map(_._1)
      val closerPos = textNodeMap.find(_._2 == closer).map(_._1)

      if (openerPos.isEmpty || closerPos.isEmpty) {
        return false
      }

      val (openerNodeIdx, openerOffset) = openerPos.get
      val (closerNodeIdx, closerOffset) = closerPos.get

      // If they're in the same text node, simple case
      if (openerNodeIdx == closerNodeIdx) {
        val textNode = nodes(openerNodeIdx).asInstanceOf[Text]
        val content  = textNode.content

        val beforeText   = content.substring(0, openerOffset)
        val emphasisText = content.substring(openerOffset + useDelims, closerOffset)
        val afterText    = content.substring(closerOffset + useDelims)

        // Create the emphasis node
        val emphNode = if (useDelims == 1) {
          Emphasis(List(Text(emphasisText)))
        } else {
          Strong(List(Text(emphasisText)))
        }

        // Replace the original node
        val newNodes = mutable.Buffer[Inline]()
        if (beforeText.nonEmpty) newNodes += Text(beforeText)
        newNodes += emphNode
        if (afterText.nonEmpty) newNodes += Text(afterText)

        nodes.remove(openerNodeIdx)
        nodes.insertAll(openerNodeIdx, newNodes)

        // Update the text node map for all delimiters
        updateTextNodeMap(textNodeMap, openerNodeIdx, newNodes.length - 1)

        return true
      } else {
        // Multi-node case (handling spans across multiple nodes)
        // This would be complex to implement fully here
        // For now, we'll handle the simplest case

        return false
      }
    } catch {
      case e: Exception =>
        return false
    }
  }

  // Update the text node map after modifying nodes
  private def updateTextNodeMap(
      map: mutable.HashMap[(Int, Int), Delimiter],
      startIdx: Int,
      change: Int,
  ): Unit = {
    val updatedMap = map.toSeq.map {
      case ((idx, offset), delim) =>
        if (idx > startIdx) {
          ((idx + change, offset), delim)
        } else {
          ((idx, offset), delim)
        }
    }

    map.clear()
    map ++= updatedMap
  }

  // Helper for loop control
  private def continue: Nothing = throw new ContinueException()

  private class ContinueException extends RuntimeException with scala.util.control.NoStackTrace

  // Find a matching opener for a closing delimiter
//  def findMatchingOpener(
//      closer: Delimiter,
//      openersBottom: mutable.Map[(DelimiterType, Int, Boolean), Delimiter],
//      stackBottom: Option[Delimiter],
//  ): Option[Delimiter] = {
//    logger.debug(s"Finding opener for closer: $closer")
//    val delimType = closer.delimiterType
//    val closerMod = closer.length % 3
//
//    // Look back through the entire stack for a matching opener
//    var current                  = closer.previous
//    var found: Option[Delimiter] = None
//
//    while (
//      current.isDefined &&
//      (stackBottom.isEmpty || current.get != stackBottom.get)
//    ) {
//      val delimiter = current.get
//      logger.debug(s"Checking potential opener: $delimiter")
//
//      // Check if this is a potential opener of the same type
//      if (delimiter.delimiterType == delimType && delimiter.canOpen) {
//        // Rule from the spec about delimiter length and emphasis
//        val sumIsMultipleOf3     = (delimiter.length + closer.length) % 3 == 0
//        val neitherIsMultipleOf3 = delimiter.length                   % 3 != 0 && closer.length % 3 != 0
//
//        logger.debug(s"Sum multiple of 3: $sumIsMultipleOf3, Neither multiple of 3: $neitherIsMultipleOf3")
//
//        if (!(sumIsMultipleOf3 && neitherIsMultipleOf3)) {
//          // Check against previous bottom delimiter for this type/length/opener status
//          val key            = (delimType, closerMod, closer.canOpen)
//          val previousBottom = openersBottom.get(key)
//
//          // If no previous bottom or current delimiter is above the previous bottom
//          if (
//            previousBottom.isEmpty ||
//            !previousBottom.exists(bottom =>
//              current.exists(current =>
//                current == bottom ||
//                  (bottom.previous.isDefined && bottom.previous.get == current),
//              ),
//            )
//          ) {
//
//            logger.debug("Found matching opener!")
//            found = current
//            // Update the bottom for this delimiter type
//            openersBottom(key) = delimiter
//            // Stop searching
//            current = None
//          } else {
//            logger.debug("Skipping due to bottom delimiter constraint")
//            current = delimiter.previous
//          }
//        } else {
//          logger.debug("Does not match due to delimiter length rule")
//          current = delimiter.previous
//        }
//      } else {
//        current = delimiter.previous
//      }
//    }
//
//    found
//  }

  def findMatchingOpener(
      closer: Delimiter,
      openersBottom: mutable.Map[(DelimiterType, Int, Boolean), Delimiter],
      stackBottom: Option[Delimiter],
  ): Option[Delimiter] = {
    logger.debug(s"Finding opener for closer: $closer")
    val delimType = closer.delimiterType
    val closerMod = closer.length % 3

    // Look back through the entire stack for a matching opener
    var current                  = closer.previous
    var found: Option[Delimiter] = None

    while (
      current.isDefined &&
      (stackBottom.isEmpty || current.get != stackBottom.get)
    ) {
      val delimiter = current.get
      logger.debug(s"Checking potential opener: $delimiter")

      // Check if this is a potential opener of the same type
      if (delimiter.delimiterType == delimType && delimiter.canOpen) {
        // Rule from the spec about delimiter length and emphasis
        val sumIsMultipleOf3     = (delimiter.length + closer.length) % 3 == 0
        val neitherIsMultipleOf3 = delimiter.length                   % 3 != 0 && closer.length % 3 != 0

        logger.debug(s"Sum multiple of 3: $sumIsMultipleOf3, Neither multiple of 3: $neitherIsMultipleOf3")

        if (!(sumIsMultipleOf3 && neitherIsMultipleOf3)) {
          logger.debug("Potential match found!")
          found = current
          // Stop searching
          current = None
        } else {
          logger.debug("Does not match due to delimiter length rule")
          current = delimiter.previous
        }
      } else {
        current = delimiter.previous
      }
    }

    found
  }

  // Map delimiters to positions in the inlines buffer
  private def mapDelimitersToPositions(
      inlines: mutable.ArrayBuffer[Inline],
      positionMap: mutable.HashMap[Delimiter, (Int, Int)],
  ): Unit = {
    logger.debug(s"Mapping delimiter positions in inlines: $inlines")

    var pos = 0
    for (i <- inlines.indices) {
      inlines(i) match {
        case Text(content) =>
          logger.debug(s"Processing text node at index $i: $content")
          // Check if any delimiters are in this text node
          var current = bottom
          while (current.isDefined) {
            val delimiter = current.get
            logger.debug(s"Checking delimiter: $delimiter")
            if (delimiter.position >= pos && delimiter.position < pos + content.length) {
              // This delimiter is in this text node
              logger.debug(s"Mapping delimiter ${delimiter} to position (${i}, ${delimiter.position - pos})")
              positionMap(delimiter) = (i, delimiter.position - pos)
            }
            current = current.get.next
          }
          pos += content.length
        case _ =>
          logger.debug(s"Skipping non-text node at index $i")
        // Skip non-text nodes
      }
    }
  }

  // Create an emphasis node from a matched opener/closer pair
  private def createEmphasisNode(
      inlines: mutable.ArrayBuffer[Inline],
      positionMap: mutable.HashMap[Delimiter, (Int, Int)],
      opener: Delimiter,
      closer: Delimiter,
      useDelims: Int,
  ): Boolean = {
    // Check if we have position info for both delimiters
    if (!positionMap.contains(opener) || !positionMap.contains(closer)) {
      logger.debug("Missing position info for delimiters")
      return false
    }

    val (openerNodeIdx, openerOffset) = positionMap(opener)
    val (closerNodeIdx, closerOffset) = positionMap(closer)

    // Handle the simple case: both in the same text node
    if (openerNodeIdx == closerNodeIdx) {
      // Same node case - implementation unchanged
      try {
        logger.debug(s"Processing emphasis in same text node: $openerNodeIdx")

        // Make sure the node index is valid
        if (openerNodeIdx >= inlines.length) {
          logger.debug(s"Invalid node index: $openerNodeIdx >= ${inlines.length}")
          return false
        }

        val textNode = inlines(openerNodeIdx).asInstanceOf[Text]
        val content  = textNode.content

        // Make sure the offsets are valid
        if (
          openerOffset >= content.length || closerOffset > content.length ||
          openerOffset + useDelims > content.length || closerOffset + useDelims > content.length
        ) {
          logger.debug(
            s"Invalid offsets: content.length=${content.length}, openerOffset=$openerOffset, closerOffset=$closerOffset",
          )
          return false
        }

        // Extract the parts: before, between, after
        val beforeText   = content.substring(0, openerOffset)
        val emphasisText = content.substring(openerOffset + useDelims, closerOffset)
        val afterText    = content.substring(closerOffset + useDelims)

        // Create emphasis node
        val emphNode = if (useDelims == 1) {
          Emphasis(List(Text(emphasisText)))
        } else {
          Strong(List(Text(emphasisText)))
        }

        // Replace the text node with the parts
        val newNodes = mutable.ArrayBuffer[Inline]()
        if (beforeText.nonEmpty) newNodes += Text(beforeText)
        newNodes += emphNode
        if (afterText.nonEmpty) newNodes += Text(afterText)

        // Update the inlines buffer
        inlines.remove(openerNodeIdx)
        inlines.insertAll(openerNodeIdx, newNodes)

        // Rebuild position map to ensure consistency after modification
        positionMap.clear()
        mapDelimitersToPositions(inlines, positionMap)

        return true
      } catch {
        case e: Exception =>
          logger.debug(s"Exception while creating emphasis node: ${e.getMessage}")
          return false
      }
    } else {
      // Handle case where delimiters are in different nodes
      logger.debug(s"Processing emphasis across multiple nodes: $openerNodeIdx to $closerNodeIdx")
      try {
        // Validate indices
        if (
          openerNodeIdx >= inlines.length || closerNodeIdx >= inlines.length ||
          openerNodeIdx > closerNodeIdx
        ) {
          logger.debug("Invalid node indices")
          return false
        }

        // Extract the nodes between opener and closer
        val startNode = inlines(openerNodeIdx).asInstanceOf[Text]
        val endNode   = inlines(closerNodeIdx).asInstanceOf[Text]

        // Handle start node - extract content after delimiter
        val startContent = startNode.content
        if (openerOffset + useDelims > startContent.length) {
          logger.debug(s"Invalid opener offset: $openerOffset in content of length ${startContent.length}")
          return false
        }
        val beforeStart = startContent.substring(0, openerOffset)
        val afterStart  = startContent.substring(openerOffset + useDelims)

        // Handle end node - extract content before delimiter
        val endContent = endNode.content
        if (closerOffset > endContent.length) {
          logger.debug(s"Invalid closer offset: $closerOffset in content of length ${endContent.length}")
          return false
        }
        val beforeEnd = endContent.substring(0, closerOffset)
        val afterEnd  = endContent.substring(closerOffset + useDelims)

        // Collect all nodes to be wrapped in emphasis
        val innerNodes = mutable.ArrayBuffer[Inline]()
        if (afterStart.nonEmpty) innerNodes += Text(afterStart)

        // Add all nodes between opener and closer
        for (i <- openerNodeIdx + 1 until closerNodeIdx) {
          innerNodes += inlines(i)
        }

        if (beforeEnd.nonEmpty) innerNodes += Text(beforeEnd)

        // Create the emphasis node
        val emphNode = if (useDelims == 1) {
          Emphasis(innerNodes.toList)
        } else {
          Strong(innerNodes.toList)
        }

        // Replace the original nodes with our new structure
        val nodesToRemove = closerNodeIdx - openerNodeIdx + 1
        val newNodes      = mutable.ArrayBuffer[Inline]()
        if (beforeStart.nonEmpty) newNodes += Text(beforeStart)
        newNodes += emphNode
        if (afterEnd.nonEmpty) newNodes += Text(afterEnd)

        inlines.remove(openerNodeIdx, nodesToRemove)
        inlines.insertAll(openerNodeIdx, newNodes)

        // Rebuild position map
        positionMap.clear()
        mapDelimitersToPositions(inlines, positionMap)

        return true
      } catch {
        case e: Exception =>
          logger.debug(s"Exception processing multi-node emphasis: ${e.getMessage}")
          return false
      }
    }
  }

  // Remove all delimiters above a certain point
  private def removeDelimitersAbove(stackBottom: Option[Delimiter]): Unit = {
    var current = if (stackBottom.isDefined) stackBottom.get.next else bottom
    while (current.isDefined) {
      val next = current.get.next
      remove(current.get)
      current = next
    }
  }

  // Deactivate all link openers (when a link is successfully processed)
  def deactivateLinkOpeners(): Unit = {
    var current = bottom
    while (current.isDefined) {
      val delimiter = current.get
      if (delimiter.delimiterType == OpenBracket) {
        delimiter.active = false
      }
      current = delimiter.next
    }
  }
}
