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
      return (Text("]") :: inlines, curPos)
    }

    // Position after the closing bracket
    var pos = curPos + 1

    // Check if we have link/image syntax after the ]
    if (pos < cursors.size && cursors(pos).char == '(' && !cursors(pos).isLiteral) {
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

      breakable {
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
      }

      if (destEnd > destStart) {
        // We found a destination
        val destination = cursors.slice(destStart, destEnd).map(_.char).mkString

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
          breakable {
            while (
              pos < cursors.size &&
              cursors(pos).char != closingDelim &&
              cursors(pos).char != '\n'
            ) {
              pos += 1
            }
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

          // Here's the fix: we need to properly extract the link text content
          // First, find the text nodes that were created between the opener and closer

          // Calculate how many inlines to skip by looking at the nodes added
          // since the opener was pushed to the stack

          // We need to find the text between the opening [ and the closing ]
          // Get the content by examining the inlines list

          // First, figure out how many nodes we need to examine
          // The nodes are in reverse order (newest first)
          var textNodes: List[Inline] = Nil
          var remainingInlines        = inlines
          var foundBracketNode        = false
          var skipCount               = 0

          // Find the bracket in the inlines list
          breakable {
            while (remainingInlines.nonEmpty && !foundBracketNode) {
              remainingInlines.head match {
                case Text(content) if content.contains("[") || content.contains("![") =>
                  foundBracketNode = true
                  // Found the opening bracket node

                  // Create a new text node without the bracket(s)
                  val bracketText = remainingInlines.head.asInstanceOf[Text]
                  if (opener.get.delimiterType == OpenBracket) {
                    // For [text], remove the [
                    if (bracketText.content == "[") {
                      // Skip this node entirely
                      skipCount += 1
                    } else {
                      // Extract only the content after the [
                      val bracketPos = bracketText.content.indexOf("[")
                      if (bracketPos >= 0 && bracketPos < bracketText.content.length - 1) {
                        // There's content after the [
                        textNodes = Text(bracketText.content.substring(bracketPos + 1)) :: textNodes
                      }
                      skipCount += 1
                    }
                  } else {
                    // For ![text], remove the ![
                    if (bracketText.content == "![") {
                      // Skip this node entirely
                      skipCount += 1
                    } else {
                      // Extract only the content after the ![
                      val bracketPos = bracketText.content.indexOf("![")
                      if (bracketPos >= 0 && bracketPos < bracketText.content.length - 2) {
                        // There's content after the ![
                        textNodes = Text(bracketText.content.substring(bracketPos + 2)) :: textNodes
                      }
                      skipCount += 1
                    }
                  }
                  break

                case _ =>
                  // This is part of the content inside the brackets
                  textNodes = remainingInlines.head :: textNodes
                  skipCount += 1
                  remainingInlines = remainingInlines.tail
              }
            }
          }

          // Process any accumulated text nodes
          val linkInlines = textNodes

          // Create appropriate node
          val newNode: Inline = if (opener.get.delimiterType == OpenImage) {
            Image(destination, title, linkInlines)
          } else {
            // For links, we need to deactivate all previous [ delimiters
            // First create the node
            val link = Link(destination, title, linkInlines)

            // Then deactivate link openers
            deactivateLinkOpeners()

            // Return the node
            link
          }

          // Remove the opening delimiter
          remove(opener.get)

          // Return the new node and updated position
          return (newNode :: inlines.drop(skipCount), pos - 1)
        }
      }
    }

    // If we get here, it means we didn't find a valid link syntax
    // Remove opener from stack and return literal "]"
    remove(opener.get)
    (Text("]") :: inlines, curPos)
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
    logger.debug(s"Processing inlines: $inlines")
    logger.debug(s"Stack bottom: $stackBottom")

    // If stack is empty or we're at the bottom, return inlines unchanged
    if (bottom.isEmpty || (stackBottom.isDefined && stackBottom.get == bottom.get)) {
      logger.debug("Returning inlines unchanged - empty stack or at bottom")
      return inlines
    }

    // Convert to mutable buffer for processing
    val inlinesBuffer = new mutable.ArrayBuffer[Inline]()
    inlinesBuffer.appendAll(inlines)

    // Map from delimiter positions to locations in the inlines buffer
    val positionMap = new mutable.HashMap[Delimiter, (Int, Int)]()

    // Fill the position map by scanning through inlines
    mapDelimitersToPositions(inlinesBuffer, positionMap)
    logger.debug(s"Position map: $positionMap")

    // Track openers_bottom for each delimiter type/length/can_open combination
    val openersBottom = new mutable.HashMap[(DelimiterType, Int, Boolean), Delimiter]()

    // Start with the first delimiter above stackBottom
    var current = if (stackBottom.isDefined) stackBottom.get.next else bottom

    // Process all potential closers
    while (current.isDefined) {
      val closer = current.get

      // Only process * and _ delimiters that can close
      if (
        (closer.delimiterType == Asterisk || closer.delimiterType == Underscore) &&
        closer.canClose
      ) {
        logger.debug(s"Processing potential closer: $closer")

        // Look for a matching opener
        val opener = findMatchingOpener(closer, openersBottom, stackBottom)

        if (opener.isDefined) {
          logger.debug(s"Found matching opener: ${opener.get}")
          // Found a matching opener! Create emphasis
          val useDelims = if (opener.get.length >= 2 && closer.length >= 2) 2 else 1

          // Create the emphasis node based on the matching pair
          val emphasisSuccess = createEmphasisNode(inlinesBuffer, positionMap, opener.get, closer, useDelims)

          if (emphasisSuccess) {
            logger.debug("Emphasis node created successfully")
            // Update delimiters
            opener.get.length -= useDelims
            closer.length -= useDelims

            // Clean up used delimiters
            if (opener.get.length == 0) {
              remove(opener.get)
            }
            if (closer.length == 0) {
              val nextDelim = closer.next
              remove(closer)
              current = nextDelim
              // Continue without incrementing current
              if (current.isDefined) {
                // Skip to next iteration of while loop
                current = current // Dummy to avoid syntax error
              } else {
                // Break out of loop
                current = None
              }
            }
          } else {
            logger.debug("Failed to create emphasis node")
            current = current.get.next
          }
        } else {
          logger.debug("No matching opener found")
          // No matching opener
          // Set this as the bottom for this type of delimiter
          val key = (closer.delimiterType, closer.length % 3, closer.canOpen)
          openersBottom(key) = closer

          // If it can't be an opener, remove it
          if (!closer.canOpen) {
            val nextDelim = closer.next
            remove(closer)
            current = nextDelim
          } else {
            current = current.get.next
          }
        }
      } else {
        // Not a closer we can process, move to next
        current = current.get.next
      }
    }

    // Clean up - remove any remaining delimiters above stackBottom
    removeDelimitersAbove(stackBottom)

    // Return the transformed inlines
    inlinesBuffer.toList
  }

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
    // Check if we have position info for both delimiter
    if (!positionMap.contains(opener) || !positionMap.contains(closer)) {
      return false
    }

    val (openerNodeIdx, openerOffset) = positionMap(opener)
    val (closerNodeIdx, closerOffset) = positionMap(closer)

    // Handle the simple case: both in the same text node
    if (openerNodeIdx == closerNodeIdx) {
      val textNode = inlines(openerNodeIdx).asInstanceOf[Text]
      val content  = textNode.content

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

      // TODO: Update position map here for multi-delimiter cases

      return true
    } else {
      // TODO: Handle the complex case: delimiters in different nodes
      // For now, just return false to indicate we couldn't process this
      return false
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
