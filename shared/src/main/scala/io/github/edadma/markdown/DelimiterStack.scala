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
case class Delimiter(
    // Position in the input stream
    position: Int,

    // Type of delimiter
    delimiterType: DelimiterType,

    // Number of delimiters (e.g., ** is 2)
    length: Int,

    // Whether this delimiter can open emphasis
    canOpen: Boolean,

    // Whether this delimiter can close emphasis
    canClose: Boolean,

    // Whether this delimiter is still active - changed to var so it can be modified
    var active: Boolean = true,

    // Links for doubly-linked list
    var previous: Option[Delimiter] = None,
    var next: Option[Delimiter] = None,
)

class DelimiterStack(cursors: LazyList[Cursor]) {
  // Bottom and top of the stack
  private var bottom: Option[Delimiter] = None
  private var top: Option[Delimiter]    = None

  // Push a new delimiter onto the stack
  def push(position: Int, delimiterType: DelimiterType, length: Int, canOpen: Boolean, canClose: Boolean): Delimiter = {
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

          // Create the content for the link/image
          // We'll do this by manually extracting text between opener and current position
          val startContentPos = opener.get.position +
            (if (opener.get.delimiterType == OpenImage) 2 else 1)
          val endContentPos = curPos

          // Collect all text nodes between these positions
          val textContent = inlines
            .takeWhile(inline => {
              // We'd need position tracking for each inline node for this to work properly
              // This is a simplified approach that won't handle nested content well
              true
            })
            .collect {
              case Text(content) => content
            }
            .mkString

          // Parse the textContent for inlines - in a real implementation we'd
          // actually call the inline parser recursively here
          val linkInlines = List(Text(textContent))

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
          return (newNode :: inlines.drop(linkInlines.length), pos - 1)
        }
      }
    }

    // If we get here, it means we didn't find a valid link syntax
    // Remove opener from stack and return literal "]"
    remove(opener.get)
    (Text("]") :: inlines, curPos)
  }

  // Process all emphasis delimiters in the stack
  def processEmphasis(stackBottom: Option[Delimiter] = None): List[Inline] = {
    // This is a simplified version of the emphasis processing algorithm
    // In a real implementation, we'd track openers_bottom, process closers in order, etc.

    // For now, just return an empty list - in practice this would transform inlines
    List.empty[Inline]
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
