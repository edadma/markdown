package io.github.edadma.markdown

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

    // Whether this delimiter is still active
    var active: Boolean = true,

    // Links for doubly-linked list
    var previous: Option[Delimiter] = None,
    var next: Option[Delimiter] = None,
)

class DelimiterStack {
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
  def lookForLinkOrImage(inlines: List[Inline], pos: Int): (List[Inline], Int) = {
    // Start from the top of the stack
    var current = top

    // Look for an opening [ or ![
    while (current.isDefined) {
      val delimiter = current.get

      if (
        (delimiter.delimiterType == OpenBracket ||
          delimiter.delimiterType == OpenImage) && delimiter.active
      ) {
        // Found a potential opener - need to check if we have a valid link/image
        // This would be where we'd parse destinations and titles

        // Simplified logic for now - just return the existing inlines and position
        // In a real implementation, we'd modify inlines and pos based on what we found
        return (inlines, pos)
      }

      current = delimiter.previous
    }

    // If no valid opener found, just add a literal ']'
    (Text("]") :: inlines, pos)
  }

  // Process all emphasis delimiters in the stack
  def processEmphasis(bottomBoundary: Option[Delimiter] = None): List[Inline] = {
    // For now, return an empty list - this would be replaced with the full implementation
    // of the emphasis processing algorithm
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
