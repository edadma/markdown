package io.github.edadma.markdown

// Add these to Node.scala:
// case class CodeSpan(content: String) extends Inline
// case class Emphasis(inlines: List[Inline]) extends Inline
// case class Strong(inlines: List[Inline]) extends Inline
// case class Link(destination: String, title: Option[String], inlines: List[Inline]) extends Inline
// case class Image(destination: String, title: Option[String], inlines: List[Inline]) extends Inline
// case class AutoLink(destination: String, text: String) extends Inline
// case class RawHTML(content: String) extends Inline

// Delimiter stack data types
sealed trait DelimiterType
case object Asterisk    extends DelimiterType
case object Underscore  extends DelimiterType
case object OpenBracket extends DelimiterType
case object OpenImage   extends DelimiterType

case class Delimiter(
    textNode: Text, // The text node containing the delimiter chars
    position: Int,  // Position in the inlines list
    length: Int,    // Number of delimiter chars
    delimiterType: DelimiterType,
    active: Boolean = true,
    canOpen: Boolean,  // Is this potentially an opener?
    canClose: Boolean, // Is this potentially a closer?
    next: Option[Delimiter] = None,
    prev: Option[Delimiter] = None,
)

def parseInline(cursors: LazyList[Cursor]): List[Inline] = {
  var pos                   = 0
  var inlines: List[Inline] = Nil

  // Delimiter stack - implemented as linked list
  var lastDelimiter: Option[Delimiter] = None

  // Reference for link reference definitions (should be passed in)
  // This would be populated during block parsing phase
  val linkReferences: Map[String, (String, Option[String])] = Map()

  // Add an inline element to our result list
  def addInline(inlineNode: Inline): Unit = {
    inlines = inlineNode :: inlines
  }

  // Push a text node and return it
  def pushTextNode(text: String): Text = {
    val node = Text(text)
    addInline(node)
    node
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

      // Process content according to spec:
      // 1. Replace line endings with spaces
      // 2. If content starts/ends with space and has non-space content,
      //    remove one space from start/end
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
      // No matching closing backticks, treat as literal text
      pos = startPos
      addInline(Text("`" * openingBackticks))
      pos += openingBackticks - 1 // Adjust for the loop increment
    }
  }

  // Process an autolink or HTML tag
  def handlePotentialHTMLOrAutolink(): Unit = {
    val startPos = pos

    // Look ahead to see if this could be an autolink
    if (pos + 1 < cursors.size) {
      // Collect characters until '>'
      var i            = pos + 1
      var content      = new StringBuilder()
      var foundClosing = false

      while (i < cursors.size && !foundClosing) {
        val c = cursors(i).char
        if (c == '>') {
          foundClosing = true
        } else {
          content.append(c)
          i += 1
        }
      }

      if (foundClosing) {
        val potentialURI = content.toString()

        // Check if it's a URL autolink
        if (isValidAutolink(potentialURI)) {
          // Found an autolink
          val uri = potentialURI
          addInline(AutoLink(uri, uri))
          pos = i // Skip to after the closing '>'
        }
        // Check if it's an email autolink
        else if (isValidEmailAutolink(potentialURI)) {
          val email = potentialURI
          addInline(AutoLink(s"mailto:$email", email))
          pos = i // Skip to after the closing '>'
        }
        // Check if it's an HTML tag
        else if (isHTMLTag("<" + potentialURI + ">")) {
          val html = "<" + potentialURI + ">"
          addInline(RawHTML(html))
          pos = i // Skip to after the closing '>'
        } else {
          // Not an autolink or HTML tag, treat as literal text
          addInline(Text("<"))
        }
      } else {
        // No closing '>', treat as literal text
        addInline(Text("<"))
      }
    } else {
      // Just a '<' character at the end
      addInline(Text("<"))
    }
  }

  // Very simplified autolink validation
  def isValidAutolink(uri: String): Boolean = {
    // A valid autolink starts with a scheme followed by ":" and does not contain whitespace
    val schemePattern = "^[a-zA-Z][a-zA-Z0-9+.-]{1,31}:".r
    schemePattern.findPrefixOf(uri).isDefined && !uri.exists(c => c.isWhitespace || c == '<')
  }

  // Simplified email autolink validation
  def isValidEmailAutolink(email: String): Boolean = {
    // Very basic check for email format (a@b.c)
    val emailPattern =
      "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$".r
    emailPattern.matches(email)
  }

  // Very simplified HTML tag validation
  def isHTMLTag(html: String): Boolean = {
    // This is a very simplified check - real implementation would be more complex
    html.startsWith("<") && html.endsWith(">") && html.length > 2
  }

  // Handle emphasis and strong emphasis delimiter (* or _)
  def handleEmphasisDelimiter(cursor: Cursor): Unit = {
    // Implementation will come in Phase 3
    // For now, just handle as plain text
    addInline(Text(cursor.char.toString))
  }

  // Handle line breaks
  def handleLineBreak(cursor: Cursor): Unit = {
    // Check if it's a hard line break (preceded by two or more spaces)
    var isHardBreak = false

    // Check for trailing spaces (>=2)
    if (pos > 0) {
      var spacesCount = 0
      var i           = pos - 1
      while (i >= 0 && cursors(i).char == ' ') {
        spacesCount += 1
        i -= 1
      }

      isHardBreak = spacesCount >= 2
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

  // Main loop - process each cursor (optimized version)
  // Main loop - process each cursor (optimized version)
  while (pos < cursors.size) {
    val cursor = cursors(pos)

    if (cursor.char == '`' && !cursor.isLiteral) {
      // Code spans
      handleCodeSpan()
    } else if ((cursor.char == '*' || cursor.char == '_') && !cursor.isLiteral) {
      // Emphasis delimiters
      handleEmphasisDelimiter(cursor)
    } else if (cursor.char == '<' && !cursor.isLiteral) {
      // Autolinks and HTML
      handlePotentialHTMLOrAutolink()
    } else if (cursor.char == '\n') {
      // Line breaks
      handleLineBreak(cursor)
    } else {
      // Plain text - collect a run of text characters
      val startPos = pos
      var textEnd  = pos

      // Advance until we find a special character
      var shouldContinue = true
      while (textEnd < cursors.size && shouldContinue) {
        val c = cursors(textEnd)
        if (
          (c.char == '`' || c.char == '*' || c.char == '_' ||
            c.char == '<' || c.char == '[' || c.char == ']' ||
            c.char == '!' || c.char == '\n') && !c.isLiteral
        ) {
          // Found a special character, stop collecting text
          shouldContinue = false
        } else {
          textEnd += 1
        }
      }

      // Create a text node for the run
      if (textEnd > startPos) {
        val textContent = cursors.slice(startPos, textEnd).map(_.char).mkString
        addInline(Text(textContent))
        pos = textEnd - 1 // -1 because the loop will increment
      } else {
        // Single character
        addInline(Text(cursor.char.toString))
      }
    }

    pos += 1
  }

  // Return the resulting inlines in the correct order
  inlines.reverse
}
