package io.github.edadma.markdown

import scala.collection.immutable.LazyList

class InputReader(input: String) {
  // Step 1: Normalize the input
  private val normalizedInput = normalizeInput(input)

  // Main public stream of cursors
  lazy val stream: LazyList[Cursor] = processToCursors(normalizedInput)

  // Normalize input - handle null characters and line endings
  private def normalizeInput(input: String): String = {
    input.replace("\u0000", "\uFFFD") // Replace null with replacement character
      .replace("\r\n", "\n")          // Normalize CRLF
      .replace("\r", "\n")            // Normalize CR
  }

  // Process input into a stream of cursors
  private def processToCursors(input: String): LazyList[Cursor] = {
    def process(index: Int, pos: Int, line: Int, col: Int): LazyList[Cursor] = {
      if (index >= input.length) {
        LazyList(EndOfInput) // End of input marker
      } else {
        val current = input.charAt(index)

        // Handle escape sequences
        if (current == '\\' && index + 1 < input.length) {
          val next = input.charAt(index + 1)
          if (isAsciiPunctuation(next)) {
            // Create cursor for the escaped character
            val cursor = Cursor(next, pos + 1, line, col + 1, true)
            cursor #:: process(index + 2, pos + 2, line, col + 2)
          } else {
            // Not escaped, just a regular backslash
            val cursor = Cursor(current, pos, line, col, false)
            cursor #:: process(index + 1, pos + 1, line, col + 1)
          }
        }
        // Handle entity references
        else if (current == '&') {
          val entityResult = parseEntityReference(input, index)
          if (entityResult.isDefined) {
            val (entityChar, entityLength) = entityResult.get
            val cursor                     = Cursor(entityChar, pos, line, col, false)
            cursor #:: process(index + entityLength, pos + entityLength, line, col + 1)
          } else {
            // Not a valid entity, treat as regular character
            val cursor = Cursor(current, pos, line, col, false)
            cursor #:: process(index + 1, pos + 1, line, col + 1)
          }
        }
        // Handle line endings
        else if (current == '\n') {
          val cursor = Cursor(current, pos, line, col, false)
          cursor #:: process(index + 1, pos + 1, line + 1, 0) // Reset column, increment line
        }
        // Handle tabs (in base stream they're passed through)
        else if (current == '\t') {
          val cursor = Cursor(current, pos, line, col, false)
          // Tab advances to next tab stop (multiples of 4)
          val nextCol = col + (4 - (col % 4))
          cursor #:: process(index + 1, pos + 1, line, nextCol)
        }
        // Regular character
        else {
          val cursor = Cursor(current, pos, line, col, false)
          cursor #:: process(index + 1, pos + 1, line, col + 1)
        }
      }
    }

    process(0, 0, 0, 0)
  }

  // Parse entity references (named, decimal, hexadecimal)
  private def parseEntityReference(input: String, startIndex: Int): Option[(Char, Int)] = {
    // This is a simplified implementation - focusing on common entities first
    val commonEntities = Map(
      "amp"  -> '&',
      "lt"   -> '<',
      "gt"   -> '>',
      "quot" -> '"',
      "apos" -> '\'',
      // Add more common entities as needed
    )

    // Check for named entity: &name;
    val namedEntityRegex = "&([a-zA-Z0-9]+);".r
    val inputSubstring   = input.substring(startIndex)
    namedEntityRegex.findPrefixMatchOf(inputSubstring).flatMap { m =>
      val name = m.group(1)
      commonEntities.get(name).map(char => (char, m.end))
    }.orElse {
      // Check for decimal entity: &#dddd;
      val decimalEntityRegex = "&#([0-9]{1,7});".r
      decimalEntityRegex.findPrefixMatchOf(inputSubstring).map { m =>
        val codePoint = m.group(1).toInt
        (codePoint.toChar, m.end)
      }.orElse {
        // Check for hex entity: &#xhhhh;
        val hexEntityRegex = "&#[xX]([0-9a-fA-F]{1,6});".r
        hexEntityRegex.findPrefixMatchOf(inputSubstring).map { m =>
          val codePoint = Integer.parseInt(m.group(1), 16)
          (codePoint.toChar, m.end)
        }
      }
    }
  }

  // Check if a character is ASCII punctuation
  private def isAsciiPunctuation(c: Char): Boolean = {
    (c >= '!' && c <= '/') ||
    (c >= ':' && c <= '@') ||
    (c >= '[' && c <= '`') ||
    (c >= '{' && c <= '~')
  }

  // Get a stream with tabs expanded to spaces in block structure contexts
  def getStreamWithExpandedTabs(): LazyList[Cursor] = {
    expandTabsInStream(stream)
  }

  // Expand tabs to spaces in a stream of cursors
  private def expandTabsInStream(stream: LazyList[Cursor]): LazyList[Cursor] = {
    def expand(remaining: LazyList[Cursor], col: Int): LazyList[Cursor] = {
      if (remaining.isEmpty) {
        LazyList.empty
      } else {
        val cursor = remaining.head

        if (cursor.char == '\t') {
          // Calculate spaces needed for this tab
          val spacesNeeded = 4 - (col % 4)

          // Create cursors for the spaces
          val spaceCursors = (0 until spacesNeeded).map { i =>
            Cursor(' ', cursor.pos, cursor.line, col + i, cursor.isLiteral)
          }

          // Append the space cursors and continue
          LazyList.from(spaceCursors) #::: expand(remaining.tail, col + spacesNeeded)
        } else if (cursor.char == '\n') {
          // Reset column counting on newline
          cursor #:: expand(remaining.tail, 0)
        } else {
          // Pass through other characters
          cursor #:: expand(remaining.tail, col + 1)
        }
      }
    }

    expand(stream, 0)
  }
}
