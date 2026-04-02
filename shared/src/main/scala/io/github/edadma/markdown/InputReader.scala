package io.github.edadma.markdown

import scala.collection.immutable.LazyList

class InputReader(input: String) {
  // Step 1: Normalize the input
  private val normalizedInput = normalizeInput(input)

  // Main public stream of cursors
  lazy val stream: LazyList[C] = processToCursors(normalizedInput)

  // Normalize input - handle null characters and line endings
  private def normalizeInput(input: String): String = {
    input.replace("\u0000", "\uFFFD") // Replace null with replacement character
      .replace("\r\n", "\n")          // Normalize CRLF
      .replace("\r", "\n")            // Normalize CR
  }

  // Process input into a stream of cursors
  private def processToCursors(input: String): LazyList[C] = {
    def process(index: Int, pos: Int, line: Int, col: Int): LazyList[C] = {
      if (index >= input.length) {
        LazyList(EndOfInput) // End of input marker
      } else {
        val current = input.charAt(index)

        // Handle escape sequences
        if (current == '\\' && index + 1 < input.length) {
          val next = input.charAt(index + 1)
          if (isAsciiPunctuation(next)) {
            // Create cursor for the escaped character
            val cursor = C(next, pos + 1, line, col + 1, true)
            cursor #:: process(index + 2, pos + 2, line, col + 2)
          } else {
            // Not escaped, just a regular backslash
            val cursor = C(current, pos, line, col, false)
            cursor #:: process(index + 1, pos + 1, line, col + 1)
          }
        }
        // Handle entity references
//        else if (current == '&') {
//          val entityResult = parseEntityReference(input, index)
//          if (entityResult.isDefined) {
//            val (entityChar, entityLength) = entityResult.get
//            val cursor                     = C(entityChar, pos, line, col, false)
//            cursor #:: process(index + entityLength, pos + entityLength, line, col + 1)
//          } else {
//            // Not a valid entity, treat as regular character
//            val cursor = C(current, pos, line, col, false)
//            cursor #:: process(index + 1, pos + 1, line, col + 1)
//          }
//        }
        // Handle line endings
        else if (current == '\n') {
          val cursor = C(current, pos, line, col, false)
          cursor #:: process(index + 1, pos + 1, line + 1, 0) // Reset column, increment line
        }
        // Handle tabs (in base stream they're passed through)
        else if (current == '\t') {
          val cursor = C(current, pos, line, col, false)
          // Tab advances to next tab stop (multiples of 4)
          val nextCol = col + (4 - (col % 4))
          cursor #:: process(index + 1, pos + 1, line, nextCol)
        }
        // Regular character
        else {
          val cursor = C(current, pos, line, col, false)
          cursor #:: process(index + 1, pos + 1, line, col + 1)
        }
      }
    }

    process(0, 0, 0, 0)
  }

  // Check if a character is ASCII punctuation
  private def isAsciiPunctuation(c: Char): Boolean = {
    (c >= '!' && c <= '/') ||
    (c >= ':' && c <= '@') ||
    (c >= '[' && c <= '`') ||
    (c >= '{' && c <= '~')
  }

  // Get a stream with tabs expanded to spaces in block structure contexts
  def getStreamWithExpandedTabs(): LazyList[C] = {
    expandTabsInStream(stream)
  }

  // Expand tabs to spaces in a stream of cursors
  private def expandTabsInStream(stream: LazyList[C]): LazyList[C] = {
    def expand(remaining: LazyList[C], col: Int): LazyList[C] = {
      if (remaining.isEmpty) {
        LazyList.empty
      } else {
        val cursor = remaining.head

        if (cursor.char == '\t') {
          // Calculate spaces needed for this tab
          val spacesNeeded = 4 - (col % 4)

          // Create cursors for the spaces
          val spaceCursors = (0 until spacesNeeded).map { i =>
            C(' ', cursor.pos, cursor.line, col + i, cursor.isLiteral)
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

/** Extract raw text from a list of C cursors, restoring backslashes for literal (escaped) characters. Use this for
  * contexts where backslash escapes should not be processed (code blocks, code spans, HTML blocks).
  */
def rawText(cursors: List[C]): String =
  cursors
    .takeWhile(_.char != '\n')
    .flatMap(c => if (c.isLiteral) List('\\', c.char) else List(c.char))
    .mkString

/** Expand tabs to spaces in the leading whitespace of a line of C cursors.
  * Only affects whitespace before the first non-whitespace character.
  * Tabs inside content are preserved.
  * @param startCol the virtual column at the start of the line (default 0)
  */
def expandLeadingTabs(line: List[C], startCol: Int = 0): List[C] = {
  val result = scala.collection.mutable.ListBuffer[C]()
  var col    = startCol
  var rest   = line
  // Expand tabs/spaces in the leading whitespace
  while (rest.nonEmpty && (rest.head.char == ' ' || rest.head.char == '\t')) {
    val c = rest.head
    if (c.char == '\t') {
      val spaces = 4 - (col % 4)
      for (i <- 0 until spaces) result += C(' ', c.pos, c.line, col + i, false)
      col += spaces
    } else {
      result += c
      col += 1
    }
    rest = rest.tail
  }
  // Append the rest of the line unchanged
  result.toList ++ rest
}

/** Count virtual column width of leading whitespace, treating tabs as expanding to next tab stop (multiples of 4). */
def virtualIndent(line: List[C]): Int = {
  import scala.util.boundary, boundary.break
  var col = 0
  boundary {
    for (c <- line) {
      if (c.char == ' ') col += 1
      else if (c.char == '\t') col += 4 - (col % 4)
      else break()
    }
  }
  col
}

/** Drop leading whitespace up to `n` virtual columns, expanding tabs as needed. Returns remaining chars.
  * @param startCol the virtual column at the start of the line (default 0)
  */
def dropIndent(line: List[C], n: Int, startCol: Int = 0): List[C] = {
  var col       = startCol
  val target    = startCol + n
  var remaining = line
  while (remaining.nonEmpty && col < target) {
    val c = remaining.head
    if (c.char == ' ') {
      col += 1
      remaining = remaining.tail
    } else if (c.char == '\t') {
      val tabWidth = 4 - (col % 4)
      if (col + tabWidth <= target) {
        col += tabWidth
        remaining = remaining.tail
      } else {
        // Partially consume the tab — replace with remaining spaces
        val spacesNeeded = target - col
        col = target
        remaining = remaining.tail
        // Prepend the leftover spaces from the partial tab
        val leftover = tabWidth - spacesNeeded
        remaining = (0 until leftover).map(i => C(' ', c.pos, c.line, c.column + spacesNeeded + i, false)).toList ++ remaining
      }
    } else {
      return remaining // Non-whitespace, stop
    }
  }
  remaining
}
