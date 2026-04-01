package io.github.edadma.markdown

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class LinkReference(destination: String, title: Option[String])

object LinkReferenceDefinitionParser extends BlockParser {
  val name: String = "link refs"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    val line = lines.head
    val spaces = line.takeWhile(c => c.char == ' ').length
    if (spaces >= 4) return false

    val stripped = line.drop(spaces)
    stripped.nonEmpty && stripped.head.char == '[' && !stripped.head.isLiteral
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Build flat text from consecutive non-blank lines, tracking line boundaries
    val textLines = new ArrayBuffer[String]()
    var linesIter = lines
    var stop = false

    while (!stop && linesIter.nonEmpty) {
      val line = linesIter.head
      if (textLines.nonEmpty && isBlankLine(line)) {
        stop = true
      } else {
        textLines += lineToString(line)
        linesIter = linesIter.tail
        // Limit how many lines we look ahead
        if (textLines.length > 20) stop = true
      }
    }

    tryParse(textLines.toArray) match {
      case Some((label, dest, title, linesUsed)) =>
        val normalizedLabel = normalizeLabel(processEscapes(label))
        if (!linkRefs.contains(normalizedLabel)) {
          linkRefs.put(normalizedLabel, LinkReference(dest, title))
        }
        (null, linesUsed)
      case None =>
        (null, 0) // Let another parser handle it
    }
  }

  private def lineToString(line: List[C]): String =
    rawText(line)

  private def isBlankLine(line: List[C]): Boolean = {
    val contentChars = line.filter(_.char != '\n')
    contentChars.isEmpty || contentChars.forall(c => c.char == ' ' || c.char == '\t')
  }

  /** Try to parse a link reference definition from the given lines. Returns (label, destination, title, linesConsumed)
    * on success.
    */
  private def tryParse(textLines: Array[String]): Option[(String, String, Option[String], Int)] = {
    // Join all lines into a single string with \n separators, tracking line boundaries
    val lineStarts = new Array[Int](textLines.length)
    val buf = new StringBuilder
    for (i <- textLines.indices) {
      lineStarts(i) = buf.length
      buf ++= textLines(i)
      buf += '\n'
    }
    val text = buf.toString
    var pos = 0

    def lineOfPos(p: Int): Int = {
      var line = 0
      while (line + 1 < lineStarts.length && lineStarts(line + 1) <= p) line += 1
      line
    }

    // Skip 0-3 spaces
    val startSpaces = skipChars(text, pos, ' ')
    if (startSpaces - pos >= 4) return None
    pos = startSpaces

    // Must start with [
    if (pos >= text.length || text(pos) != '[') return None
    pos += 1

    // Parse label: find unescaped ] — no unescaped [ allowed inside
    val labelStart = pos
    var foundClose = false
    while (pos < text.length && !foundClose) {
      text(pos) match {
        case '\\' =>
          pos += 2 // skip escaped char
        case ']' =>
          foundClose = true
        case '[' =>
          return None // unescaped [ inside label
        case _ =>
          pos += 1
      }
    }
    if (!foundClose) return None

    val label = text.substring(labelStart, pos)
    // Label must have at least one non-whitespace char and at most 999 chars
    if (label.length > 999 || label.forall(c => c == ' ' || c == '\t' || c == '\n')) return None
    pos += 1 // skip ]

    // Must be followed by :
    if (pos >= text.length || text(pos) != ':') return None
    pos += 1

    // Optional spaces/tabs, then optional one line ending, then optional spaces/tabs
    pos = skipSpacesAndTabs(text, pos)
    if (pos < text.length && text(pos) == '\n') pos += 1
    pos = skipSpacesAndTabs(text, pos)

    // Parse link destination
    val destResult = parseLinkDestination(text, pos)
    if (destResult.isEmpty) return None
    val (dest, destEnd) = destResult.get
    pos = destEnd

    // After destination: check what follows
    val posAfterDest = pos
    val lineAfterDest = lineOfPos(pos)

    // Skip spaces/tabs on the current line after destination
    val posAfterDestSpaces = skipSpacesAndTabs(text, pos)

    // Check if rest of line (after dest) is blank
    val restOfLineBlank = posAfterDestSpaces >= text.length || text(posAfterDestSpaces) == '\n'

    if (restOfLineBlank) {
      // No title on this line — try next line for title
      pos = posAfterDestSpaces
      if (pos < text.length && text(pos) == '\n') pos += 1

      // Check if next line could start a title
      val titleLineStart = pos
      val titleLineSpaces = skipSpacesAndTabs(text, pos)
      if (titleLineSpaces < text.length && isTitleStart(text(titleLineSpaces))) {
        // Try to parse title on next line
        val titleResult = parseLinkTitle(text, titleLineSpaces)
        titleResult match {
          case Some((title, titleEnd)) =>
            // After title, only whitespace allowed to end of line
            val afterTitle = skipSpacesAndTabs(text, titleEnd)
            if (afterTitle >= text.length || text(afterTitle) == '\n') {
              val consumed = lineOfPos(titleEnd) + 1
              return Some((label, dest, Some(title), consumed))
            }
            // Invalid content after title — definition has no title
            // Fall through to return without title
          case None =>
          // Not a valid title — definition has no title
        }
      }
      // No title — definition ends at end of destination line
      val consumed = lineAfterDest + 1
      return Some((label, dest, None, consumed))
    }

    // Rest of line is not blank — check for title on same line
    if (posAfterDestSpaces > posAfterDest && isTitleStart(text(posAfterDestSpaces))) {
      // There are spaces between dest and potential title
      val titleResult = parseLinkTitle(text, posAfterDestSpaces)
      titleResult match {
        case Some((title, titleEnd)) =>
          // After title, only whitespace allowed to end of line
          val afterTitle = skipSpacesAndTabs(text, titleEnd)
          if (afterTitle >= text.length || text(afterTitle) == '\n') {
            val consumed = lineOfPos(titleEnd) + 1
            return Some((label, dest, Some(title), consumed))
          }
          // Content after title — invalid definition
          return None
        case None =>
          // Title started but didn't complete — invalid
          return None
      }
    }

    // Non-whitespace after destination but not a title, or no space before title
    // This is invalid (e.g., `[foo]: <bar>(baz)` or `[foo]: /url "title" ok`)
    None
  }

  private def isTitleStart(c: Char): Boolean = c == '"' || c == '\'' || c == '('

  /** Parse a link destination starting at pos. Returns (destination, endPos) or None. */
  private def parseLinkDestination(text: String, pos: Int): Option[(String, Int)] = {
    if (pos >= text.length) return None

    if (text(pos) == '<') {
      // Angle-bracketed destination: <...> with no newlines, no unescaped < or >
      var i = pos + 1
      while (i < text.length) {
        text(i) match {
          case '\n' => return None
          case '\\' if i + 1 < text.length =>
            i += 2
          case '<' => return None
          case '>' =>
            val dest = text.substring(pos + 1, i)
            return Some((processEscapes(dest), i + 1))
          case _ =>
            i += 1
        }
      }
      None // No closing >
    } else {
      // Bare destination: no spaces, no control chars, balanced parens
      var i = pos
      var parenDepth = 0
      while (i < text.length) {
        val c = text(i)
        if (c == '\\' && i + 1 < text.length) {
          i += 2
        } else if (c == '(') {
          parenDepth += 1
          i += 1
        } else if (c == ')') {
          if (parenDepth == 0) {
            // End of destination (unbalanced close paren)
            val s = i
            if (s == pos) return None // empty bare destination is invalid
            return Some((processEscapes(text.substring(pos, s)), s))
          }
          parenDepth -= 1
          i += 1
        } else if (c <= ' ' || c == 0x7f.toChar) {
          // Space or control character ends the destination
          if (i == pos) return None // empty bare destination
          return Some((processEscapes(text.substring(pos, i)), i))
        } else {
          i += 1
        }
      }
      if (i == pos) None
      else Some((processEscapes(text.substring(pos, i)), i))
    }
  }

  /** Parse a link title starting at pos. Returns (title, endPos) or None. Title may span multiple lines but not blank
    * lines.
    */
  private def parseLinkTitle(text: String, pos: Int): Option[(String, Int)] = {
    if (pos >= text.length) return None

    val openChar = text(pos)
    val closeChar = openChar match {
      case '"'  => '"'
      case '\'' => '\''
      case '('  => ')'
      case _    => return None
    }

    var i = pos + 1
    var prevNewline = false
    while (i < text.length) {
      val c = text(i)
      if (c == '\n') {
        if (prevNewline) return None // blank line inside title
        prevNewline = true
        i += 1
        // Check if next line is blank (all spaces/tabs then \n or end)
        val nextLineEnd = text.indexOf('\n', i)
        val nextLine = if (nextLineEnd >= 0) text.substring(i, nextLineEnd) else text.substring(i)
        if (nextLine.forall(c => c == ' ' || c == '\t')) return None // blank line
      } else {
        prevNewline = false
        if (c == '\\' && i + 1 < text.length) {
          i += 2
        } else if (c == closeChar) {
          val title = text.substring(pos + 1, i)
          return Some((processEscapes(title), i + 1))
        } else {
          i += 1
        }
      }
    }
    None // No closing delimiter
  }

  private def processEscapes(s: String): String = {
    val buf = new StringBuilder
    var i = 0
    while (i < s.length) {
      if (s(i) == '\\' && i + 1 < s.length && isEscapable(s(i + 1))) {
        buf += s(i + 1)
        i += 2
      } else {
        buf += s(i)
        i += 1
      }
    }
    buf.toString
  }

  private def isEscapable(c: Char): Boolean =
    "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".contains(c)

  private def skipSpacesAndTabs(text: String, pos: Int): Int = {
    var i = pos
    while (i < text.length && (text(i) == ' ' || text(i) == '\t')) i += 1
    i
  }

  private def skipChars(text: String, pos: Int, c: Char): Int = {
    var i = pos
    while (i < text.length && text(i) == c) i += 1
    i
  }

  private def normalizeLabel(label: String): String = {
    // Unicode case fold, strip leading/trailing whitespace, collapse internal whitespace
    label.trim.toLowerCase.replaceAll("\\s+", " ")
  }
}
