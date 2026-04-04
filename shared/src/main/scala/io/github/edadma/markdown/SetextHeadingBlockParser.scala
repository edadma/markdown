package io.github.edadma.markdown

import scala.collection.mutable

object SetextHeadingBlockParser extends BlockParser {
  val name: String = "setext headings"

  // Regex patterns for setext underlines
  private val SetextLevel1Pattern = """^ {0,3}=+[ \t]*$""".r // Level 1 with =
  private val SetextLevel2Pattern = """^ {0,3}-+[ \t]*$""".r // Level 2 with -

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (lines.size < 2) return false

    val firstLine = lines.head

    // First line can't be blank
    if (isBlankLine(firstLine)) return false

    // First line must not be indented 4+ spaces (that's an indented code block)
    val firstLineText = lineToString(firstLine)
    if (firstLineText.takeWhile(_ == ' ').length >= 4) return false

    // First line must not start a block quote (non-escaped >) — those take precedence
    val firstNonSpace = firstLine.dropWhile(c => c.char == ' ').headOption
    if (firstNonSpace.exists(c => c.char == '>' && !c.isLiteral)) return false

    // Look ahead for an underline, skipping non-blank content lines
    var i = 1
    while (i < lines.size) {
      // If we find an underline, this is a setext heading
      if (isSetextUnderline(lines(i))) return true

      // If we find a blank line, this can't be a setext heading
      if (isBlankLine(lines(i))) return false

      // If the line would start another block construct, stop looking
      val lineText = lineToString(lines(i))
      if (lineText.trim.startsWith(">") || lineText.takeWhile(_ == ' ').length >= 4) return false

      i += 1
    }

    false
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Collect content lines until we find the underline
    var contentLines = List.empty[List[C]]
    var i            = 0

    while (i < lines.size && !isSetextUnderline(lines(i))) {
      contentLines = contentLines :+ lines(i)
      i += 1
    }

    // The underline is at position i
    val underlineText = lineToString(lines(i)).trim
    val level         = if (underlineText.head == '=' || underlineText.dropWhile(_ == ' ').head == '=') 1 else 2

    // Build content from all content lines, trimming leading spaces (up to 3) and trailing spaces
    val content = contentLines.flatMap { line =>
      val chars = line.takeWhile(_.char != '\n')
      // Trim up to 3 leading spaces
      val leadingSpaces = chars.takeWhile(_.char == ' ').size
      val trimmedStart  = chars.drop(Math.min(leadingSpaces, 3))
      // Trim trailing whitespace (spaces and tabs)
      val trimmed = trimmedStart.reverse.dropWhile(c => c.char == ' ' || c.char == '\t').reverse
      // Add soft line break between lines
      if (contentLines.head eq line) trimmed
      else List(C('\n', 0, 0, 0, false)) ++ trimmed
    }

    // Check for trailing attributes on the last content line
    if (config.attributes) {
      val contentStr = content.map(_.char).mkString
      val (stripped, attrs) = extractTrailingAttributes(contentStr)
      if (attrs.isDefined) {
        val newContent = content.take(stripped.length)
        return (Heading(level, newContent, attrs), i + 1)
      }
    }

    (Heading(level, content), i + 1) // Consume content lines + underline
  }

  private def isSetextUnderline(line: List[C]): Boolean = {
    val text = lineToString(line)
    // Check pattern matches and that no underline chars are escaped (isLiteral)
    val matches = SetextLevel1Pattern.matches(text) || SetextLevel2Pattern.matches(text)
    if (!matches) return false
    // Verify no escaped characters in the underline portion
    !line.takeWhile(_.char != '\n').exists(c => (c.char == '=' || c.char == '-') && c.isLiteral)
  }

  private def isBlankLine(line: List[C]): Boolean =
    line.forall(c => c.char == ' ' || c.char == '\t' || c.char == '\n')

  private def lineToString(line: List[C]): String =
    line.takeWhile(_.char != '\n').map(_.char).mkString
}
