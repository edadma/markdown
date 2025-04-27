package io.github.edadma.markdown

import scala.collection.mutable

object SetextHeadingBlockParser extends BlockParser {
  val name: String = "setext headings"

  // Regex patterns for setext underlines
  private val SetextLevel1Pattern = """^ {0,3}=+[ \t]*$""".r // Level 1 with =
  private val SetextLevel2Pattern = """^ {0,3}-+[ \t]*$""".r // Level 2 with -

  def canStart(lines: List[LazyList[C]]): Boolean = {
    if (lines.size < 2) return false

    // Need at least two lines - content and underline
    val firstLine  = lines.head
    val secondLine = lines(1)

    // First line can't be blank
    if (isBlankLine(firstLine)) return false

    // Check if second line is a setext heading underline
    val secondLineText = lineToString(secondLine)
    SetextLevel1Pattern.matches(secondLineText) || SetextLevel2Pattern.matches(secondLineText)
  }

  def parse(lines: List[LazyList[C]], linkRefs: mutable.Map[String, LinkReference]): (Block, Int) = {
    // Get the content from the first line
    val contentLine = lines.head

    // Get the underline from the second line
    val underlineText = lineToString(lines(1))

    // Determine level (1 for = and 2 for -)
    val level = if (underlineText.charAt(0) == '=') 1 else 2

    // Extract content (excluding newline)
    val content = contentLine.takeWhile(c => c.char != '\n').toList

    (Heading(level, content), 2) // Consume two lines (content + underline)
  }

  // Helper function to check for blank lines
  private def isBlankLine(line: LazyList[C]): Boolean = {
    line.forall(c => c.char == ' ' || c.char == '\t' || c.char == '\n')
  }

  // Helper function to convert line to string
  private def lineToString(line: LazyList[C]): String = {
    line.takeWhile(_.char != '\n').map(_.char).mkString
  }
}
