package io.github.edadma.markdown

import scala.collection.mutable

/** Parser for callout blocks in Markdown.
  *
  * Implements the syntax: > [!TYPE] or > [!TYPE]: Title where TYPE is the callout type (e.g., note, warning, info)
  */
object CalloutBlockParser extends BlockParser {
  val name: String = "callout blocks"

  // Regular expression to detect callout syntax
  private val CalloutPattern = """^\s*\[!([\w-]+)\](?:\s*\:(.*?))?$""".r

  // List of supported callout types
  private val SupportedTypes = Set("note", "warning", "info", "tip", "danger", "important")

  /** Check if the lines can start a callout block. Requires that the first line is a blockquote that contains the
    * callout marker syntax.
    */
  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    // Only consider this parser if callouts are enabled in the config
    if (!config.callouts) return false

    // Check if it could be a block quote first
    if (!BlockQuoteParser.canStart(lines, config)) return false

    // Now check if the first line contains callout syntax
    val firstLine         = lines.head
    val blockQuoteContent = extractBlockQuoteContent(firstLine)

    hasCalloutSyntax(blockQuoteContent)
  }

  /** Parse a callout block from the given lines.
    */
  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Extract callout type and title from first line
    val firstLine        = lines.head
    val firstLineContent = extractBlockQuoteContent(firstLine)

    // Get callout marker info
    val calloutMatcher = CalloutPattern.findFirstMatchIn(firstLineContent).get
    val rawCalloutType = calloutMatcher.group(1).toLowerCase

    // Normalize callout type
    val calloutType  = if (SupportedTypes.contains(rawCalloutType)) rawCalloutType else "note"
    val calloutTitle = Option(calloutMatcher.group(2)).map(_.trim).filter(_.nonEmpty)

    // Skip the first line (which has the callout marker)
    // and use BlockQuoteParser's helper methods to process the remaining lines
    val contentLines = lines.tail

    // Use the BlockQuoteParser methods for collecting and processing blockquote lines
    val (blockQuoteLines, contentLinesConsumed) = BlockQuoteParser.collectBlockQuoteLines(contentLines)
    val processedLines                          = BlockQuoteParser.processBlockQuoteContent(blockQuoteLines)

    // Parse the processed lines into blocks
    val blocks = processLines(processedLines, linkRefs, parentIndent, config)

    // Return the CalloutBlock with the total lines consumed (first line + content lines)
    (CalloutBlock(calloutType, calloutTitle, blocks), 1 + contentLinesConsumed)
  }

  /** Extract content after the '>' marker from a blockquote line. */
  private def extractBlockQuoteContent(line: List[C]): String = {
    val content = line.dropWhile(c => c.char != '>').drop(1)
    content.takeWhile(_.char != '\n').map(_.char).mkString.trim
  }

  /** Check if a string has callout syntax ([!TYPE]). */
  private def hasCalloutSyntax(content: String): Boolean = {
    CalloutPattern.findFirstMatchIn(content).isDefined
  }
}
