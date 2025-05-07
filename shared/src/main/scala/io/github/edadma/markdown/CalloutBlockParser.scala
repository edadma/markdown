package io.github.edadma.markdown

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Parser for callout blocks in Markdown.
  *
  * Implements the syntax: > [!TYPE] or > [!TYPE]: Title where TYPE is the callout type (e.g., note, warning, info)
  */
object CalloutBlockParser extends BlockParser {
  val name: String = "callout blocks"

  // Regular expression to detect callout syntax
  private val CalloutPattern = """^\s*\[!([\w-]+)\](?:\s*\:(.*?))?$""".r

  // List of supported callout types (can be extended)
  private val SupportedTypes = Set("note", "warning", "info", "tip", "danger", "important")

  /** Check if the lines can start a callout block. Requires that the first line is a blockquote that contains the
    * callout marker syntax.
    */
  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    // Only consider this parser if callouts are enabled in the config
    if (!config.callouts) return false

    // Check if it could be a block quote first (reusing existing logic)
    if (!BlockQuoteParser.canStart(lines, config)) return false

    // Now check if the first line contains callout syntax
    val firstLine         = lines.head
    val blockQuoteContent = extractBlockQuoteContent(firstLine)

    hasCalloutSyntax(blockQuoteContent)
  }

  /** Parse a callout block from the given lines. Uses BlockQuoteParser to handle most of the parsing, then extracts
    * callout-specific information.
    */
  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {

    // First, use BlockQuoteParser to parse the block as a regular blockquote
    val (blockQuote, linesConsumed) = BlockQuoteParser.parse(lines, linkRefs, parentIndent, config)

    // Then extract callout information from the first line
    blockQuote match {
      case BlockQuote(children) if children.nonEmpty =>
        extractCalloutInfo(children) match {
          case Some((calloutType, title, remainingBlocks)) =>
            // If found, create a CalloutBlock
            (CalloutBlock(calloutType, title, remainingBlocks), linesConsumed)
          case None =>
            // If not found (shouldn't happen if canStart is correct), return original blockquote
            (blockQuote, linesConsumed)
        }
      case _ =>
        // No children or not a BlockQuote (shouldn't happen), return original
        (blockQuote, linesConsumed)
    }
  }

  /** Extract content after the '>' marker from a blockquote line.
    */
  private def extractBlockQuoteContent(line: List[C]): String = {
    val content = line.dropWhile(c => c.char != '>').drop(1)
    content.takeWhile(_.char != '\n').map(_.char).mkString.trim
  }

  /** Check if a string has callout syntax ([!TYPE]).
    */
  private def hasCalloutSyntax(content: String): Boolean = {
    CalloutPattern.findFirstMatchIn(content).isDefined
  }

  /** Extract callout type, optional title, and remaining blocks from blockquote children. Returns None if the first
    * block isn't a paragraph or doesn't contain callout syntax.
    */
  private def extractCalloutInfo(blocks: List[Block]): Option[(String, Option[String], List[Block])] = {
    blocks.headOption match {
      case Some(Paragraph(inlines)) =>
        // Convert inlines to text for pattern matching
        val content = inlinesAsText(inlines)

        CalloutPattern.findFirstMatchIn(content) match {
          case Some(m) =>
            val calloutType = m.group(1).toLowerCase
            // Normalize type to one of supported types, or default to "note"
            val normalizedType = if (SupportedTypes.contains(calloutType)) calloutType else "note"
            val title          = Option(m.group(2)).map(_.trim).filter(_.nonEmpty)

            // Remove the callout marker paragraph if it contained only the marker
            val remainingBlocks = if (content.trim == s"[!${m.group(1)}]${Option(m.group(2)).getOrElse("")}") {
              blocks.tail
            } else {
              // If there was additional content, keep the paragraph but remove the callout marker
              val modifiedInlines = removeCalloutMarker(inlines, content, m.group(0))
              Paragraph(modifiedInlines) :: blocks.tail
            }

            Some((normalizedType, title, remainingBlocks))

          case None => None
        }

      case _ => None
    }
  }

  /** Convert a list of inlines to a single text string.
    */
  private def inlinesAsText(inlines: List[Inline]): String = {
    inlines.map {
      case Text(content) => content
      case c: C          => c.char.toString
      case _             => ""
    }.mkString
  }

  /** Remove the callout marker from the inlines, replacing it with empty text.
    */
  private def removeCalloutMarker(inlines: List[Inline], fullText: String, marker: String): List[Inline] = {
    // Complex case: marker might span multiple inlines
    // Simplified approach: rebuild text without the marker
    val newText = fullText.replace(marker, "").trim
    if (newText.isEmpty) {
      List.empty
    } else {
      List(Text(newText))
    }
  }
}
