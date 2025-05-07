package io.github.edadma.markdown

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Parser for callout blocks in Markdown that processes lines directly. */
object CalloutBlockParser extends BlockParser {
  val name: String = "callout blocks"

  // Regular expression to detect callout syntax
  private val CalloutPattern = """^\s*\[!([\w-]+)\](?:\s*\:(.*?))?$""".r

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    logger.debug("===== CalloutBlockParser.canStart =====")

    // Only consider this parser if callouts are enabled in the config
    logger.debug(s"config.callouts = ${config.callouts}")
    if (!config.callouts) return false

    // Check if it could be a block quote first
    val isBlockQuote = BlockQuoteParser.canStart(lines, config)
    logger.debug(s"Is blockquote: $isBlockQuote")
    if (!isBlockQuote) return false

    // Extract content after '>'
    val firstLine = lines.head
    val blockQuoteContent = firstLine.dropWhile(c => c.char != '>').drop(1)
      .takeWhile(_.char != '\n').map(_.char).mkString.trim
    logger.debug(s"Content after >: '$blockQuoteContent'")

    // Check for callout syntax
    val hasCallout = CalloutPattern.findFirstMatchIn(blockQuoteContent).isDefined
    logger.debug(s"Has callout syntax: $hasCallout")

    // Log match details if found
    if (hasCallout) {
      val m = CalloutPattern.findFirstMatchIn(blockQuoteContent).get
      logger.debug(s"Matched groups: type='${m.group(1)}', title='${Option(m.group(2)).getOrElse("")}'")
    }

    hasCallout
  }

  /** Parse a callout block from the given lines. This implementation processes the blockquote lines individually to
    * preserve content.
    */
  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    logger.debug("===== CalloutBlockParser.parse =====")

    // Extract type and title from first line
    val firstLine = lines.head
    val firstLineContent = firstLine.dropWhile(c => c.char != '>').drop(1)
      .takeWhile(_.char != '\n').map(_.char).mkString.trim
    logger.debug(s"First line content: '$firstLineContent'")

    // Find callout marker
    val calloutMatcher = CalloutPattern.findFirstMatchIn(firstLineContent).get
    val calloutType    = calloutMatcher.group(1).toLowerCase
    val calloutTitle   = Option(calloutMatcher.group(2)).map(_.trim).filter(_.nonEmpty)
    logger.debug(s"Extracted: type='$calloutType', title=$calloutTitle")

    // Use BlockQuoteParser to get all the lines
    val (blockQuote, linesConsumed) = BlockQuoteParser.parse(lines, linkRefs, parentIndent, config)
    logger.debug(s"BlockQuote consumed $linesConsumed lines")

    // Parse the inner content without the callout marker
    blockQuote match {
      case BlockQuote(children) =>
        // Process the first paragraph to remove the callout marker if needed
        val modifiedChildren = if (children.nonEmpty && children.head.isInstanceOf[Paragraph]) {
          val firstPara = children.head.asInstanceOf[Paragraph]
          val paraText = firstPara.inlines.map {
            case Text(t) => t
            case c: C    => c.char.toString
            case _       => ""
          }.mkString.trim

          logger.debug(s"First paragraph text: '$paraText'")

          // Check if paragraph starts with the marker and has additional content
          if (paraText == firstLineContent) {
            // Paragraph contains only the marker - remove it
            logger.debug("First paragraph contains only marker - removing it")
            children.tail
          } else if (paraText.startsWith(firstLineContent)) {
            // Paragraph contains marker plus content - extract only the content
            val contentAfterMarker = paraText.substring(firstLineContent.length).trim
            logger.debug(s"Content after marker: '$contentAfterMarker'")

            if (contentAfterMarker.isEmpty) {
              children.tail
            } else {
              Paragraph(List(Text(contentAfterMarker))) :: children.tail
            }
          } else {
            // Keep original structure (shouldn't happen)
            logger.debug("Unexpected: First paragraph doesn't match marker line")
            children
          }
        } else {
          children
        }

        logger.debug(s"Final blocks count: ${modifiedChildren.size}")

        // Create the CalloutBlock
        val calloutBlock = CalloutBlock(calloutType, calloutTitle, modifiedChildren)
        (calloutBlock, linesConsumed)

      case _ =>
        // Not a BlockQuote (shouldn't happen)
        (blockQuote, linesConsumed)
    }
  }
}
