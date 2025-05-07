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

  /** Parse a callout block from the given lines. Uses BlockQuoteParser to handle most of the parsing, then extracts
    * callout-specific information.
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

    // Let BlockQuoteParser do the heavy lifting
    val (blockQuote, linesConsumed) = BlockQuoteParser.parse(lines, linkRefs, parentIndent, config)

    // Modify the result into a callout block
    blockQuote match {
      case BlockQuote(children) =>
        // Special case: if there's only one child and it's a paragraph, try to reprocess it
        // to preserve inline formatting while removing the callout marker
        val modifiedChildren = if (children.nonEmpty) {
          children.head match {
            case Paragraph(firstInlines) =>
              // Extract the callout marker to determine where it ends
              val markerPattern     = s"\\[!${rawCalloutType}\\].*?".r
              val contentWithMarker = inlinesToText(firstInlines)

              markerPattern.findFirstMatchIn(contentWithMarker) match {
                case Some(m) if m.start == 0 =>
                  // The marker is at the beginning of the paragraph
                  val markerEnd = m.end

                  if (markerEnd >= contentWithMarker.length) {
                    // The marker is the entire paragraph - remove it
                    children.tail
                  } else {
                    // There's content after the marker - parse it as new inlines
                    val contentAfterMarker = contentWithMarker.substring(markerEnd).trim

                    // If the content is empty, remove the paragraph
                    if (contentAfterMarker.isEmpty) {
                      children.tail
                    } else {
                      // Use original inlines but skip the marker part
                      var charsToSkip      = markerEnd
                      val remainingInlines = new scala.collection.mutable.ListBuffer[Inline]

                      for (inline <- firstInlines) {
                        inline match {
                          case Text(t) if charsToSkip > 0 =>
                            if (t.length <= charsToSkip) {
                              // Skip this text node entirely
                              charsToSkip -= t.length
                            } else {
                              // Keep part of this text node
                              remainingInlines += Text(t.substring(charsToSkip))
                              charsToSkip = 0
                            }
                          case SoftLineBreak() if charsToSkip > 0 =>
                            // Skip soft line breaks within the marker section
                            charsToSkip -= 1
                          case other if charsToSkip > 0 =>
                            // Approximate size of other nodes as 1 character
                            charsToSkip -= 1
                          case other =>
                            // Keep all other nodes once we're past the marker
                            remainingInlines += other
                        }
                      }

                      // Remove any leading SoftLineBreak nodes
                      val cleanedInlines = remainingInlines.toList.dropWhile(_.isInstanceOf[SoftLineBreak])

                      if (cleanedInlines.isEmpty) {
                        children.tail
                      } else {
                        Paragraph(cleanedInlines) :: children.tail
                      }
                    }
                  }

                case _ =>
                  // Marker not found or not at start (shouldn't happen)
                  children
              }

            case _ =>
              // Not a paragraph, keep as is
              children
          }
        } else {
          // No children
          children
        }

        // Create the CalloutBlock
        (CalloutBlock(calloutType, calloutTitle, modifiedChildren), linesConsumed)

      case _ =>
        // Not a BlockQuote (shouldn't happen)
        (blockQuote, linesConsumed)
    }
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

  /** Convert inlines to a text representation for comparison */
  private def inlinesToText(inlines: List[Inline]): String = {
    inlines.map {
      case Text(t) => t
      case c: C    => c.char.toString
      case _       => ""
    }.mkString
  }
}
