package io.github.edadma.markdown

import scala.collection.mutable

object DefinitionListParser extends BlockParser {
  val name: String = "definition lists"

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    // Only consider this parser if definition lists are enabled
    if (!config.enableDefinitionLists) return false

    // Need at least two lines to start a definition list
    if (lines.isEmpty || lines.tail.isEmpty) return false

    // First line should not be blank and not start with a colon
    val firstLine = lines.head.takeWhile(_.char != '\n').map(_.char).mkString.trim
    if (firstLine.isEmpty || firstLine.startsWith(":")) return false

    // Second line should start with a colon (definition)
    val secondLine = lines.tail.head.takeWhile(_.char != '\n').map(_.char).mkString.trim
    secondLine.startsWith(":")
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    // Parse definition list items recursively
    parseItems(lines, Nil, 0) match {
      case (items, consumed) if items.nonEmpty =>
        (DefinitionList(items), consumed)
      case _ =>
        // No valid definition list found
        (null, 0)
    }
  }

  @scala.annotation.tailrec
  private def parseItems(
      lines: LazyList[List[C]],
      items: List[(List[Inline], List[Block])],
      consumed: Int,
  ): (List[(List[Inline], List[Block])], Int) = {
    if (lines.isEmpty) {
      return (items, consumed)
    }

    // Try to parse a term
    val termText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString.trim
    if (termText.isEmpty || termText.startsWith(":")) {
      // Not a term - end of definition list
      return (items, consumed)
    }

    // This is a term
    val term = lines.head.takeWhile(_.char != '\n').toList

    // Look for definitions
    val (defs, defLines) = parseDefinitions(lines.tail)

    if (defs.isEmpty) {
      // No definitions found - not a valid definition list item
      if (items.isEmpty) {
        // No items parsed yet - not a definition list
        return (Nil, 0)
      } else {
        // End of definition list
        return (items, consumed)
      }
    }

    // Add this item and continue parsing
    parseItems(
      lines.drop(1 + defLines), // Drop term + definitions
      items :+ (term, defs),    // Add new item
      consumed + 1 + defLines,  // Update consumed lines count
    )
  }

  private def parseDefinitions(lines: LazyList[List[C]]): (List[Block], Int) = {
    @scala.annotation.tailrec
    def loop(
        currentLines: LazyList[List[C]],
        defs: List[Block],
        consumed: Int,
    ): (List[Block], Int) = {
      if (currentLines.isEmpty) {
        return (defs, consumed)
      }

      val line = currentLines.head
      val text = line.takeWhile(_.char != '\n').map(_.char).mkString.trim

      if (text.startsWith(":")) {
        // Definition line - extract content after the colon
        val content = text.substring(text.indexOf(':') + 1).trim
        val newDef  = Paragraph(List(Text(content)))

        // Continue to next line
        loop(currentLines.tail, defs :+ newDef, consumed + 1)
      } else if (text.isEmpty) {
        // Blank line - skip it
        loop(currentLines.tail, defs, consumed + 1)
      } else {
        // Not a definition line - must be a new term or end of list
        (defs, consumed)
      }
    }

    loop(lines, Nil, 0)
  }
}
