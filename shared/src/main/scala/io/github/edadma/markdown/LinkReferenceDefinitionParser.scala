package io.github.edadma.markdown

import scala.collection.mutable

case class LinkReference(destination: String, title: Option[String])

object LinkReferenceDefinitionParser extends BlockParser {
  val name: String = "link refs"

  def canStart(lines: List[LazyList[C]]): Boolean = {
    val content = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    content.trim.startsWith("[") && content.contains("]:")
  }

  def parse(
      lines: List[LazyList[C]],
      linkRefs: scala.collection.mutable.Map[String, LinkReference],
  ): (Block, Int) = {
    val line = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    parseDefinition(line) match {
      case Some((label, reference)) =>
        if (!linkRefs.contains(label)) {
          linkRefs.put(label, reference)
        }
        (null, 1)

      case None =>
        (null, 0)
    }
  }

  private def parseDefinition(line: String): Option[(String, LinkReference)] = {
    // Split into label and rest
    val labelAndRest = """^\s*\[(.*?)\]:\s*(.*)$""".r

    line match {
      case labelAndRest(label, rest) =>
        val normalizedLabel = normalizeLabel(label)

        // Extract destination and remaining title part
        val (destination, titlePart) = extractDestination(rest)

        // Extract title without delimiters
        val title = extractTitle(titlePart)

        Some(normalizedLabel -> LinkReference(destination, title))

      case _ => None
    }
  }

  private def extractDestination(text: String): (String, String) = {
    if (text.startsWith("<")) {
      // Angle-bracketed URL
      val closingBracket = text.indexOf('>')
      if (closingBracket >= 0) {
        val dest = text.substring(1, closingBracket)
        val rest = text.substring(closingBracket + 1).trim
        return (dest, rest)
      }
    }

    // Space-separated URL
    val firstSpace = text.indexOf(' ')
    if (firstSpace >= 0) {
      val dest = text.substring(0, firstSpace)
      val rest = text.substring(firstSpace + 1).trim
      return (dest, rest)
    }

    // No title part
    (text.trim, "")
  }

  private def extractTitle(text: String): Option[String] = {
    if (text.isEmpty) return None

    // Double quoted title
    if (text.startsWith("\"") && text.endsWith("\"") && text.length >= 2) {
      return Some(text.substring(1, text.length - 1))
    }

    // Single quoted title
    if (text.startsWith("'") && text.endsWith("'") && text.length >= 2) {
      return Some(text.substring(1, text.length - 1))
    }

    // Parenthesized title
    if (text.startsWith("(") && text.endsWith(")") && text.length >= 2) {
      return Some(text.substring(1, text.length - 1))
    }

    None
  }

  private def normalizeLabel(label: String): String = {
    // Unicode case fold, collapse whitespace
    label.trim.toLowerCase.replaceAll("\\s+", " ")
  }
}
