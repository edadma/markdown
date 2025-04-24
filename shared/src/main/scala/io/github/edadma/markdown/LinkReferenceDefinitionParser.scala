package io.github.edadma.markdown

import scala.collection.mutable

case class LinkReference(destination: String, title: Option[String])

object LinkReferenceDefinitionParser extends BlockParser {
  def canStart(line: LazyList[Cursor]): Boolean = {
    // Check if the line starts with a potential link reference pattern
    val content = line.takeWhile(_.char != '\n').map(_.char).mkString
    content.trim.startsWith("[") && content.contains("]:")
  }

  def parse(
      lines: List[LazyList[Cursor]],
      linkRefs: mutable.Map[String, LinkReference],
  ): (Block, Int) = {
    // Process the line as a link reference definition
    val line = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    // Parse the link reference definition
    parseDefinition(line) match {
      case Some((label, reference)) =>
        // Store in the map if not already present (first definition wins)
        if (!linkRefs.contains(label)) {
          linkRefs.put(label, reference)
        }
        // Return no block (None) and consume 1 line
        (null, 1) // We'll handle null blocks in the main parser

      case None =>
        // Not a valid link reference, consume 0 lines
        (null, 0)
    }
  }

  private def parseDefinition(line: String): Option[(String, LinkReference)] = {
    // Basic regex to extract components
    val pattern = """^\s*\[(.*?)\]:\s*(?:<([^>]*)>|(\S+))(?:\s+(?:"(.*?)"|(\'(.*?)\')|(\((.*?)\))))?$""".r

    line match {
      case pattern(label, destBracketed, destRaw, dqTitle, sqTitle, _, pTitle, _) =>
        val normalizedLabel = normalizeLabel(label)
        val destination     = Option(destBracketed).getOrElse(destRaw)
        val title           = Option(dqTitle).orElse(Option(sqTitle)).orElse(Option(pTitle))

        Some(normalizedLabel -> LinkReference(destination, title))

      case _ => None
    }
  }

  private def normalizeLabel(label: String): String = {
    // Unicode case fold, collapse whitespace
    label.trim.toLowerCase.replaceAll("\\s+", " ")
  }
}
