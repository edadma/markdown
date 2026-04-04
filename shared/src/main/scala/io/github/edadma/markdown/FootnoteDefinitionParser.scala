package io.github.edadma.markdown

import scala.collection.mutable

object FootnoteDefinitionParser extends BlockParser {
  val name: String = "footnote definitions"

  // [^label]: content
  private val FootnoteDefPattern = """^ {0,3}\[\^([^\]]+)\]:\s+(.*)$""".r
  private val FootnoteDefEmpty   = """^ {0,3}\[\^([^\]]+)\]:\s*$""".r

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (!config.footnotes) return false
    if (lines.isEmpty) return false

    val lineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString
    FootnoteDefPattern.matches(lineText) || FootnoteDefEmpty.matches(lineText)
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    val firstLineText = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    val (label, firstContent) = FootnoteDefPattern.findFirstMatchIn(firstLineText) match {
      case Some(m) => (m.group(1), m.group(2))
      case None =>
        val m = FootnoteDefEmpty.findFirstMatchIn(firstLineText).get
        (m.group(1), "")
    }

    // Collect continuation lines (indented by 4+ spaces or blank)
    var linesConsumed = 1
    val contentLines  = new mutable.ListBuffer[String]
    if (firstContent.nonEmpty) contentLines += firstContent

    var currentLines = lines.tail
    var continue     = true

    while (continue && currentLines.nonEmpty) {
      val line     = currentLines.head
      val lineText = line.takeWhile(_.char != '\n').map(_.char).mkString
      val isBlank  = lineText.forall(c => c == ' ' || c == '\t')

      if (isBlank) {
        contentLines += ""
        linesConsumed += 1
        currentLines = currentLines.tail
      } else if (lineText.startsWith("    ") || lineText.startsWith("\t")) {
        // Remove 4 spaces of indentation
        val stripped = if (lineText.startsWith("    ")) lineText.drop(4)
                       else lineText.drop(1)
        contentLines += stripped
        linesConsumed += 1
        currentLines = currentLines.tail
      } else {
        continue = false
      }
    }

    // Trim trailing blank lines
    while (contentLines.nonEmpty && contentLines.last.isEmpty) contentLines.dropRightInPlace(1)

    // Parse content as blocks
    val contentStr = contentLines.mkString("\n") + "\n"
    val reader     = new InputReader(contentStr)
    val blocks     = processLines(groupIntoLines(reader.stream), linkRefs, parentIndent, config)

    (FootnoteDefinition(label, blocks), linesConsumed)
  }
}
