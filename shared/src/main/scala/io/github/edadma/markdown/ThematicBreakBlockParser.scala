package io.github.edadma.markdown

import scala.collection.mutable

object ThematicBreakBlockParser extends BlockParser {
  val name: String = "thematic breaks"

  /** Can this parser start on the very first line in `lines`? */
  def canStart(lines: List[LazyList[C]], config: MarkdownConfig): Boolean = {
    if (lines.isEmpty) return false

    // Grab the raw line (without the trailing newline)
    val raw = lines.head.takeWhile(_.char != '\n').map(_.char).mkString

    // Up to three spaces of indentation
    val indent = raw.segmentLength(c => c == ' ')
    if (indent > 3) return false

    // The rest of the line
    val rest = raw.drop(indent)

    // Strip trailing spaces/tabs
    val body = rest.stripTrailing()

    // Remove all spaces and tabs to see the markers
    val markers = body.filterNot(c => c == ' ' || c == '\t')

    // Must be at least three identical markers (*, -, or _), and
    // the body may only contain those markers plus spaces/tabs
    markers.nonEmpty &&
    markers.forall(_ == markers.head) &&
    Set('*', '-', '_').contains(markers.head) &&
    markers.length >= 3 &&
    body.forall(c => c == markers.head || c == ' ' || c == '\t')
  }

  /** Parse a thematic break (always consumes exactly 1 line). */
  def parse(
      lines: List[LazyList[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    (ThematicBreak(), 1)
  }
}
