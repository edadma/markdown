package io.github.edadma.markdown

import scala.collection.mutable

/** Block-level parser for `@name [target] [— body]` doc-tag annotations.
  *
  * Opt-in via `MarkdownConfig.docTags` (`DocTagConfig`). When disabled, `@`-lines fall through to ordinary paragraph
  * parsing — this parser returns `false` from `canStart` unless the extension is enabled.
  */
object DocTagBlockParser extends BlockParser {
  val name: String = "doc tags"

  // Separators recognized after a target: em-dash (U+2014), en-dash (U+2013), hyphen, colon.
  private def isSeparator(c: Char): Boolean = c == '\u2014' || c == '\u2013' || c == '-' || c == ':'

  private def isTagNameStart(c: Char): Boolean = c.isLetter
  private def isTagNameChar(c: Char): Boolean  = c.isLetterOrDigit || c == '_' || c == '-'

  private def isIdentStart(c: Char): Boolean = c.isLetter || c == '_'
  private def isIdentChar(c: Char): Boolean  = c.isLetterOrDigit || c == '_'

  /** Parse a line's leading whitespace + optional `@name`. Returns `(atColumn, tagName, restOfLine)` if the line looks
    * like a doc-tag trigger (honoring backslash escapes on the `@`), else `None`.
    */
  private def matchTrigger(line: List[C]): Option[(Int, String, List[C])] = {
    val content = line.takeWhile(_.char != '\n')
    // Leading spaces/tabs. Use tab-expanded input; tabs are already expanded in leading position by groupIntoLines.
    val leadingWs = content.takeWhile(c => c.char == ' ' || c.char == '\t')
    val atColumn = leadingWs.length
    val after    = content.drop(atColumn)
    if (after.isEmpty) return None
    val atCh = after.head
    if (atCh.char != '@' || atCh.isLiteral) return None
    val afterAt = after.tail
    if (afterAt.isEmpty) return None
    val nameHead = afterAt.head
    if (!isTagNameStart(nameHead.char)) return None
    val nameChars = afterAt.takeWhile(c => isTagNameChar(c.char))
    val tagName   = nameChars.map(_.char).mkString
    val rest      = afterAt.drop(nameChars.length)
    Some((atColumn, tagName, rest))
  }

  /** Strip leading whitespace chars from a string (spaces + tabs). */
  private def stripLeadingWs(s: String): String = s.dropWhile(c => c == ' ' || c == '\t')

  /** Count leading space/tab columns (tabs already expanded for leading whitespace by groupIntoLines). */
  private def leadingIndent(line: List[C]): Int = line.takeWhile(c => c.char == ' ' || c.char == '\t').length

  /** True if `line` is blank (only whitespace, maybe a newline). */
  private def lineIsBlank(line: List[C]): Boolean = {
    val content = line.filter(_.char != '\n')
    content.isEmpty || content.forall(c => c.char == ' ' || c.char == '\t')
  }

  /** Look up a tag, returning the definition and an `unknown` flag. Returns `None` in strict mode for unknown tags,
    * which signals `canStart`/`parse` to decline.
    */
  private def resolveTag(tagName: String, cfg: DocTagConfig): Option[(TagDefinition, Boolean)] =
    cfg.registry.lookup(tagName) match {
      case Some(d) => Some((d, false))
      case None =>
        if (cfg.strictUnknownTags) None
        else Some((TagDefinition(tagName, acceptsTarget = true, contentMode = ContentMode.InlineMarkdown), true))
    }

  def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = {
    if (!config.docTags.enabled) return false
    if (lines.isEmpty) return false
    matchTrigger(lines.head) match {
      case None => false
      case Some((atColumn, tagName, _)) =>
        // Don't trigger on indented-code-block territory (4+ leading spaces).
        if (atColumn >= 4) false
        else resolveTag(tagName, config.docTags).isDefined
    }
  }

  def parse(
      lines: LazyList[List[C]],
      linkRefs: mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {
    val docCfg = config.docTags
    val first  = lines.head

    val (atColumn, tagName, rest) = matchTrigger(first).get
    val tagDef: TagDefinition = resolveTag(tagName, docCfg) match {
      case Some((d, _)) => d
      case None         => return (null, 0)
    }

    // Build first-line body text from `rest` (after @name).
    val restText = rest.map(_.char).mkString

    val (target, firstLineBody) =
      if (tagDef.acceptsTarget) {
        // Consume optional whitespace, then try identifier target.
        val afterWs1 = stripLeadingWs(restText)
        val idLen: Int =
          if (afterWs1.nonEmpty && isIdentStart(afterWs1.head)) {
            var i = 1
            while (i < afterWs1.length && isIdentChar(afterWs1.charAt(i))) i += 1
            i
          } else 0
        if (idLen == 0) {
          // No target; still strip an optional leading separator.
          val afterSep = {
            val t = afterWs1
            if (t.nonEmpty && isSeparator(t.head)) stripLeadingWs(t.drop(1)) else t
          }
          (None, afterSep)
        } else {
          val tgt      = afterWs1.substring(0, idLen)
          val afterId  = afterWs1.substring(idLen)
          val afterWs2 = stripLeadingWs(afterId)
          val afterSep =
            if (afterWs2.nonEmpty && isSeparator(afterWs2.head)) stripLeadingWs(afterWs2.drop(1))
            else afterWs2
          (Some(tgt), afterSep)
        }
      } else {
        // No target, no separator stripping.
        (None, stripLeadingWs(restText))
      }

    // Collect continuation lines: indent > atColumn, not blank, not another @-tag line.
    val continuationRaw = new mutable.ListBuffer[String]
    var linesConsumed   = 1
    var remaining       = lines.tail

    var continue = true
    while (continue && remaining.nonEmpty) {
      val ln = remaining.head
      if (lineIsBlank(ln)) {
        continue = false
      } else {
        val ind = leadingIndent(ln)
        if (ind <= atColumn) {
          continue = false
        } else {
          // Check if this line itself is a valid doc-tag trigger — terminates the current tag.
          val isTagLine = matchTrigger(ln) match {
            case Some((_, tn, _)) => resolveTag(tn, docCfg).isDefined
            case None             => false
          }
          if (isTagLine) {
            continue = false
          } else {
            val text = ln.takeWhile(_.char != '\n').map(_.char).mkString
            continuationRaw += text
            linesConsumed += 1
            remaining = remaining.tail
          }
        }
      }
    }

    // Dedent continuation lines by the minimum leading whitespace across them.
    val continuationStripped: List[String] =
      if (continuationRaw.isEmpty) Nil
      else {
        val minIndent = continuationRaw.iterator.map { s =>
          val n = s.takeWhile(c => c == ' ' || c == '\t').length
          n
        }.min
        continuationRaw.iterator.map(_.drop(minIndent)).toList
      }

    // Build combined body text.
    val bodyLines: List[String] =
      (if (firstLineBody.nonEmpty) List(firstLineBody) else Nil) ++ continuationStripped
    val bodyText = bodyLines.mkString("\n")

    // Produce body blocks per content mode.
    val bodyBlocks: List[Block] = tagDef.contentMode match {
      case ContentMode.Opaque =>
        if (bodyText.isEmpty) Nil
        else List(Paragraph(List(Text(bodyText))))
      case ContentMode.InlineMarkdown =>
        if (bodyText.isEmpty) Nil
        else {
          // Build a Paragraph with C cursors so processInlines will run parseInline on it later.
          val reader  = new InputReader(bodyText)
          val cursors = reader.stream.takeWhile(c => c != EndOfInput).toList
          // Trim trailing newline cursor if present to avoid a stray soft break.
          val trimmed =
            if (cursors.nonEmpty && cursors.last.char == '\n') cursors.dropRight(1)
            else cursors
          List(Paragraph(trimmed))
        }
      case ContentMode.BlockMarkdown =>
        if (bodyText.isEmpty) Nil
        else {
          val reader = new InputReader(bodyText + "\n")
          processLines(groupIntoLines(reader.stream), linkRefs, parentIndent, config)
        }
    }

    (DocTagBlock(tagDef.name, target, bodyBlocks, tagDef.contentMode), linesConsumed)
  }
}
