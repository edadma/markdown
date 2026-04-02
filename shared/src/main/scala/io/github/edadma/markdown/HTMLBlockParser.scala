package io.github.edadma.markdown

object HTMLBlockParser extends BlockParser {
  val name: String = "HTML blocks"

  // Type 1: raw content tags (content not parsed as markdown)
  private val type1Tags = Set("script", "style", "pre", "textarea")

  // Type 6: block-level tags (exact list from CommonMark spec § 4.6)
  private val blockTags = Set(
    "address", "article", "aside", "base", "basefont", "blockquote", "body",
    "caption", "center", "col", "colgroup", "dd", "details", "dialog", "dir",
    "div", "dl", "dt", "fieldset", "figcaption", "figure", "footer", "form",
    "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header",
    "hr", "html", "iframe", "legend", "li", "link", "main", "menu", "menuitem",
    "nav", "noframes", "ol", "optgroup", "option", "p", "param", "search",
    "section", "source", "summary", "table", "tbody", "td", "tfoot", "th",
    "thead", "title", "tr", "track", "ul",
  )

  private def text(line: List[C]): String =
    line.takeWhile(_.char != '\n').map(_.char).mkString

  /** Classify an HTML block by its CommonMark type (1-7), or 0 if not an HTML block. */
  private def classifyHtmlBlock(line: List[C]): Int = {
    // Check if the leading < is escaped
    val stripped = line.dropWhile(c => c.char == ' ')
    if (stripped.nonEmpty && stripped.head.char == '<' && stripped.head.isLiteral) return 0

    // Check leading indent (must be 0-3 spaces)
    val leadingSpaces = line.takeWhile(c => c.char == ' ' && !c.isLiteral).length
    if (leadingSpaces > 3) return 0

    val t  = text(line).trim
    val lc = t.toLowerCase

    // Type 2: HTML comment
    if (lc.startsWith("<!--")) return 2

    // Type 3: Processing instruction
    if (lc.startsWith("<?")) return 3

    // Type 5: CDATA
    if (lc.startsWith("<![cdata[")) return 5

    // Type 4: <!LETTER (includes DOCTYPE)
    if (t.length >= 3 && t.charAt(0) == '<' && t.charAt(1) == '!' && t.charAt(2).isUpper) return 4

    // Type 1: <script, <pre, <style, <textarea followed by space/tab/>/end-of-line
    for (tag <- type1Tags) {
      if (lc.startsWith(s"<$tag")) {
        val afterTag = lc.drop(tag.length + 1)
        if (afterTag.isEmpty || afterTag.charAt(0) == ' ' || afterTag.charAt(0) == '\t' ||
            afterTag.charAt(0) == '>' || afterTag.charAt(0) == '\n')
          return 1
      }
    }

    // Type 6: block-level tag opener or closer
    // <tagname followed by space/tab/>/end-of-line or />
    // </tagname followed by space/tab/>/end-of-line
    if (lc.startsWith("</")) {
      val rest = lc.drop(2)
      val tagName = rest.takeWhile(c => c.isLetterOrDigit || c == '-')
      if (tagName.nonEmpty && blockTags.contains(tagName)) {
        val afterTag = rest.drop(tagName.length)
        if (afterTag.isEmpty || afterTag.charAt(0) == ' ' || afterTag.charAt(0) == '\t' ||
            afterTag.charAt(0) == '>')
          return 6
      }
    } else if (lc.startsWith("<")) {
      val rest = lc.drop(1)
      val tagName = rest.takeWhile(c => c.isLetterOrDigit || c == '-')
      if (tagName.nonEmpty && blockTags.contains(tagName)) {
        val afterTag = rest.drop(tagName.length)
        if (afterTag.isEmpty || afterTag.charAt(0) == ' ' || afterTag.charAt(0) == '\t' ||
            afterTag.charAt(0) == '>' || afterTag.charAt(0) == '/' || afterTag.charAt(0) == '\n')
          return 6
      }
    }

    // Type 7: complete open tag or closing tag, alone on a line
    // Use rawText to preserve backslash escapes
    val rt = rawText(line).trim
    if (rt.startsWith("<") && rt.endsWith(">")) {
      val inner = rt.substring(1, rt.length - 1)
      if (inner.nonEmpty && isHtmlTag(inner)) {
        // But not if it's a type 1 tag name
        val tagNameFromInner = if (inner.startsWith("/")) inner.drop(1).takeWhile(c => c.isLetterOrDigit || c == '-')
                               else inner.takeWhile(c => c.isLetterOrDigit || c == '-')
        if (!type1Tags.contains(tagNameFromInner.toLowerCase))
          return 7
      }
    }

    0
  }

  override def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean =
    lines.headOption.exists(line => classifyHtmlBlock(line) > 0)

  /** Type 7 cannot interrupt a paragraph. Types 1-6 can. */
  def canInterruptParagraph(lines: LazyList[List[C]], config: MarkdownConfig): Boolean =
    lines.headOption.exists(line => {
      val t = classifyHtmlBlock(line)
      t >= 1 && t <= 6
    })

  override def parse(
      lines: LazyList[List[C]],
      linkRefs: scala.collection.mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {

    def rawTextLine(line: List[C]): String = rawText(line)

    // Collect lines until one contains `close` (case-insensitive), starting from line 0
    def takeUntilClose(close: String): (String, Int) = {
      val closeLower = close.toLowerCase
      val all = lines.map(l => rawTextLine(l) + "\n")
      val idx = lines.indexWhere(l => text(l).toLowerCase.contains(closeLower))
      if (idx >= 0) (all.take(idx + 1).mkString, idx + 1)
      else (all.mkString, lines.length)
    }

    def isBlank(line: List[C]): Boolean = {
      val content = line.filter(_.char != '\n')
      content.isEmpty || content.forall(c => c.char == ' ' || c.char == '\t')
    }

    // Collect lines until a blank line (exclusive) or end of input
    def takeUntilBlank(): (String, Int) = {
      val all = lines.map(l => rawTextLine(l) + "\n")
      val idx = lines.indexWhere(l => isBlank(l))
      if (idx >= 0) (all.take(idx).mkString, idx)
      else (all.mkString, lines.length)
    }

    val htmlType = classifyHtmlBlock(lines.head)

    val (block, count) = htmlType match {
      case 1 =>
        // Find which type 1 tag matched
        val lc = text(lines.head).trim.toLowerCase
        val tag = type1Tags.find(t => lc.startsWith(s"<$t")).get
        takeUntilClose(s"</$tag>")

      case 2 => takeUntilClose("-->")
      case 3 => takeUntilClose("?>")
      case 4 => takeUntilClose(">")
      case 5 => takeUntilClose("]]>")

      case 6 => takeUntilBlank()
      case 7 => takeUntilBlank()

      case _ =>
        // Fallback: single line
        (rawTextLine(lines.head) + "\n", 1)
    }

    (HTMLBlock(block), count)
  }

}
