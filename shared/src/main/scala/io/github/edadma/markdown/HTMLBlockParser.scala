package io.github.edadma.markdown

object HTMLBlockParser extends BlockParser {
  val name: String = "HTML blocks"

  // 1–3 openers and closers
  private val multilineOpeners = List(
    "<!--"      -> "-->",
    "<?"        -> "?>",
    "<![CDATA[" -> "]]>",
  )

  // 4: DOCTYPE (single‐line)
  private val doctypePattern = """(?i)^<!DOCTYPE\b.*>$""".r

  // 5: script|style|pre
  private val multiTagNames = Set("script", "style", "pre")

  // 6: other block tags (exact list from spec § 4.6)
  private val blockTags = Set(
    "address",
    "article",
    "aside",
    "base",
    "basefont",
    "blockquote",
    "body",
    "caption",
    "center",
    "col",
    "colgroup",
    "dd",
    "details",
    "dialog",
    "dir",
    "div",
    "dl",
    "dt",
    "fieldset",
    "figcaption",
    "figure",
    "footer",
    "form",
    "frame",
    "frameset",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "head",
    "header",
    "hr",
    "html",
    "li",
    "link",
    "main",
    "menu",
    "menuitem",
    "meta",
    "nav",
    "noframes",
    "ol",
    "optgroup",
    "option",
    "p",
    "param",
    "section",
    "source",
    "summary",
    "table",
    "tbody",
    "td",
    "tfoot",
    "th",
    "thead",
    "title",
    "tr",
    "track",
    "ul",
    "wbr",
  )

  // 7: any other <tag…> or </tag>
  private val genericTagPattern = """^</?[A-Za-z][A-Za-z0-9\-]*(\s+[^>]*)?>\s*$""".r

  private def text(line: List[C]) =
    line.takeWhile(_.char != '\n').map(_.char).mkString

  override def canStart(lines: LazyList[List[C]], config: MarkdownConfig): Boolean = lines.headOption.exists { line =>
    val t  = text(line).trim
    val lc = t.toLowerCase

    // 1. HTML comment
    lc.startsWith("<!--") ||
    // 2. Processing instruction
    lc.startsWith("<?") ||
    // 3. CDATA
    lc.startsWith("<![cdata[") ||
    // 4. DOCTYPE
    doctypePattern.matches(t) ||
    // 5. <script>/<style>/<pre>
    multiTagNames.exists(tag => lc.startsWith(s"<$tag")) ||
    // 6–7. Any other block-level or generic tag (single line)
    genericTagPattern.matches(t)
  }

  override def parse(
      lines: LazyList[List[C]],
      linkRefs: scala.collection.mutable.Map[String, LinkReference],
      parentIndent: Int,
      config: MarkdownConfig,
  ): (Block, Int) = {

    // 1. Turn one LazyList[C] into its String (dropping the trailing '\n')
    def text(line: List[C]): String =
      line.takeWhile(_.char != '\n').map(_.char).mkString

    // 2. Consume up to (and including) the first line containing `close`,
    //    or all lines if never found.
    def takeUntilClose(close: String): (String, Int) = {
      val all = lines.map(l => text(l) + "\n")
      val idx = lines.indexWhere(l => text(l).contains(close), 1)
      if (idx >= 0) (all.take(idx + 1).mkString, idx + 1)
      else (all.mkString, lines.length)
    }

    val firstLine = text(lines.head)
    val trimmed   = firstLine.trim
    val lc        = trimmed.toLowerCase

    // Try each HTML‐block form in order, building an Option[(Block,Int)]
    val resultOpt: Option[(Block, Int)] =

      // 1–3) <!--…-->, <?…?>, <![CDATA[…]]>
      multilineOpeners
        .collectFirst { case (open, close) if lc.startsWith(open.toLowerCase) => close }
        .map(close => {
          val (body, cnt) = takeUntilClose(close)
          (HTMLBlock(body), cnt)
        })

        // 4) <!DOCTYPE …>
        .orElse(Option.when(doctypePattern.matches(trimmed)) {
          (HTMLBlock(firstLine + "\n"), 1)
        })

        // 5) <script>…</script>
        .orElse(Option.when(lc.startsWith("<script")) {
          val (body, cnt) = takeUntilClose("</script>")
          (HTMLBlock(body), cnt)
        })

        // 6) <style>…</style>
        .orElse(Option.when(lc.startsWith("<style")) {
          val (body, cnt) = takeUntilClose("</style>")
          (HTMLBlock(body), cnt)
        })

        // 7) <pre>…</pre>
        .orElse(Option.when(lc.startsWith("<pre")) {
          val (body, cnt) = takeUntilClose("</pre>")
          (HTMLBlock(body), cnt)
        })

        // 8) any other block‐level tag (div, blockquote, table, h1–h6, ul, ol, etc.)
        .orElse {
          // `blockTags` is your Set("div","blockquote","table", "div", …)
          blockTags
            .find(tag => lc.startsWith(s"<$tag"))
            .map(tag => {
              val (body, cnt) = takeUntilClose(s"</$tag>")
              (HTMLBlock(body), cnt)
            })
        }

        // 9) any single‐line tag (</foo> or <span> etc.)
        .orElse(Option.when(genericTagPattern.matches(trimmed)) {
          (HTMLBlock(firstLine + "\n"), 1)
        })

    // Finally, if somehow nothing matched, treat it as one‐line HTML
    resultOpt.getOrElse((HTMLBlock(firstLine + "\n"), 1))
  }

}
