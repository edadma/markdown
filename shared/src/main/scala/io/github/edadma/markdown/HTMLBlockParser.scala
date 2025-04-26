package io.github.edadma.markdown

object HTMLBlockParser extends BlockParser {

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

  private def text(line: LazyList[C]) =
    line.takeWhile(_.char != '\n').map(_.char).mkString

  override def canStart(lines: List[LazyList[C]]): Boolean = lines.headOption.exists { line =>
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
//  override def parse(
//      lines: List[LazyList[C]],
//      linkRefs: scala.collection.mutable.Map[String, LinkReference],
//  ): (Block, Int) = {
//    val first = text(lines.head)
//    // Types 1–3: find matching closer
//    multilineOpeners.collectFirst {
//      case (open, close) if first.trim.startsWith(open) =>
//        val sb = new StringBuilder(first + "\n")
//        var i  = 1
//        while (i < lines.length) {
//          val l = text(lines(i))
//          sb.append(l).append("\n")
//          if (l.contains(close)) return (HTMLBlock(sb.toString), i + 1)
//          i += 1
//        }
//        // fell off end
//        return (HTMLBlock(sb.toString), lines.length)
//    }
//
//    // Type 4: DOCTYPE is single-line
//    if (doctypePattern.matches(first.trim))
//      return (HTMLBlock(first + "\n"), 1)
//
//    // Type 5: script|style|pre — consume until matching </tag>
//    multiTagNames.find(t => first.toLowerCase.startsWith(s"<$t")) match {
//      case Some(tag) =>
//        val endMarker = s"</$tag>"
//        val sb        = new StringBuilder(first + "\n")
//        var i         = 1
//        while (i < lines.length) {
//          val l = text(lines(i))
//          sb.append(l).append("\n")
//          if (l.toLowerCase.contains(endMarker)) return (HTMLBlock(sb.toString), i + 1)
//          i += 1
//        }
//        return (HTMLBlock(sb.toString), lines.length)
//      case None => ()
//    }
//
//    // Types 6 & 7: single-line only
//    (HTMLBlock(first + "\n"), 1)
//  }

//  override def parse(
//      lines: List[LazyList[C]],
//      linkRefs: scala.collection.mutable.Map[String, LinkReference],
//  ): (Block, Int) = {
//
//    // Helper: get the text of a line (drop the '\n')
//    def text(line: LazyList[C]): String =
//      line.takeWhile(_.char != '\n').map(_.char).mkString
//
//    // Helper: consume up through the first line containing `close`,
//    // or all lines if not found
//    def takeUntilClose(close: String): (String, Int) = {
//      // turn each line into its text + "\n"
//      val allLines = lines.map(l => text(l) + "\n")
//      // find the index of the first line (after line 0) containing close
//      val idx = lines.indexWhere(l => text(l).contains(close), 1)
//      if (idx >= 0) (allLines.take(idx + 1).mkString, idx + 1)
//      else (allLines.mkString, lines.length)
//    }
//
//    val firstLine = text(lines.head)
//    val trimmed   = firstLine.trim.toLowerCase
//
//    // Try the seven cases in order, building one Option[(Block,Int)] each:
//    val maybeResult: Option[(Block, Int)] =
//      // 1–3: <!--…-->, <?…?>, <![CDATA[…]]>
//      multilineOpeners
//        .collectFirst { case (open, close) if trimmed.startsWith(open) => close }
//        .map(takeUntilClose)
//        .map { case (block, cnt) => (HTMLBlock(block), cnt) }
//        .orElse(
//          // 4: <!DOCTYPE …>
//          Option.when(doctypePattern.matches(trimmed))((HTMLBlock(firstLine + "\n"), 1)),
//        )
//        .orElse(
//          // 5: <script>, <style>, <pre>…</…>
//          multiTagNames
//            .find(tag => trimmed.startsWith(s"<$tag"))
//            .map(_ => takeUntilClose(s"</${multiTagNames.find(tag => trimmed.startsWith(s"<$tag")).get}>"))
//            .map { case (block, cnt) => (HTMLBlock(block), cnt) },
//        )
//        .orElse(
//          // 6–7: single-line block or generic tag
//          Option.when(genericTagPattern.matches(firstLine))((HTMLBlock(firstLine + "\n"), 1)),
//        )
//
//    // Fall back to a single-line HTMLBlock if somehow nothing matched
//    maybeResult.getOrElse((HTMLBlock(firstLine + "\n"), 1))
//  }

//  override def parse(
//      lines: List[LazyList[C]],
//      linkRefs: scala.collection.mutable.Map[String, LinkReference],
//  ): (Block, Int) = {
//
//    // 1) extract pure text from a cursor‐line (drop the '\n')
//    def text(line: LazyList[C]): String =
//      line.takeWhile(_.char != '\n').map(_.char).mkString
//
//    // 2) helper: take up through the first line containing `close`, or all
//    def takeUntilClose(close: String): (String, Int) = {
//      // build each line + "\n"
//      val all = lines.map(l => text(l) + "\n")
//      // look for closer starting at line 1
//      val idx = lines.indexWhere(l => text(l).contains(close), 1)
//      if (idx >= 0) (all.take(idx + 1).mkString, idx + 1)
//      else (all.mkString, lines.length)
//    }
//
//    val firstLine = text(lines.head)
//    val trimmed   = firstLine.trim
//    val lc        = trimmed.toLowerCase
//
//    // 3) Try each of the seven block‐HTML kinds in turn:
//
//    // (1–3) comments / PI / CDATA
//    multilineOpeners
//      .collectFirst { case (open, close) if lc.startsWith(open) => close }
//      .map(close => {
//        val (body, cnt) = takeUntilClose(close)
//        (HTMLBlock(body), cnt)
//      })
//      // (4) DOCTYPE (single‐line)
//      .orElse(Option.when(doctypePattern.matches(trimmed))((HTMLBlock(firstLine + "\n"), 1)))
//      // (5 & 6) ANY block‐level tag (script/style/pre **and** div, blockquote, table, h1, ul, etc.)
//      .orElse {
//        // find an opening tag for any of your blockTags
//        blockTags.find(tag => lc.startsWith(s"<$tag") && !lc.startsWith(s"</$tag"))
//          .map(tag => {
//            val (body, cnt) = takeUntilClose(s"</$tag>")
//            (HTMLBlock(body), cnt)
//          })
//      }
//      // (7) Any other single‐line <…> (closing tags, spans, inline tags)
//      .orElse(Option.when(genericTagPattern.matches(trimmed))((HTMLBlock(firstLine + "\n"), 1)))
//      // fallback (shouldn’t happen if canStart is correct)
//      .get
//  }

//  override def parse(
//      lines: List[LazyList[C]],
//      linkRefs: scala.collection.mutable.Map[String, LinkReference],
//  ): (Block, Int) = {
//
//    // 1. Get raw text of a line (drop the '\n')
//    def text(line: LazyList[C]): String =
//      line.takeWhile(_.char != '\n').map(_.char).mkString
//
//    // 2. Helper: take lines up through the first occurrence of `close`, or all if not found
//    def takeUntilClose(close: String): (String, Int) = {
//      val allLines = lines.map(l => text(l) + "\n")
//      // look for the closer starting at the *second* line
//      val idx = lines.indexWhere(l => text(l).contains(close), 1)
//      if (idx >= 0) (allLines.take(idx + 1).mkString, idx + 1)
//      else (allLines.mkString, lines.length)
//    }
//
//    val firstLine = text(lines.head)
//    val trimmedLC = firstLine.trim.toLowerCase // lowercase for matching
//
//    // 3. Try each HTML‐block form in turn, producing an Option[(HTMLBlock, count)]
//    val resultOpt: Option[(Block, Int)] =
//
//      // (1–3) comments, PI’s, CDATA
//      multilineOpeners
//        // normalize opener to lower-case before matching
//        .collectFirst { case (open, close) if trimmedLC.startsWith(open.toLowerCase) => close }
//        .map(close => {
//          val (block, cnt) = takeUntilClose(close)
//          (HTMLBlock(block), cnt)
//        })
//
//        // (4) DOCTYPE
//        .orElse(Option.when(doctypePattern.matches(firstLine.trim)) {
//          (HTMLBlock(firstLine + "\n"), 1)
//        })
//
//        // (5–6) any block‐level tag: script, style, pre, div, blockquote, h1–h6, ul, ol, table, etc.
//        .orElse {
//          // find the tag name (lowercase) if this is an opening tag
//          blockTags
//            .find(tag => trimmedLC.startsWith(s"<$tag"))
//            .map(tag => {
//              val close        = s"</$tag>"
//              val (block, cnt) = takeUntilClose(close)
//              (HTMLBlock(block), cnt)
//            })
//        }
//
//        // (7) any other single‐line tag (closing tags, spans, inline tags)
//        .orElse(Option.when(genericTagPattern.matches(firstLine.trim)) {
//          (HTMLBlock(firstLine + "\n"), 1)
//        })
//
//    // 4. Fall back to a one-line block (guaranteed non-null by canStart)
//    resultOpt.getOrElse((HTMLBlock(firstLine + "\n"), 1))
//  }

  override def parse(
      lines: List[LazyList[C]],
      linkRefs: scala.collection.mutable.Map[String, LinkReference],
  ): (Block, Int) = {

    // 1. Turn one LazyList[C] into its String (dropping the trailing '\n')
    def text(line: LazyList[C]): String =
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
