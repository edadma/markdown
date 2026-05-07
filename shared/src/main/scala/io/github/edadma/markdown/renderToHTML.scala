package io.github.edadma.markdown

// Footnote label → number mapping for current render
private var _footnoteNumbers: Map[String, Int] = Map.empty

/** Render Attributes as HTML attribute string (with leading space if non-empty). */
private def renderAttrs(attrs: Option[Attributes]): String = attrs match {
  case None => ""
  case Some(Attributes(id, classes, kvPairs)) =>
    val parts = new scala.collection.mutable.ListBuffer[String]
    id.foreach(v => parts += s"""id="${escapeXml(v)}"""")
    if (classes.nonEmpty) parts += s"""class="${escapeXml(classes.mkString(" "))}""""
    kvPairs.foreach { case (k, v) => parts += s"""${escapeXml(k)}="${escapeXml(v)}"""" }
    if (parts.nonEmpty) " " + parts.mkString(" ") else ""
}

def renderToHTML(md: String, config: MarkdownConfig = MarkdownConfig.default): String =
  renderToHTML(parseDocumentContent(md, config), config)

def parseDocumentContent(input: String, config: MarkdownConfig = MarkdownConfig.default): Document =
  val (document, _) = parseDocumentContentWithRefs(input, config)

  document

def parseDocumentContentWithRefs(
    input: String,
    config: MarkdownConfig = MarkdownConfig.default,
): (Document, Map[String, LinkReference]) =
  val reader = new InputReader(input)

  parseDocument(reader.stream, config)

def renderBlockToHTML(node: Block, config: MarkdownConfig = MarkdownConfig.default): String =
  node match
    case Paragraph(inlines)      => s"<p>${renderInlines(inlines)}</p>"
    case Heading(level, inlines, attrs) => s"<h$level${renderAttrs(attrs)}>${renderInlines(inlines)}</h$level>"
    case Code(content, infoString, indented, codeAttrs) =>
      // Use the info string language, or for indented blocks use the configured default
      val lang = infoString.orElse(if indented then config.indentedCodeLanguage else None)
      val extraAttrs = renderAttrs(codeAttrs)

      val highlighted = for
        highlighter <- config.codeHighlighter
        l <- lang
        html <- highlighter(content, l)
      yield
        val languageClass = s""" class="language-$l""""
        s"<pre$extraAttrs><code$languageClass>$html</code></pre>"

      highlighted.getOrElse {
        val languageClass = lang.map(l => s" class=\"language-$l\"").getOrElse("")
        val trailing = if (content.nonEmpty) "\n" else ""
        s"<pre$extraAttrs><code$languageClass>${escapeXml(content)}$trailing</code></pre>"
      }
    case BlockQuote(children) =>
      if (children.isEmpty) "<blockquote>\n</blockquote>"
      else
        val body = children.map(renderBlockToHTML(_, config)).mkString("\n")
        val sep = if (body.endsWith("\n")) "" else "\n"
        s"<blockquote>\n$body$sep</blockquote>"
    case ThematicBreak()      => "<hr />"
    case HTMLBlock(content)   => content
    case ListBlock(data, items) =>
      val tagName = if (data.isOrdered) "ol" else "ul"
      val startAttr = if (data.isOrdered && data.startNumber.exists(_ != 1))
        s""" start="${data.startNumber.get}""""
      else
        ""

      s"<$tagName$startAttr>\n${items.map {
          case ListItem(content) =>
            if (content.isEmpty) {
              s"<li></li>\n"
            } else {
              // Check for task list item: first paragraph starts with [ ]/[x]/[X] followed by space
              val (taskCheckbox, adjustedContent) = if (config.taskListItems) {
                content.headOption match {
                  case Some(Paragraph(inlines)) =>
                    inlines.headOption match {
                      case Some(Text(text)) if text.startsWith("[ ] ") =>
                        (Some(false), Paragraph(Text(text.drop(4)) :: inlines.tail) :: content.tail)
                      case Some(Text(text)) if text.toLowerCase.startsWith("[x] ") =>
                        (Some(true), Paragraph(Text(text.drop(4)) :: inlines.tail) :: content.tail)
                      case _ => (None, content)
                    }
                  case _ => (None, content)
                }
              } else (None, content)

              val rendered = adjustedContent.map { block =>
                if (data.isTight) block match {
                  case Paragraph(inlines) => renderInlines(inlines)
                  case other              => renderBlockToHTML(other, config)
                }
                else renderBlockToHTML(block, config)
              }

              val checkboxHtml = taskCheckbox match {
                case Some(true)  => """<input checked="" disabled="" type="checkbox"> """
                case Some(false) => """<input disabled="" type="checkbox"> """
                case None        => ""
              }

              if (data.isTight && adjustedContent.headOption.exists(_.isInstanceOf[Paragraph])) {
                // Tight list: first paragraph inline after <li>, rest on new lines
                if (rendered.size == 1)
                  s"<li>$checkboxHtml${rendered.head}</li>\n"
                else
                  s"<li>$checkboxHtml${rendered.head}\n${rendered.tail.mkString("\n")}\n</li>\n"
              } else {
                val renderedWithCheckbox = if (checkboxHtml.nonEmpty && rendered.nonEmpty) {
                  // In loose lists, inject checkbox after the opening <p> tag
                  val first = rendered.head
                  val injected = if (first.startsWith("<p>")) s"<p>$checkboxHtml${first.drop(3)}" else s"$checkboxHtml$first"
                  injected :: rendered.tail.toList
                } else rendered
                val body = renderedWithCheckbox.mkString("\n")
                val closingNl = if (body.endsWith("\n") ||
                    (data.isTight && adjustedContent.lastOption.exists(_.isInstanceOf[Paragraph]))) ""
                  else "\n"
                s"<li>\n$body$closingNl</li>\n"
              }
            }
        }.mkString}</$tagName>"
    case Table(headerRow, rows, alignments) =>
      val alignAttrs = alignments.map {
        case TableAlignment.Left   => " align=\"left\""
        case TableAlignment.Center => " align=\"center\""
        case TableAlignment.Right  => " align=\"right\""
        case TableAlignment.None   => ""
      }

      val headerHTML = s"<thead>\n<tr>${
          headerRow.cells.zip(alignAttrs).map { case (cell, align) =>
            s"<th$align>${renderInlines(cell.content)}</th>"
          }.mkString
        }</tr>\n</thead>"

      val bodyHTML = if (rows.nonEmpty) {
        s"<tbody>\n${
            rows.map { row =>
              s"<tr>${
                  row.cells.zip(alignAttrs).map { case (cell, align) =>
                    s"<td$align>${renderInlines(cell.content)}</td>"
                  }.mkString
                }</tr>"
            }.mkString("\n")
          }\n</tbody>"
      } else ""

      s"<table>\n$headerHTML\n$bodyHTML\n</table>"
    case DefinitionListBlock(items) =>
      val sb = new StringBuilder("<dl>\n")

      items.foreach { case (term, definitions) =>
        sb.append("  <dt>").append(renderInlines(term)).append("</dt>\n")

        // `definitions` is a List[Block], not Document — call the block
        // renderer directly. Calling the public `renderToHTML(node, config)`
        // would only succeed for Document and throw a MatchError on
        // Paragraph / List / etc.
        definitions.foreach { defBlock =>
          sb.append("  <dd>\n")
          sb.append(renderBlockToHTML(defBlock, config))
          sb.append("  </dd>\n")
        }
      }

      sb.append("</dl>")
      sb.toString
    case MathBlock(content) => s"""<div class="math display">\\[${escapeXml(content)}\\]</div>"""
    case CalloutBlock(calloutType, title, children) =>
      val titleHtml = title.map(t => s"""<div class="callout-title">$t</div>""").getOrElse(
        s"""<div class="callout-title">${calloutType.capitalize}</div>""",
      )

      s"""<div class="callout callout-$calloutType">
         |  $titleHtml
         |  <div class="callout-content">
         |    ${children.map(renderBlockToHTML(_, config)).mkString("\n    ")}
         |  </div>
         |</div>""".stripMargin
    case DocTagBlock(name, target, body, contentMode) =>
      val dt = target match {
        case Some(t) => s"${escapeXml(name)}: ${escapeXml(t)}"
        case None    => escapeXml(name)
      }
      val bodyHtml = contentMode match {
        case ContentMode.Opaque =>
          body match {
            case Paragraph(List(Text(s))) :: Nil => escapeXml(s)
            case _                               => body.map(renderBlockToHTML(_, config)).mkString("\n")
          }
        case ContentMode.InlineMarkdown =>
          body match {
            case Paragraph(inlines) :: Nil => renderInlines(inlines)
            case _                         => body.map(renderBlockToHTML(_, config)).mkString("\n")
          }
        case ContentMode.BlockMarkdown =>
          body.map(renderBlockToHTML(_, config)).mkString("\n")
      }
      s"""<dl class="doc-tag doc-tag-${escapeXml(name)}">\n  <dt>$dt</dt>\n  <dd>$bodyHtml</dd>\n</dl>"""
    case FootnoteDefinition(_, _) => "" // Rendered separately in footnote section
    case CollapsibleBlock(title, isOpen, children) =>
      val openAttr  = if (isOpen) " open" else ""
      val titleText = if title.isEmpty then "Click to expand" else renderInlines(title)

      s"""<details$openAttr>
         |  <summary>$titleText</summary>
         |  ${children.map(renderBlockToHTML(_, config)).mkString("\n")}
         |</details>""".stripMargin

def renderToHTML(node: Node): String = renderToHTML(node, MarkdownConfig.default)

def renderToHTML(node: Node, config: MarkdownConfig): String = node match {
  case Document(children) =>
    if (config.footnotes) {
      // Collect footnote definitions
      val footnoteDefs = children.collect { case fd: FootnoteDefinition => fd.label -> fd }.toMap
      // Collect referenced footnotes in order
      val referencedLabels = collectFootnoteReferences(children)
      val orderedLabels    = referencedLabels.distinct

      // Build label-to-number mapping and set for renderInlines
      _footnoteNumbers = orderedLabels.zipWithIndex.map { case (l, i) => l -> (i + 1) }.toMap

      // Render non-footnote blocks
      val mainContent = children.filterNot(_.isInstanceOf[FootnoteDefinition])
        .map(renderBlockToHTML(_, config))
        .map(s => if (s.endsWith("\n")) s else s + '\n').mkString

      // Render footnote section
      if (orderedLabels.nonEmpty) {
        val footnoteItems = orderedLabels.map { label =>
          val content = footnoteDefs.get(label) match {
            case Some(fd) => fd.content.map(renderBlockToHTML(_, config)).mkString("\n")
            case None     => ""
          }
          // Inject backref into last paragraph if present, otherwise append
          val backref = s""" <a href="#fnref-$label" class="footnote-backref">&#8617;</a>"""
          val withBackref = if (content.contains("</p>")) {
            content.reverse.replaceFirst(">p/<", s">${backref.reverse}>p/<").reverse
          } else {
            content + s"\n<p>$backref</p>"
          }
          s"""<li id="fn-$label">\n$withBackref\n</li>"""
        }.mkString("\n")
        val result = mainContent + s"""<section class="footnotes">\n<ol>\n$footnoteItems\n</ol>\n</section>\n"""
        _footnoteNumbers = Map.empty
        result
      } else {
        _footnoteNumbers = Map.empty
        mainContent
      }
    } else {
      children.map(renderBlockToHTML(_, config)).map(s => if (s.endsWith("\n")) s else s + '\n').mkString
    }
  case n: Inline => sys.error(s"inline node in block position: '$n'")
}

/** Collect footnote reference labels in document order */
private def collectFootnoteReferences(blocks: List[Block]): List[String] = {
  val result = new scala.collection.mutable.ListBuffer[String]

  def visitInlines(inlines: List[Inline]): Unit = inlines.foreach {
    case FootnoteReference(label) => result += label
    case Emphasis(children)       => visitInlines(children)
    case Strong(children)         => visitInlines(children)
    case Strikethrough(children)  => visitInlines(children)
    case Link(_, _, children)     => visitInlines(children)
    case Image(_, _, children, _) => visitInlines(children)
    case _                        =>
  }

  def visitBlocks(blocks: List[Block]): Unit = blocks.foreach {
    case Paragraph(inlines)      => visitInlines(inlines)
    case Heading(_, inlines, _)  => visitInlines(inlines)
    case BlockQuote(children)    => visitBlocks(children)
    case ListBlock(_, items)     => items.foreach(i => visitBlocks(i.content))
    case ListItem(content)       => visitBlocks(content)
    case Table(h, rows, _)       => visitBlocks(h.cells); rows.foreach(r => visitBlocks(r.cells))
    case TableRow(cells)         => cells.foreach(c => visitInlines(c.content))
    case TableCell(content)      => visitInlines(content)
    case FootnoteDefinition(_, content) => visitBlocks(content)
    case DocTagBlock(_, _, body, _)     => visitBlocks(body)
    case CalloutBlock(_, _, children)   => visitBlocks(children)
    case CollapsibleBlock(title, _, children) => visitInlines(title); visitBlocks(children)
    case DefinitionListBlock(items) => items.foreach { case (term, defs) => visitInlines(term); visitBlocks(defs) }
    case _ =>
  }

  visitBlocks(blocks)
  result.toList
}

/** Render a list of inline nodes to HTML.
  *
  * Public so consumers (TOC builders, anchor-text generators, etc.) can
  * render heading or link-text content as HTML without first wrapping it in
  * a [[Paragraph]] and stripping the `<p>` tags.
  */
def renderInlines(inlines: List[Inline]): String =
  inlines.map {
    case Text(content)      => escapeXml(content)
    case SoftLineBreak()    => "\n"
    case HardLineBreak()    => "<br />\n"
    case CodeSpan(content)  => s"<code>${escapeXml(content)}</code>"
    case Emphasis(children) => s"<em>${renderInlines(children)}</em>"
    case Strong(children)        => s"<strong>${renderInlines(children)}</strong>"
    case Strikethrough(children) => s"<del>${renderInlines(children)}</del>"
    case Link(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeXml(t)}\"").getOrElse("")
      s"""<a href="${escapeXml(percentEncode(destination))}"$titleAttr>${renderInlines(children)}</a>"""
    case Image(destination, title, children, imgAttrs) =>
      val titleAttr = title.map(t => s" title=\"${escapeXml(t)}\"").getOrElse("")
      s"""<img src="${escapeXml(percentEncode(destination))}" alt="${renderAltText(children)}"$titleAttr${renderAttrs(imgAttrs)} />"""
    case AutoLink(destination, text) => s"""<a href="${escapeXml(destination)}">${escapeXml(text)}</a>"""
    case RawHTML(content)            => content // Raw HTML is passed through as-is
    case MathExpr(content)           => s"""<span class="math inline">\\(${escapeXml(content)}\\)</span>"""
    case Emoji(name)                 => emojis(name)
    case FootnoteReference(label) =>
      val num = _footnoteNumbers.getOrElse(label, 0)
      s"""<sup class="footnote-ref"><a href="#fn-$label" id="fnref-$label">$num</a></sup>"""
    case c: C                        => sys.error(s"unparsed character wrapper: '$c'")
  }.mkString

// Helper for image alt text - extracts plain text only
private def renderAltText(inlines: List[Inline]): String = plainText(inlines, escape = true)

/** Plain-text projection of a list of inline nodes — strips all formatting
  * and returns just the textual content. Used internally for image `alt`
  * attributes; exposed publicly because it's the obvious building block for
  * heading-id slugs, ARIA labels, page `<title>` extraction, etc.
  *
  * @param inlines the inlines to flatten
  * @param escape  if `true`, run the result through XML escaping (suitable for
  *                emitting straight into an HTML attribute or `<title>` tag);
  *                if `false`, return the raw text (suitable for slugify
  *                callbacks and other downstream string processing).
  */
def plainText(inlines: List[Inline], escape: Boolean = false): String = {
  def esc(s: String): String = if (escape) escapeXml(s) else s
  val buf                    = new StringBuilder
  def go(node: Inline): Unit = node match {
    case Text(content)            => buf ++= esc(content)
    case CodeSpan(content)        => buf ++= esc(content)
    case Emphasis(children)       => children.foreach(go)
    case Strong(children)         => children.foreach(go)
    case Strikethrough(children)  => children.foreach(go)
    case Link(_, _, children)     => children.foreach(go)
    case Image(_, _, children, _) => children.foreach(go)
    case AutoLink(href, text)     => buf ++= esc(text)
    case SoftLineBreak()          => buf += ' '
    case HardLineBreak()          => buf += ' '
    case Emoji(name)              => buf ++= emojis(name)
    case _                        => () // RawHTML, MathExpr, FootnoteReference, C: no plain-text contribution
  }
  inlines.foreach(go)
  buf.toString
}
