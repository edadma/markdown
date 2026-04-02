package io.github.edadma.markdown

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
    case Heading(level, inlines) => s"<h$level>${renderInlines(inlines)}</h$level>"
    case Code(content, infoString, indented) =>
      // Use the info string language, or for indented blocks use the configured default
      val lang = infoString.orElse(if indented then config.indentedCodeLanguage else None)

      val highlighted = for
        highlighter <- config.codeHighlighter
        l <- lang
        html <- highlighter(content, l)
      yield
        val languageClass = s""" class="language-$l""""
        s"<pre><code$languageClass>$html</code></pre>"

      highlighted.getOrElse {
        val languageClass = lang.map(l => s" class=\"language-$l\"").getOrElse("")
        val trailing = if (content.nonEmpty) "\n" else ""
        s"<pre><code$languageClass>${escapeXml(content)}$trailing</code></pre>"
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

        definitions.foreach { defBlock =>
          sb.append("  <dd>\n")
          sb.append(renderToHTML(defBlock, config))
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
    case CollapsibleBlock(title, isOpen, children) =>
      val openAttr  = if (isOpen) " open" else ""
      val titleText = if title.isEmpty then "Click to expand" else renderInlines(title)

      s"""<details$openAttr>
         |  <summary>$titleText</summary>
         |  ${children.map(renderBlockToHTML(_, config)).mkString("\n")}
         |</details>""".stripMargin

def renderToHTML(node: Node): String = renderToHTML(node, MarkdownConfig.default)

def renderToHTML(node: Node, config: MarkdownConfig): String = node match {
  case Document(children) => children.map(renderBlockToHTML(_, config)).map(s => if (s.endsWith("\n")) s else s + '\n').mkString
  case n: Inline          => sys.error(s"inline node in block position: '$n'")
}

private def renderInlines(inlines: List[Inline]): String =
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
    case Image(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeXml(t)}\"").getOrElse("")
      s"""<img src="${escapeXml(percentEncode(destination))}" alt="${renderAltText(children)}"$titleAttr />"""
    case AutoLink(destination, text) => s"""<a href="${escapeXml(destination)}">${escapeXml(text)}</a>"""
    case RawHTML(content)            => content // Raw HTML is passed through as-is
    case MathExpr(content)           => s"""<span class="math inline">\\(${escapeXml(content)}\\)</span>"""
    case Emoji(name)                 => emojis(name)
    case c: C                        => sys.error(s"unparsed character wrapper: '$c'")
  }.mkString

// Helper for image alt text - extracts plain text only
private def renderAltText(inlines: List[Inline]): String = {
  // For image alt text, we only want the literal text content without formatting
  inlines.map {
    case Text(content)        => escapeXml(content)
    case CodeSpan(content)    => escapeXml(content)
    case Emphasis(children)   => renderAltText(children)
    case Strong(children)        => renderAltText(children)
    case Strikethrough(children) => renderAltText(children)
    case Link(_, _, children)    => renderAltText(children)
    case Image(_, _, children) => renderAltText(children)
    case _                     => ""
  }.mkString
}
