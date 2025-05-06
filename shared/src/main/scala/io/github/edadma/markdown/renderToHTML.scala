package io.github.edadma.markdown

def renderToHTML(md: String, config: MarkdownConfig = MarkdownConfig.default): String =
  renderToHTML(parseDocumentContent(md, config))

def parseDocumentContent(input: String, config: MarkdownConfig = MarkdownConfig.default): Document =
  val (document, _) = parseDocumentContentWithRefs(input, config)

  document

def parseDocumentContentWithRefs(
    input: String,
    config: MarkdownConfig = MarkdownConfig.default,
): (Document, Map[String, LinkReference]) =
  val reader = new InputReader(input)

  parseDocument(reader.stream, config)

def renderToHTML(node: Node): String = node match {
  case Document(children)      => children.map(renderToHTML).mkString("\n")
  case Paragraph(inlines)      => s"<p>${renderInlines(inlines)}</p>"
  case Heading(level, inlines) => s"<h$level>${renderInlines(inlines)}</h$level>"
  case Code(content, infoString) =>
    val languageClass = infoString.map(info => s" class=\"language-$info\"").getOrElse("")
    s"<pre><code$languageClass>${escapeXml(content)}</code></pre>"
  case BlockQuote(children) => s"<blockquote>\n${children.map(renderToHTML).mkString("\n")}\n</blockquote>"
  case ThematicBreak()      => "<hr />"
  case HTMLBlock(content)   => content
  // In the renderToHTML function, add cases for ListBlock and ListItem
  case ListBlock(data, items) =>
    val tagName = if (data.isOrdered) "ol" else "ul"
    val startAttr = if (data.isOrdered && data.startNumber.exists(_ != 1))
      s""" start="${data.startNumber.get}""""
    else
      ""

    s"<$tagName$startAttr>\n${items.flatMap {
        case ListItem(List(Paragraph(List(Text(text))))) => s"<li>$text</li>\n"
        case ListItem(content)                           => s"<li>${content.map(renderToHTML).mkString("\n")}\n</li>"
      }.mkString}</$tagName>"
  // In the renderToHTML function, add cases for Table, TableRow, and TableCell
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
        sb.append(renderToHTML(defBlock))
        sb.append("  </dd>\n")
      }
    }

    sb.append("</dl>")
    sb.toString
  case MathBlock(content) => s"""<div class="math display">\\[${escapeXml(content)}\\]</div>"""
  case n: Inline          => sys.error(s"inline node in block position: '$n'")
}

private def renderInlines(inlines: List[Inline]): String =
  inlines.map {
    case Text(content)      => escapeXml(content)
    case SoftLineBreak()    => "\n"
    case HardLineBreak()    => "<br />\n"
    case CodeSpan(content)  => s"<code>${escapeXml(content)}</code>"
    case Emphasis(children) => s"<em>${renderInlines(children)}</em>"
    case Strong(children)   => s"<strong>${renderInlines(children)}</strong>"
    case Link(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeXml(t)}\"").getOrElse("")
      s"""<a href="${escapeXml(destination)}"$titleAttr>${renderInlines(children)}</a>"""
    case Image(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeXml(t)}\"").getOrElse("")
      s"""<img src="${escapeXml(destination)}" alt="${renderAltText(children)}"$titleAttr />"""
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
    case Strong(children)     => renderAltText(children)
    case Link(_, _, children) => renderAltText(children)
    case _                    => ""
  }.mkString
}
