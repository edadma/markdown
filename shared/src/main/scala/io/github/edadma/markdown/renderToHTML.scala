package io.github.edadma.markdown

def renderToHTML(md: String): String = renderToHTML(parseDocumentContent(md))

def parseDocumentContent(input: String): Document =
  val (document, _) = parseDocumentContentWithRefs(input)

  document

def parseDocumentContentWithRefs(input: String): (Document, Map[String, LinkReference]) =
  val reader = new InputReader(input)

  parseDocument(reader.stream)

def renderToHTML(node: Node): String = node match {
  case Document(children)      => children.map(renderToHTML).mkString("\n")
  case Paragraph(inlines)      => s"<p>${renderInlines(inlines)}</p>"
  case Heading(level, inlines) => s"<h$level>${renderInlines(inlines)}</h$level>"
  case Code(content, infoString) =>
    val languageClass = infoString.map(info => s" class=\"language-$info\"").getOrElse("")
    s"<pre><code$languageClass>${escapeHtml(content)}</code></pre>"
  case BlockQuote(children) => s"<blockquote>\n${children.map(renderToHTML).mkString("\n")}\n</blockquote>"
  case ThematicBreak()      => "<hr />"
  case HTMLBlock(content)   => content
  case n: Inline            => sys.error(s"inline node in block position: '$n'")
}

private def renderInlines(inlines: List[Inline]): String =
  inlines.map {
    case Text(content)      => escapeHtml(content)
    case SoftLineBreak()    => "\n"
    case HardLineBreak()    => "<br />\n"
    case CodeSpan(content)  => s"<code>${escapeHtml(content)}</code>"
    case Emphasis(children) => s"<em>${renderInlines(children)}</em>"
    case Strong(children)   => s"<strong>${renderInlines(children)}</strong>"
    case Link(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeHtml(t)}\"").getOrElse("")
      s"""<a href="${escapeHtml(destination)}"$titleAttr>${renderInlines(children)}</a>"""
    case Image(destination, title, children) =>
      val titleAttr = title.map(t => s" title=\"${escapeHtml(t)}\"").getOrElse("")
      s"""<img src="${escapeHtml(destination)}" alt="${renderAltText(children)}"$titleAttr />"""
    case AutoLink(destination, text) => s"""<a href="${escapeHtml(destination)}">${escapeHtml(text)}</a>"""
    case RawHTML(content)            => content // Raw HTML is passed through as-is
    case c: C                        => sys.error(s"unparsed character wrapper: '$c'")
  }.mkString

// Helper for image alt text - extracts plain text only
private def renderAltText(inlines: List[Inline]): String = {
  // For image alt text, we only want the literal text content without formatting
  inlines.map {
    case Text(content)        => escapeHtml(content)
    case CodeSpan(content)    => escapeHtml(content)
    case Emphasis(children)   => renderAltText(children)
    case Strong(children)     => renderAltText(children)
    case Link(_, _, children) => renderAltText(children)
    case _                    => ""
  }.mkString
}

private def escapeHtml(text: String): String =
  text.replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
