package io.github.edadma.markdown

def renderToHTML(node: Node): String = node match {
  case Document(children) =>
    children.map(renderToHTML).mkString("\n")

  case Paragraph(inlines) =>
    s"<p>${renderInlines(inlines)}</p>"

  case _ => "" // Ignore other node types for now
}

private def renderInlines(inlines: List[Inline]): String =
  inlines.map {
    case Text(content) => escapeHtml(content)
    case _             => "" // Ignore other inline types for now
  }.mkString

private def escapeHtml(text: String): String =
  text.replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
