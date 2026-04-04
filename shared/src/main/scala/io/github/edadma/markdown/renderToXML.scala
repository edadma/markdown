package io.github.edadma.markdown

private val NS = "http://commonmark.org/xml/1.0"

def renderToXML(doc: Document, system: String = "document"): String = {
  val sb = new StringBuilder
  sb.append("""<?xml version="1.0" encoding="UTF-8"?>""").append("\n")
    .append(s"""<!DOCTYPE $system SYSTEM "CommonMark.dtd">""").append("\n")
    .append(s"<document xmlns=\"$NS\">").append("\n")
  doc.children.foreach(blockToXml(_, sb, 2))
  sb.append("</document>")
  sb.toString
}

private def blockToXml(b: Block, sb: StringBuilder, indent: Int): Unit = b match {
  case ListBlock(data, items) =>
    val typ = if (data.isOrdered) "ordered" else "bullet"
    appendIndent(sb, indent).append(s"<list type=\"$typ\" tight=\"${data.isTight}\">").append("\n")
    items.foreach(itemToXml(_, sb, indent + 2))
    appendIndent(sb, indent).append("</list>").append("\n")

  case Paragraph(inls) =>
    appendIndent(sb, indent).append("<paragraph>")
    inls.foreach(inlineToXml(_, sb, 0))
    sb.append("</paragraph>").append("\n")

  case Heading(level, inls, _) =>
    appendIndent(sb, indent).append(s"<h$level>")
      .append(escapeXml(inls.map {
        case Text(t) => t
        case other   => "" // simplified
      }.mkString))
    sb.append(s"</h$level>").append("\n")

  case Code(content, _, _, _) =>
    appendIndent(sb, indent).append("<code>").append(escapeXml(content)).append("</code>").append("\n")

  case BlockQuote(children) =>
    appendIndent(sb, indent).append("<blockquote>").append("\n")
    children.foreach(c => blockToXml(c, sb, indent + 2))
    appendIndent(sb, indent).append("</blockquote>").append("\n")

  case ThematicBreak() =>
    appendIndent(sb, indent).append("<hr/>").append("\n")

  case HTMLBlock(raw) =>
    appendIndent(sb, indent).append(raw).append("\n")

  case MathBlock(content) =>
    appendIndent(sb, indent).append("<math display=\"true\">").append(escapeXml(content)).append("</math>").append("\n")

}

private def itemToXml(item: ListItem, sb: StringBuilder, indent: Int): Unit = {
  appendIndent(sb, indent).append("<item>").append("\n")
  item.content.foreach(c => blockToXml(c, sb, indent + 2))
  appendIndent(sb, indent).append("</item>").append("\n")
}

private def inlineToXml(inl: Inline, sb: StringBuilder, indent: Int): Unit = inl match {
  case Text(t)         => sb.append("<text>").append(escapeXml(t)).append("</text>")
  case SoftLineBreak() => sb.append("\n")
  case HardLineBreak() => sb.append("\n")
  case CodeSpan(c)     => sb.append("<codespan>").append(escapeXml(c)).append("</codespan>")
  case Emphasis(ch) =>
    sb.append("<emphasis>"); ch.foreach(inlineToXml(_, sb, 0)); sb.append("</emphasis>")
  case Strong(ch) =>
    sb.append("<strong>"); ch.foreach(inlineToXml(_, sb, 0)); sb.append("</strong>")
  case Link(dest, title, ch) =>
    sb.append(s"<link destination=\"${escapeXml(dest)}\"")
    title.foreach(t => sb.append(s" title=\"${escapeXml(t)}\""))
    sb.append(">")
    ch.foreach(inlineToXml(_, sb, 0))
    sb.append("</link>")
  case Image(dest, title, ch, _) =>
    sb.append(s"<image destination=\"${escapeXml(dest)}\"")
    title.foreach(t => sb.append(s" title=\"${escapeXml(t)}\""))
    sb.append(">")
    ch.foreach(inlineToXml(_, sb, 0))
    sb.append("</image>")
  case AutoLink(dest, text) =>
    sb.append(s"<autolink destination=\"${escapeXml(dest)}\">").append(escapeXml(text)).append("</autolink>")
  case RawHTML(html)     => sb.append(html)
  case MathExpr(content) => sb append s"""<math display="false">${escapeXml(content)}</math>"""
  case Emoji(name)              => sb append s"<emoji>$name</emoji>"
  case Strikethrough(ch)        => sb.append("<strikethrough>"); ch.foreach(inlineToXml(_, sb, 0)); sb.append("</strikethrough>")
  case FootnoteReference(label) => sb append s"""<footnote-ref label="$label"/>"""
  case c: C                     => sb.append(escapeXml(c.char.toString))
}

private def appendIndent(sb: StringBuilder, n: Int): StringBuilder = {
  sb.append(" " * n)
}

def escapeXml(s: String): String =
  s.flatMap {
    case '&'  => "&amp;"
    case '<'  => "&lt;"
    case '>'  => "&gt;"
    case '"'  => "&quot;"
    case c    => c.toString
  }
