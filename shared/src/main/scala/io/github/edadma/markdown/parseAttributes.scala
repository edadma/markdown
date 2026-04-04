package io.github.edadma.markdown

/** Parse a `{#id .class key=value}` attribute string.
  * Returns `Some(Attributes)` on success, `None` if not valid attribute syntax.
  */
def parseAttributes(input: String): Option[Attributes] = {
  val trimmed = input.trim
  if (!trimmed.startsWith("{") || !trimmed.endsWith("}")) return None

  val inner = trimmed.substring(1, trimmed.length - 1).trim
  if (inner.isEmpty) return None

  val tokens = tokenizeAttributes(inner)
  if (tokens.isEmpty) return None

  import scala.util.boundary, boundary.break

  boundary {
    var id      = Option.empty[String]
    var classes = List.empty[String]
    var kvPairs = Map.empty[String, String]

    for (token <- tokens) {
      if (token.startsWith("#")) {
        val value = token.substring(1)
        if (value.isEmpty || value.exists(_.isWhitespace)) break(None)
        id = Some(value)
      } else if (token.startsWith(".")) {
        val value = token.substring(1)
        if (value.isEmpty || value.exists(_.isWhitespace)) break(None)
        classes = classes :+ value
      } else if (token.contains("=")) {
        val eqIdx = token.indexOf('=')
        val key   = token.substring(0, eqIdx)
        val raw   = token.substring(eqIdx + 1)
        if (key.isEmpty) break(None)
        // Strip surrounding quotes from value if present
        val value = if ((raw.startsWith("\"") && raw.endsWith("\"")) ||
                        (raw.startsWith("'") && raw.endsWith("'")))
          raw.substring(1, raw.length - 1)
        else raw
        kvPairs = kvPairs + (key -> value)
      } else {
        break(None) // Invalid token
      }
    }

    Some(Attributes(id, classes, kvPairs))
  }
}

/** Tokenize attribute string, respecting quoted values. */
private def tokenizeAttributes(input: String): List[String] = {
  val tokens = new scala.collection.mutable.ListBuffer[String]
  val current = new StringBuilder
  var i = 0
  var inQuote: Char = 0

  while (i < input.length) {
    val c = input.charAt(i)
    if (inQuote != 0) {
      current.append(c)
      if (c == inQuote) inQuote = 0
    } else if (c == '"' || c == '\'') {
      inQuote = c
      current.append(c)
    } else if (c == ' ' || c == '\t') {
      if (current.nonEmpty) {
        tokens += current.toString
        current.clear()
      }
    } else {
      current.append(c)
    }
    i += 1
  }

  if (current.nonEmpty) tokens += current.toString
  tokens.toList
}

/** Try to extract a trailing `{...}` attribute block from a string.
  * Returns `(contentWithoutAttrs, Some(Attributes))` or `(originalContent, None)`.
  */
def extractTrailingAttributes(content: String): (String, Option[Attributes]) = {
  val trimmed = content.replaceAll("\\s+$", "")
  if (!trimmed.endsWith("}")) return (content, None)

  // Find matching opening brace, skipping nested braces
  var depth = 0
  var i = trimmed.length - 1
  while (i >= 0) {
    if (trimmed.charAt(i) == '}') depth += 1
    else if (trimmed.charAt(i) == '{') {
      depth -= 1
      if (depth == 0) {
        val attrStr = trimmed.substring(i)
        parseAttributes(attrStr) match {
          case Some(attrs) =>
            val before = trimmed.substring(0, i).replaceAll("\\s+$", "")
            return (before, Some(attrs))
          case None =>
            return (content, None)
        }
      }
    }
    i -= 1
  }

  (content, None)
}
