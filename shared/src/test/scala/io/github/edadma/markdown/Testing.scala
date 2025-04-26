package io.github.edadma.markdown

def parseInlineContent(input: String, linkRefs: Map[String, LinkReference] = Map()): List[Inline] = {
  val reader           = new InputReader(input)
  val streamWithoutEOI = reader.stream.takeWhile(_ != EndOfInput).toList

  parseInline(streamWithoutEOI, Map())
}

def parseDocumentContent(input: String): Document = {
  val (document, _) = parseDocumentContentWithRefs(input)

  document
}

def parseDocumentContentWithRefs(input: String): (Document, Map[String, LinkReference]) = {
  val reader = new InputReader(input)

  parseDocument(reader.stream)
}
