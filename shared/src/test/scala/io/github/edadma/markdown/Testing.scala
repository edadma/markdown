package io.github.edadma.markdown

import scala.collection.immutable

def parseInlineContent(input: String): List[Inline] = {
  val reader           = new InputReader(input)
  val streamWithoutEOI = reader.stream.takeWhile(_ != EndOfInput).toList

  parseInline(streamWithoutEOI, Map())
}

def parseDocumentContent(input: String): Document = {
  val (document, _) = parseDocumentContentWithRefs(input)

  document
}

def parseDocumentContentWithRefs(input: String): (Document, immutable.Map[String, LinkReference]) = {
  val reader = new InputReader(input)

  parseDocument(reader.stream)
}
