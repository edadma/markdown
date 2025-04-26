package io.github.edadma.markdown

def parseInlineContent(input: String, linkRefs: Map[String, LinkReference] = Map()): List[Inline] = {
  val reader           = new InputReader(input)
  val streamWithoutEOI = reader.stream.takeWhile(_ != EndOfInput).toList

  parseInline(streamWithoutEOI, linkRefs)
}
