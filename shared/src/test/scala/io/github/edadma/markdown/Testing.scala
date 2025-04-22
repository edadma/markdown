package io.github.edadma.markdown

def parseInlineContent(input: String): List[Inline] = {
  val reader           = new InputReader(input)
  val streamWithoutEOI = reader.stream.takeWhile(_ != EndOfInput)

  parseInline(streamWithoutEOI)
}
