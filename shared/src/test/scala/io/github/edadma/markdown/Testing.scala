package io.github.edadma.markdown

import scala.collection.mutable.ArrayBuffer

def parseInlineContent(input: String): List[Inline] = {
  val reader           = new InputReader(input)
  val streamWithoutEOI = reader.stream.takeWhile(_ != EndOfInput).toList

  parseInline(streamWithoutEOI)
}
