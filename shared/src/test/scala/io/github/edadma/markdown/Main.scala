package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  pprintln(parseInlineContent("***triple emphasis***"))

  val input =
    """
      |Testing
      |-------
      |
      |This is a little *test* document with some **boring** text.
      |It should _not_ be taken __seriously__.
      |""".stripMargin

  pprintln(parseDocumentContentWithRefs(input))

//  pprintln(parseDocumentContentWithRefs("#\n## \n"))
