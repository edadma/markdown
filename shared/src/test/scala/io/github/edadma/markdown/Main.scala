package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  pprintln(parseInlineContent("***triple emphasis***"))

  val input =
    """
      |##    Heading with more spaces   
      |""".stripMargin

  pprintln(parseDocumentContentWithRefs(input))
