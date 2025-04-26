package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  val linkRefs = Map("ref" -> LinkReference("http://example.com", None))
//  val input    = "This is a [ref]."
//
//  pprintln(parseInlineContent(input, linkRefs))

  pprintln(parseInlineContent("""[![alt text](image.jpg)](https://example.com)"""))

//  val input =
//    """
//      |Overview
//      |========
//      |
//      |This is a little *test* document with some **boring** text.
//      |
//      |Advice
//      |------
//      |
//      |It should _not_ be taken __seriously__.
//      |""".stripMargin

//  val input =
//    """
//      |#
//      |""".stripMargin
//  val (doc, refs) = parseDocumentContentWithRefs(input)
//
//  pprintln(doc)
//  pprintln(extractHeaders(doc))

//  pprintln(parseDocumentContent("asdf"))
