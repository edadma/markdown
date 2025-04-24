package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
//  logger.setLogLevel(LogLevel.DEBUG)
//  logger.setFileLogging()

//  pprintln(parseInlineContent("Not autolinks: < https://foo.bar> <foo bar> <https://example.com space>"))
//  println(isHtmlTag("<foo bar>"))

  val input =
    """
      |[fo\\o]: </url> "ti\\tle"
      |[bar]: /url2 'title2'
      |[baz]: /url3 (title3)
      |[no_title]: /url4 ""
      |""".stripMargin

  pprintln(parseDocumentContentWithRefs(input))
