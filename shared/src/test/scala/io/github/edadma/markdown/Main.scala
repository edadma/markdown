package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.{FileHandler, LogLevel}

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setHandler(new FileHandler("log"))

  val input =
    "*foo*"
//    """l` ine ` 1
//      |line 2
//      |""".stripMargin
//  val reader = new InputReader(input)
//  val ast    = parseDocument(reader.stream)
  val ast = parseInlineContent(input)

  pprintln(ast)
