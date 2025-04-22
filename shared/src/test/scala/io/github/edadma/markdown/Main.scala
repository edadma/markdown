package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.{FileHandler, LogLevel}

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

  pprintln(parseInlineContent("![alt text](image.jpg)"))
