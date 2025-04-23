package io.github.edadma.markdown

import io.github.edadma.dllist.DLList
import pprint.pprintln
import io.github.edadma.logger.{FileHandler, LogLevel}

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  pprintln(parseInlineContent("![alt text](image.jpg)"))

  val dllist = DLList[Inline](C('a'), C('b'), C('c'))

  consolidateCharacters(dllist)
  pprintln(dllist)
