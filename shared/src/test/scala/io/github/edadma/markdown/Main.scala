package io.github.edadma.markdown

import io.github.edadma.dllist.DLList
import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  pprintln(parseInlineContent("![alt text](image.jpg)"))

  val dllist = DLList[Inline](Cursor('a', 0, 0, 0, false), Cursor('b', 0, 0, 0, false), Cursor('c', 0, 0, 0, false))

  consolidateCharacters(dllist)
  pprintln(dllist)
