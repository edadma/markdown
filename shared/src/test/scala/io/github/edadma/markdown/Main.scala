package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main def run(): Unit =
//  logger.setLogLevel(LogLevel.DEBUG)
//  logger.setFileLogging()

//  val linkRefs = Map("ref" -> LinkReference("image.jpg", None))
//  val input    = "Shortcut image: ![ref]"
//
//  pprintln(parseInlineContent(input, linkRefs))

//  pprintln(parseInlineContent("""[![alt text](image.jpg)](https://example.com)"""))

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
//
//  println(renderToHTML(input))

//  val input =
//    """
//      |<div>
//      |Hello
//      |</div>
//      |
//      |This is a paragraph.
//      |""".stripMargin
//  val (doc, refs) = parseDocumentContentWithRefs(input)

  val input =
    """
      || Column 1 | Column 2 | Column 3 |
      ||:---------|:--------:|---------:|
      || Left     | Center   | Right    |
      || Cell 1   | Cell 2   | Cell 3   |
      || *Italic* | **Bold** | `Code`   |""".stripMargin
  val (doc, refs) = parseDocumentContentWithRefs(input)

  pprintln(doc)
  println(renderToHTML(doc))

//  pprintln(extractHeaders(doc))

//  pprintln(parseDocumentContent("	This is indented with a tab."))
