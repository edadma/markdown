package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.logger.LogLevel

@main
def run(): Unit =
  logger.setLogLevel(LogLevel.DEBUG)
  logger.setFileLogging()

//  val linkRefs = Map("ref" -> LinkReference("image.jpg", None))
//  val input    = "Shortcut image: ![ref]"
//
//  pprintln(parseInlineContent(input, linkRefs))

//  pprintln(parseInlineContent("""__zxcv *asdf*__"""))

//  val input =
//    """
//      |# _asdf_""".stripMargin
//
//  println(renderToHTML(input))

//  val input =
//    """&trade;""".stripMargin
  val input = "<https://example.com/\\[\\>\n"
  val (doc, refs) =
    parseDocumentContentWithRefs(input, MarkdownConfig.all)

//  val input =
//    """
//      |- a
//      |- """.stripMargin
//  val (doc, refs) = parseDocumentContentWithRefs(input)

//  pprintln(refs)
  pprintln(doc)
  println(renderToHTML(doc))

//  pprintln(extractHeaders(doc))

//  pprintln(parseDocumentContent("	This is indented with a tab."))
