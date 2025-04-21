package io.github.edadma.markdown

import pprint.pprintln

@main def run(): Unit =
  val input =
    """line 1
      |
      |line 2
      |""".stripMargin
  val reader = new InputReader(input)
  val ast    = parseDocument(reader.stream)

  pprintln(ast)
