package io.github.edadma.markdown

import pprint.pprintln

@main def run(): Unit =
  val input  = "test"
  val reader = new InputReader(input)

  pprintln(reader.stream)
