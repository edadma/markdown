package io.github.edadma.markdown

import pprint.pprintln
import io.github.edadma.cross_platform.{readFile, writeFile}

//@main
def entityTool(): Unit =
  val json = readFile("entities.json")
  val entityPattern =
    """"&([^;:]+);"\s*:\s*\{\s*"codepoints"\s*:\s*\[[^]]+]\s*,\s*"characters"\s*:\s*"(\\u[^"]+)"\s*}""".r
  val entities = entityPattern.findAllMatchIn(json).map(m => (m.group(1), m.group(2))).toList.grouped(300).toList

  entities.zipWithIndex.foreach { (es, idx) =>
    val buf  = new StringBuilder
    val file = idx + 1

    buf ++=
      s"""package io.github.edadma.markdown
        |
        |val entities$file = Map[String, String](
        |""".stripMargin
    es.foreach { (name, chars) => buf ++= s"""  "$name" -> "$chars",\n""" }
    buf ++= ")\n"
    writeFile(s"shared/src/main/scala/io/github/edadma/markdown/entities$file.scala", buf.toString)
  }
