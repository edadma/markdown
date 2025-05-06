package io.github.edadma.markdown

import pprint.pprintln
import zio.json.*
import io.github.edadma.cross_platform.{readFile, writeFile}

case class EmojiJson(
    emoji: String,
    description: String,
    category: String,
    aliases: List[String],
    tags: List[String],
    unicode_version: String,
    ios_version: String,
) derives JsonDecoder

@main def tools(args: String*): Unit =
  val emojis = readFile("emoji.json").fromJson[List[EmojiJson]].getOrElse(sys.error("error parsing emojis"))
  val buf    = new StringBuilder

  buf ++=
    """package io.github.edadma.markdown
      |
      |val emojis = Map(
      |""".stripMargin

  emojis foreach {
    case EmojiJson(emoji, description, _, aliases, _, _, _) =>
      buf ++= s"""  "$description" -> "$emoji",\n"""
      buf ++= s"""  "${description.replace(' ', '_')}" -> "$emoji",\n"""
      buf ++= s"""  "${description.replace(' ', '-')}" -> "$emoji",\n"""
      aliases foreach (a => buf ++= s"""  "$a" -> "$emoji",\n""")
  }

  buf ++= ")\n"
  writeFile("shared/src/main/scala/io/github/edadma/markdown/emojis.scala", buf.toString)
