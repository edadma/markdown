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
  val json                    = readFile("emoji.json")
  val emojis: List[EmojiJson] = json.fromJson[List[EmojiJson]].getOrElse(sys.error("error parsing emojis"))
  val buf                     = new StringBuilder

  buf ++=
    """package io.github.edadma.markdown
      |
      |""".stripMargin

  val emojiList =
    emojis flatMap {
      case EmojiJson(emoji, description, _, aliases, _, _, _) =>
        val altList =
          if description.contains(" ") then
            List(
              description.replace(' ', '_') -> emoji,
              description.replace(' ', '-') -> emoji,
            )
          else Nil
        val aliasList = aliases map (a => a -> emoji)

        List(description -> emoji) ++ alitList ++ aliasList
    }

  val emojiBlocks = emojiList.grouped(maxSize).toList

  emojiBlocks.zipWithIndex foreach { (block, idx) =>
    buf ++= s"val $idx = List(\n"
    block foreach {
      case (desc, emoji) =>
        buf ++= s"  \"$desc\" -> \"$emoji\",\n"
    }
    buf ++= ")\n"
  }

  writeFile("shared/src/main/scala/io/github/edadma/markdown/emojis.scala", buf.toString)
