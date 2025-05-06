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

//@main
def tools(args: String*): Unit =
  val json                    = readFile("emoji.json")
  val emojis: List[EmojiJson] = json.fromJson[List[EmojiJson]].getOrElse(sys.error("error parsing emojis"))
  val buf                     = new StringBuilder
  val maxSize                 = 300

  buf ++=
    """package io.github.edadma.markdown
      |
      |import scala.collection.mutable
      |
      |""".stripMargin

  val emojiList =
    emojis flatMap {
      case EmojiJson(emoji, description, _, aliases, _, _, _) =>
        val desc = description.replace(":", "")
        val descList =
          if desc.contains(" ") then List(desc.replace(' ', '_'), desc.replace(' ', '-'), desc)
          else List(desc)
        val aliasList = aliases filterNot (a => descList contains a)

        (descList ++ aliasList) map (a => a -> emoji)
    }

  val emojiBlocks = emojiList.grouped(maxSize).toList

  emojiBlocks.zipWithIndex foreach { (block, idx) => generateBlock(block, idx + 1) }

  buf ++=
    s"""val emojis =
       |  val map = new mutable.HashMap[String, String]
       |  
       |  map ++=
       |""".stripMargin
  buf ++= (emojiBlocks.indices map (i => s"  emojiBlock${i + 1}") mkString " ++\n")
  buf ++= "\n"
  buf ++= "  map.toMap\n"
  writeFile("shared/src/main/scala/io/github/edadma/markdown/emojis.scala", buf.toString)

  def generateBlock(block: List[(String, String)], num: Int): Unit =
    val buf = new StringBuilder

    buf ++=
      """package io.github.edadma.markdown
        |
        |""".stripMargin
    buf ++= s"val emojiBlock$num = List(\n"
    block foreach {
      case (desc, emoji) =>
        buf ++= s"  \"$desc\" -> \"$emoji\",\n"
    }
    buf ++= ")\n"
    writeFile(s"shared/src/main/scala/io/github/edadma/markdown/emojiBlock$num.scala", buf.toString)
