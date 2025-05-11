//package io.github.edadma.markdown
//
//import pprint.pprintln
//import zio.json.*
//import io.github.edadma.cross_platform.{readFile, writeFile}
//
//case class TestJson(
//    markdown: String,
//    html: String,
//    example: Int,
//    start_line: Int,
//    end_line: Int,
//    section: String,
//) derives JsonDecoder
//
//@main
//def specTool(): Unit =
//  val json                                  = readFile("spec.json")
//  val tests: List[TestJson]                 = json.fromJson[List[TestJson]].getOrElse(sys.error("error parsing emojis"))
//  val sections: Map[String, List[TestJson]] = tests.groupBy(_.section)
//  val section                               = sections("Autolinks")
//
//  genSection("Autolinks")
//
//def genSection(sections: Map[String, List[TestJson]], section: String): Unit =
//  val buf   = new StringBuilder
//  val tests = sections(section)
//  val name  = section.replace(" ", "_")
//
//  buf ++=
//    s"""package io.github.edadma.markdown
//      |
//      |import org.scalatest.freespec.AnyFreeSpec
//      |import org.scalatest.matchers.should.Matchers
//      |
//      |class Spec_${name}_Test extends AnyFreeSpec with Matchers {
//      |""".stripMargin
//
//  tests.foreach {
//    case TestJson(markdown, html, example, start_line, end_line, _) =>
//      buf ++=
//  }
//
////  val buf                    = new StringBuilder
////
////  buf ++=
////    """package io.github.edadma.markdown
////      |
////      |import scala.collection.mutable
////      |
////      |""".stripMargin
////
////  val emojiList =
////    emojis flatMap {
////      case EmojiJson(emoji, description, _, aliases, _, _, _) =>
////        val desc = description.replace(":", "")
////        val descList =
////          if desc.contains(" ") then List(desc.replace(' ', '_'), desc.replace(' ', '-'), desc)
////          else List(desc)
////        val aliasList = aliases filterNot (a => descList contains a)
////
////        (descList ++ aliasList) map (a => a -> emoji)
////    }
////
////  val emojiBlocks = emojiList.grouped(maxSize).toList
////
////  emojiBlocks.zipWithIndex foreach { (block, idx) => generateBlock(block, idx + 1) }
////
////  buf ++=
////    s"""val emojis =
////       |  val map = new mutable.HashMap[String, String]
////       |
////       |  map ++=
////       |""".stripMargin
////  buf ++= (emojiBlocks.indices map (i => s"  emojiBlock${i + 1}") mkString " ++\n")
////  buf ++= "\n"
////  buf ++= "  map.toMap\n"
////  writeFile("shared/src/main/scala/io/github/edadma/markdown/emojis.scala", buf.toString)
////
////  def generateBlock(block: List[(String, String)], num: Int): Unit =
////    val buf = new StringBuilder
////
////    buf ++=
////      """package io.github.edadma.markdown
////        |
////        |""".stripMargin
////    buf ++= s"val emojiBlock$num = List(\n"
////    block foreach {
////      case (desc, emoji) =>
////        buf ++= s"  \"$desc\" -> \"$emoji\",\n"
////    }
////    buf ++= ")\n"
////    writeFile(s"shared/src/main/scala/io/github/edadma/markdown/emojiBlock$num.scala", buf.toString)
