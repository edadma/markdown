package io.github.edadma.markdown

import io.github.edadma.cross_platform.processExit

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.{Dictionary, JSON}

/** A simple command-line utility to generate ScalaTest suites from CommonMark spec JSON test cases.
  *
  * Usage: sbt "runMain io.github.edadma.markdown.SpecTestGenerator spec.json > SpecTests.scala"
  */
@main def generateSpecTests(args: String*): Unit = {
//  if (args.length != 1) {
//    Console.err.println("Usage: SpecTestGenerator <spec.json>")
//    processExit(1)
//  }
//
//  val specFile = args(0)
  val specFile = "spec.json"

  // 2) read it via Node's fs
  val fs      = global.require("fs")
  val jsonStr = fs.readFileSync(specFile, "utf8").asInstanceOf[String]

  // 3) parse with the JS engine
  val testCases = JSON.parse(jsonStr).asInstanceOf[js.Array[js.Dictionary[String]]].toList map (_.toMap) take 2

  // Sanitize section names into valid Scala class names
  def sanitize(section: String): String = {
    // Replace non-alphanumeric with underscores, trim, split, capitalize
    val clean = section.replaceAll("[^A-Za-z0-9]+", "_").replaceAll("^_+|_+$", "")
    val words = clean.split("_+").filter(_.nonEmpty).map(w => w.capitalize)
    val base  = if (words.isEmpty) "Uncategorized" else words.mkString
    base + "Spec"
  }

  // Escape triple quotes inside strings
  def escape(s: String): String =
    s.replace("\"\"\"", "\\\"\"\"")

  // Group test cases by section
  val grouped: Map[String, List[Map[String, Any]]] =
    testCases.groupBy(_.getOrElse("section", "Uncategorized"))

  // Print package and imports
  println("package io.github.edadma.markdown")
  println
  println("import org.scalatest.funsuite.AnyFunSuite")
  println("import org.scalatest.matchers.should.Matchers")
  println

  // Emit one suite per section
  for ((section, cases) <- grouped) {
    val className = sanitize(section)
    println(s"class $className extends AnyFunSuite with Matchers {")
    cases.foreach { tc =>
      val md       = escape(tc("markdown").toString)
      val html     = escape(tc("html").toString)
      val ex       = tc("example").toString
      val start    = tc("start_line").toString
      val end      = tc("end_line").toString
      val testName = s"$section example $ex (lines $start-$end)"

      println(s"  test(\"$testName\") {")
      println(s"    val input = \"\"\"$md\"\"\"  ")
      println(s"    val expected = \"\"\"$html\"\"\"  ")
      println("    val output = renderMarkdown(input)")
      println("    output shouldBe expected")
      println("  }")
      println()
    }
    println("}")
    println()
  }
}
