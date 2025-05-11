package io.github.edadma.markdown

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class ThematicBreakBlockParserTest extends AnyFunSuite with Matchers {

  // According to CommonMark, a thematic break is a line containing
  // 3+ of *, -, or _ (optionally separated by spaces),
  // with no other characters, and preceded/followed by blank line.
  private val tests = Table(
    // input                                 // expected blocks
    ("input", "expected"),
    ("---\n", List(ThematicBreak())),                  // simple hyphens
    ("***\n", List(ThematicBreak())),                  // simple asterisks
    ("___\n", List(ThematicBreak())),                  // simple underscores
    ("- - -\n", List(ThematicBreak())),                // spaced hyphens
    ("* * * *\n", List(ThematicBreak())),              // spaced asterisks
    ("_ _ _ _ _\n", List(ThematicBreak())),            // spaced underscores
    ("----\n", List(ThematicBreak())),                 // 4 hyphens still ok
    ("*** * ***\n", List(ThematicBreak())),            // mixed but all asterisks
    ("-*-\n", List(Paragraph(List(Text("-*-"))))),     // contains non-delimiter
    ("--- a\n", List(Paragraph(List(Text("--- a"))))), // extra text
    (" --\n", List(Paragraph(List(Text("--"))))),      // too few hyphens
    (
      "*_*_*_\n",
      List(Paragraph(List(Emphasis(List(Text("_"))), Emphasis(List(Text("*")))))),
    ), // underscores next to asterisks
  )

  forAll(tests) { (input, expected) =>
    test(s"parsing `${input.trim}` → $expected") {
      // feed through our parser
      val reader   = new InputReader(input)
      val (doc, _) = parseDocument(reader.getStreamWithExpandedTabs())
      doc.children shouldBe expected
    }
  }
}
