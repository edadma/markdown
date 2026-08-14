package io.github.edadma.markdown

import scala.io.Source
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The npm distribution keeps its own copy of the version number, in
  * `npm/package.json`, and that copy is what npm consumers install. The
  * `version` value the linked `main.js` exports comes from the generated
  * `BuildVersion`, so the two can disagree — and did, sitting at 0.4.3 while
  * the library published 0.4.6 and 0.4.7. This pins them together.
  *
  * Reads a path relative to the repository root, which is where sbt runs
  * tests from (`specTool` reads `spec.json` the same way).
  */
class NpmPackageVersionTest extends AnyFlatSpec with Matchers {

  "npm/package.json" should "declare the version the build was made at" in {
    val json     = Using.resource(Source.fromFile("npm/package.json"))(_.mkString)
    val declared = """"version"\s*:\s*"([^"]+)"""".r.findFirstMatchIn(json).map(_.group(1))

    declared shouldBe Some(BuildVersion.value)
  }
}
