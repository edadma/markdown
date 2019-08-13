package xyz.hyperreal.markdown

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Lists extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Tests
{
	"unordered" in
	{
		markdown( "* one" ) shouldBe "<ul><li>one</li></ul>"
		markdown( """
			|*   A list item.
			|
			|    With multiple paragraphs.
			|
			|*   Bar
			|""".stripMargin
		) shouldBe "<ul><li>A list item.<p>With multiple paragraphs.</p></li><li>Bar</li></ul>"
	}
	
	"mixed" in
	{
		markdown( """
			|*   Abacus
			|    * answer
			|*   Bubbles
			|    1.  bunk
			|    2.  bupkis
			|        * BELITTLER
			|    3. burper
			|*   Cunning
			|""".stripMargin
		) shouldBe "<ul><li>Abacus<ul><li>answer</li></ul></li><li>Bubbles<ol><li>bunk</li><li>bupkis<ul><li>BELITTLER</li></ul></li><li>burper</li></ol></li><li>Cunning</li></ul>"
	}
}