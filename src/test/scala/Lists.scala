package xyz.hyperreal.__markdown__

import org.scalatest._
import prop.PropertyChecks


class Lists extends FreeSpec with PropertyChecks with Matchers
{
	"unordered" in
	{
		Markdown( "* one" ) shouldBe "<ul><li>one</li></ul>"
		Markdown( """
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
		Markdown( """
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