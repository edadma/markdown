package ca.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Headings extends FreeSpec with PropertyChecks with Matchers
{
	"underline" in
	{
		Markdown( """
			|Heading Text
			|============
			|text
			|""".stripMargin ) shouldBe "<h1>Heading Text</h1><p>text</p>"
		Markdown( """
			|Heading Text
			|============
			|
			|text
			|""".stripMargin ) shouldBe "<h1>Heading Text</h1><p>text</p>"
		Markdown( """
			|Heading Text
			|------------
			|text
			|""".stripMargin ) shouldBe "<h2>Heading Text</h2><p>text</p>"
		Markdown( """
			|Heading Text
			|------------
			|
			|text
			|""".stripMargin ) shouldBe "<h2>Heading Text</h2><p>text</p>"
	}
}