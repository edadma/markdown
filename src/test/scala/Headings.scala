package xyz.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Headings extends FreeSpec with PropertyChecks with Matchers with Tests
{
	"underline" in
	{
		markdown( """
			|Heading Text
			|============
			|text
			|""".stripMargin ) shouldBe """<h1 id="Heading_Text">Heading Text</h1><p>text</p>"""
		markdown( """
			|Heading Text
			|============
			|
			|text
			|""".stripMargin ) shouldBe """<h1 id="Heading_Text">Heading Text</h1><p>text</p>"""
		markdown( """
			|Heading Text
			|------------
			|text
			|""".stripMargin ) shouldBe """<h2 id="Heading_Text">Heading Text</h2><p>text</p>"""
		markdown( """
			|Heading Text
			|------------
			|
			|text
			|""".stripMargin ) shouldBe """<h2 id="Heading_Text">Heading Text</h2><p>text</p>"""
	}
	
	"hash" in
	{
		markdown( """
			|#Heading Text
			|text
			|""".stripMargin ) shouldBe """<h1 id="Heading_Text">Heading Text</h1><p>text</p>"""
		markdown( """
			|#Heading Text
			|
			|text
			|""".stripMargin ) shouldBe """<h1 id="Heading_Text">Heading Text</h1><p>text</p>"""
		markdown( """
			|##Heading Text
			|text
			|""".stripMargin ) shouldBe """<h2 id="Heading_Text">Heading Text</h2><p>text</p>"""
		markdown( """
			|##Heading Text
			|
			|text
			|""".stripMargin ) shouldBe """<h2 id="Heading_Text">Heading Text</h2><p>text</p>"""
	}
}