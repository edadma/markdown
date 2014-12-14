package ca.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Inline extends FreeSpec with PropertyChecks with Matchers
{
	"autolink" in
	{
		Markdown( "visit http://asdf.com for info" ) shouldBe """<p>visit <a href="http://asdf.com">http://asdf.com</a> for info</p>"""
	}
}