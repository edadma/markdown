package ca.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Lists extends FreeSpec with PropertyChecks with Matchers
{
	"unordered" in
	{
		Markdown( "* one" ) shouldBe "<ul><li>one</li></ul>"
	}
}