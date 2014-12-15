package ca.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Inline extends FreeSpec with PropertyChecks with Matchers
{
	"autolink" in
	{
		Markdown( "visit http://asdf.com for info" ) shouldBe """<p>visit <a href="http://asdf.com">http://asdf.com</a> for info</p>"""
		Markdown( "visit http://asdf.com:8000 for info" ) shouldBe """<p>visit <a href="http://asdf.com:8000">http://asdf.com:8000</a> for info</p>"""
		Markdown( "visit http://asdf.com:8000/ for info" ) shouldBe """<p>visit <a href="http://asdf.com:8000/">http://asdf.com:8000/</a> for info</p>"""
		Markdown( "visit http://asdf.com:8000/this/is/the/path for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com:8000/this/is/the/path">http://asdf.com:8000/this/is/the/path</a> for info</p>"""
		Markdown( "visit http://asdf.com:8000/another/path/ for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com:8000/another/path/">http://asdf.com:8000/another/path/</a> for info</p>"""
		Markdown( "visit http://asdf.com/ for info" ) shouldBe """<p>visit <a href="http://asdf.com/">http://asdf.com/</a> for info</p>"""
		Markdown( "visit http://asdf.com/this/is/the/path for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com/this/is/the/path">http://asdf.com/this/is/the/path</a> for info</p>"""
		Markdown( "visit http://asdf.com/another/path/ for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com/another/path/">http://asdf.com/another/path/</a> for info</p>"""
	}
}