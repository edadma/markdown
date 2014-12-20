package ca.hyperreal.superMarkdown

import org.scalatest._
import prop.PropertyChecks


class Inline extends FreeSpec with PropertyChecks with Matchers
{
	"autolink" in
	{
		Markdown( "visit http://asdf for info" ) shouldBe """<p>visit http://asdf for info</p>"""
		Markdown( "visit <http://asdf.com> for info" ) shouldBe """<p>visit <a href="http://asdf.com">http://asdf.com</a> for info</p>"""
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
		Markdown( "visit http://asdf?key1=value1&key2=value2 for info" ) shouldBe """<p>visit http://asdf?key1=value1&amp;key2=value2 for info</p>"""
	}
	
	"escapes" in
	{
		Markdown( """\\ is a backslash""" ) shouldBe """<p>\ is a backslash</p>"""
		Markdown( """\` is a backtick""" ) shouldBe """<p>` is a backtick</p>"""
		Markdown( """\* is an asterisk""" ) shouldBe """<p>* is an asterisk</p>"""
		Markdown( """\_ is an underscore""" ) shouldBe """<p>_ is an underscore</p>"""
	}
}