package xyz.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Inline extends FreeSpec with PropertyChecks with Matchers with Tests
{
	"autolink" in
	{
		markdown( "visit http://asdf for info" ) shouldBe """<p>visit http://asdf for info</p>"""
		markdown( "visit <http://asdf.com> for info" ) shouldBe """<p>visit <a href="http://asdf.com">http://asdf.com</a> for info</p>"""
		markdown( "visit http://asdf.com for info" ) shouldBe """<p>visit <a href="http://asdf.com">http://asdf.com</a> for info</p>"""
		markdown( "visit http://asdf.com:8000 for info" ) shouldBe """<p>visit <a href="http://asdf.com:8000">http://asdf.com:8000</a> for info</p>"""
		markdown( "visit http://asdf.com:8000/ for info" ) shouldBe """<p>visit <a href="http://asdf.com:8000/">http://asdf.com:8000/</a> for info</p>"""
		markdown( "visit http://asdf.com:8000/this/is/the/path for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com:8000/this/is/the/path">http://asdf.com:8000/this/is/the/path</a> for info</p>"""
		markdown( "visit http://asdf.com:8000/another/path/ for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com:8000/another/path/">http://asdf.com:8000/another/path/</a> for info</p>"""
		markdown( "visit http://asdf.com/ for info" ) shouldBe """<p>visit <a href="http://asdf.com/">http://asdf.com/</a> for info</p>"""
		markdown( "visit http://asdf.com/this/is/the/path for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com/this/is/the/path">http://asdf.com/this/is/the/path</a> for info</p>"""
		markdown( "visit http://asdf.com/another/path/ for info" ) shouldBe
			"""<p>visit <a href="http://asdf.com/another/path/">http://asdf.com/another/path/</a> for info</p>"""
		markdown( "visit http://asdf?key1=value1&key2=value2 for info" ) shouldBe """<p>visit http://asdf?key1=value1&amp;key2=value2 for info</p>"""
	}
	
	"escapes" in
	{
		markdown( """\\ is a backslash""" ) shouldBe """<p>\ is a backslash</p>"""
		markdown( """\` is a backtick""" ) shouldBe """<p>` is a backtick</p>"""
		markdown( """\* is an asterisk""" ) shouldBe """<p>* is an asterisk</p>"""
		markdown( """\_ is an underscore""" ) shouldBe """<p>_ is an underscore</p>"""
	}
}