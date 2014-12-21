package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
Just a [URL](/url/).

[URL and title](/url/ "title").

[URL and title](/url/  "title preceded by two spaces").

[URL and title](/url/	"title preceded by a tab").

[Empty]().
"""

	println( Markdown(s) )
}