package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
Here's a [link] [1] with an ampersand in the URL.
[1]: http://example.com/?foo=1&bar=2
"""

	println( GFM(s) )
//	println( headings )
}