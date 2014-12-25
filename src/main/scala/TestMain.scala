package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
Foo [bar][].

Foo [bar](/url/ "Title with "quotes" inside").


  [bar]: /url/ "Title with "quotes" inside"
"""

	println( GFM.asXML(s) )
}