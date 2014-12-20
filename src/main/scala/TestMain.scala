package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
asdf <a>xcvb<b>klj<c/>iyou</b>khj</a> zxcv
"""

	println( GFM(s) )
//	println( headings )
}