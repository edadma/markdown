package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
asdf <a class="hg">xcvb<b>klj<c/>iyou</b>khj</a> zxcv
"""

	println( GFM(s) )
//	println( headings )
}