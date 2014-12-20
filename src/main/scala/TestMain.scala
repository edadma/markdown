package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
*asdf <!-- <b>khj</b>* --> zxcv
"""

	println( GFM(s) )
//	println( headings )
}