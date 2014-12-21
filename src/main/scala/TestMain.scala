package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
> A list within a blockquote:
> 
> *	asterisk 1
> *	asterisk 2
> *	asterisk 3
"""

	println( GFM(s) )
//	println( headings )
}