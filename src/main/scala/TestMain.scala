package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""`` asdf zx`cv ``

asdf"""

	println( Markdown(s) )
//	println( headings )
}