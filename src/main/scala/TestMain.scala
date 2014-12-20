package ca.hyperreal.__markdown__


object MarkdownTest extends App
{
	val s =
"""asdf ``Roses are red, `Violets` are blue.`` zxcv"""

	println( Markdown(s) )
//	println( headings )
}