package ca.hyperreal.markdown


object MarkdownTest extends App
{
	val s =
"""
~~as*df*~~
"""

	println( Markdown(s) )
//	println( headings )
}