package ca.hyperreal.markdown


object MarkdownTest extends App
{
	val s =
"""
a
- - -
two
"""
	
	println( Markdown(s) )
//	println( headings )
}