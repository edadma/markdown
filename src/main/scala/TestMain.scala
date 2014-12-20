package ca.hyperreal.markdown


object MarkdownTest extends App
{
	val s =
"""
asdf
- - -
zxcv
"""

	println( Markdown(s) )
//	println( headings )
}