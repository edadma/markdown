package ca.hyperreal.superMarkdown


object MarkdownTest extends App
{
	val s =
"""
asdf`code`zxcv
"""

	println( Markdown(s) )
//	println( headings )
}