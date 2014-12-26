package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
Paragraph one.

<!-- This is a simple comment -->

<!--
    This is another comment.
-->

Paragraph two.

<!-- one comment block -- -- with two comments -->

The end.
"""

	println( GFM.asXML(s) )
}