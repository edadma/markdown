package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
+	this is a list item
	indented with tabs

+   this is a list item
    indented with spaces

Code:

	this code block is indented by one tab

And:

		this code block is indented by two tabs

And:

	+	this is an example list item
		indented with tabs
	
	+   this is an example list item
	    indented with spaces
"""

	println( GFM(s) )
//	println( headings )
}