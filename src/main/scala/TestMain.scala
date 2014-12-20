package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
asdf  
ghjk\
qwer
zxcv  

qwer  """

	println( Markdown(s, "newline-break", "backslash-break") )
//	println( headings )
}