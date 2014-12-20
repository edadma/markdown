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

	println( GFM(s) )
//	println( headings )
}