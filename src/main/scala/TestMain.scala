package ca.hyperreal.__markdown__


object TestMain extends App
{
	val s =
"""
***This is strong and em.***

So is ***this*** word.

___This is strong and em.___

So is ___this___ word.
"""

	println( GFM.asXML(s) )
}