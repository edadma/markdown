package ca.hyperreal.__markdown__

import java.io._


object TestMain extends App
{
// 	val w = new FileWriter( "test.html" )
// 	
// 	w write Markdown( io.Source.fromFile("test.text").mkString )
// 	w.close
	
	println( Markdown(
"""
*a**a*sdf

zxcv
dfgh
""") )
}