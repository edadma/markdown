package xyz.hyperreal.__markdown__

import java.io._


object TestMain extends App
{
  println( Markdown(io.Source.fromFile("test.md").mkString) )
//  val w = new FileWriter( "test.html" )
//
//  w write Markdown( io.Source.fromFile("test.md").mkString )
//  w.close

  //	println( Markdown(
  //"""
  //_*asdf_
  //"""
  //	) )
}