package ca.hyperreal.__markdown__

import scala.util.parsing.combinator.lexical._
import collection.mutable.{Buffer, ListBuffer, HashMap}
import scala.util.Try


class Markdown( features: String* ) extends RegexParsers
{
	private val refmap = new HashMap[String, (String, Option[String])]
	private var pass2 = false
	private val buf = new StringBuilder
	private var featureNewlineBreak = false
	private var featureBackslashBreak = false
	private var tabSize = 4
	
	for (f <- features)
		f.toLowerCase match
		{
			case "newline-break" => featureNewlineBreak = true
			case "backslash-break" => featureBackslashBreak = true
			case "gfm" =>
				featureNewlineBreak = true
			case "commonmark" =>
				featureBackslashBreak = true
			case Markdown.TABSIZE_REGEX( s ) =>
				tabSize = s.toInt
				
				if (tabSize > 0)
					sys.error( "tab-size should be positive: " + tabSize )
			case _ => sys.error( "unrecognized feature: " + f )
		}
}

object Markdown
{
	private val TABSIZE_REGEX = "tab-size *= *([0-9]+)"r
	
	private def stripReturns( s: String ) = s.replace( "\r\n", "\n" ).replace( "\r", "\n" )

	private def tabs2spaces( s: String, size: Int ) =
	{
		require( size > 0 )
		
	val lines = s.split( "\n", -1 )
	
		for (i <- 0 until lines.length)
		{
		var index = 0
		
			while ({index = lines(i).indexOf( '\t', index ); index > -1})
			{
			val pad = size - (index % size)
			
				lines(i) = lines(i).substring( 0, index ) + " "*pad + lines(i).substring( index + 1 )
				index += pad - 1
			}
		}
		
		lines.reduce( _ + "\n" + _ )
	}

// 	def asXML( s: String, features: String* ) = (new Markdown( features: _* )).parseDocument( s )
// 	
// 	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}

object GFM
{
// 	def asXML( s: String, features: String* ) = (new Markdown( ("gfm" +: features): _* )).parseDocument( s )
// 	
// 	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}