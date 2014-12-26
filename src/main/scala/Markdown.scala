package ca.hyperreal.__markdown__

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input._
import CharArrayReader.EofCh
import collection.mutable.{Buffer, ArrayBuffer, HashMap}
import scala.util.Try


class Markdown( features: String* ) extends StdLexical
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
			case Markdown.TABSIZE( s ) =>
				tabSize = s.toInt
				
				if (tabSize > 0)
					sys.error( "tab-size should be positive: " + tabSize )
			case _ => sys.error( "unrecognized feature: " + f )
		}
	
	def text( c: Char ) =
		c ^^^
		(Text( c.toString ))
		
	def plain =
		rep1(chrExcept('*', '_')) ^^
			{case cs => Text( cs.mkString )}
		
	def inline_no_em( allow: Char ) =
		rep(plain | text(allow))
	
	def emphasis: Parser[Document] =
		'*' ~> not('*' ~ not('*')) ~> guard(rep1(chrExcept('*')) ~ '*') ~> inline_no_em( '_' ) <~ not('*' ~ '*') <~ '*' ^^// ~> guard(rep1(chrExcept('*')) ~ '*')
			(Emphasis( _ )) |
		'_' ~> not('_' ~ not('_')) ~> inline_no_em( '*' ) <~ '_' <~ not('_' ~ not('_')) ^^
			(Emphasis( _ )) |
		'*' ~> not('*')^^^
			Text( "*" ) |
		'_' ^^^
			Text( "_" )

	def strong =
// 		'$'  ^^^ Text("$")
		'*' ~> '*' ~> inline_no_strong <~ '*' <~ '*' ^^
			(Strong( _ )) |
		'*' ~ '*' ^^^
			Text( "**" )
	
	def inline_no_strong =
		rep(emphasis | plain)
		
	def inline = rep(strong | emphasis | plain)
	
	def n = elem('\n')
	
	def s = elem(' ')
	
	def t = elem('\t')
	
	def z = elem(EofCh)
	
	def paragraph_sep =
		n ~ not(n | z)
		
	def line =
		(s ~ s ~ s | s ~ s | s | success("")) ~> rep1(chrExcept('\n', EofCh)) ^^
			(cs => cs.mkString)
			
	def paragraph =
		rep1sep(line, paragraph_sep) ^^
			(p =>
				Paragraph( parseRule(inline, p.reduce(_ + "\n" + _)) )
			)
	
	def blank =
		(n ~ rep(rep(s) ~ n))|rep(n) ~ z
		
	def block = (paragraph) <~ blank
	
	def space =
		rep(s | t | n)
		
	def document =
		space ~> rep(block) <~ space ^^
			(Group( _ ))
	
	protected def parseRule[T]( rule: Parser[T], s: String ) =
	{
		phrase( rule )( new CharSequenceReader(s) ) match
		{
			case Success( result, _ ) => result
			case Failure( msg, rest ) => sys.error( msg + " at " + rest.pos + "\n" + rest.pos.longString )
			case Error( msg, _ ) => sys.error( msg )
		}
	}
	
	def parseDocument( s: String ) =
		parseRule( document, Markdown.stripReturns( s ) )
}

object Markdown
{
	private val TABSIZE = "tab-size *= *([0-9]+)"r
	
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

	def html( d: Document ) =
	{
	val tab = 2
	val buf = new StringBuilder
	var level = -2
	
		def indent = buf append " "*(tab*level)

		def _html( s: Document )
		{
			level += 1
			
			s match
			{
				case Group( l ) =>
					for (e <- l)
						_html( e )
				case Paragraph( l ) =>
					indent
					buf append "<p>"
					
					for (e <- l)
						_html( e )
						
					buf append "</p>\n"
				case Emphasis( l ) =>
					buf append "<em>"
					
					for (e <- l)
						_html( e )
						
					buf append "</em>"
				case Strong( l ) =>
					buf append "<strong>"
					
					for (e <- l)
						_html( e )
						
					buf append "</strong>"
				case Text( t ) =>
					buf append t
			}
			
			level -= 1
		}
		
		_html( d )
		buf.toString
	}
	
 	def asDocument( s: String, features: String* ) = (new Markdown( features: _* )).parseDocument( s )
	
	def apply( s: String, features: String* ) = html( asDocument(s, features: _*) )
}

object GFM
{
	def asDocument( s: String, features: String* ) = (new Markdown( ("gfm" +: features): _* )).parseDocument( s )
	
	def apply( s: String, features: String* ) = Markdown.html( asDocument( s, features: _* ) )
}