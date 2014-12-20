package ca.hyperreal.markdown

import util.parsing.combinator._
import util.matching.Regex
import xml.{Elem, Node, Text, Group}
import collection.mutable.{Buffer, ListBuffer, HashMap}


case class Heading( level: Int, heading: Node )

class Markdown( headings: Buffer[Heading] ) extends RegexParsers
{
	private class Holder( var addr: String, var title: Option[String] )
	
	private val refmap = new HashMap[String, Holder]

	override def skipWhitespace = false

	def plain = text( """[^\n~*_`\\\[! \t]+"""r )

	def space = text( """[ \t]+"""r, _ => " " )
	
	def underscore_word = text( """[a-zA-Z0-9]_+[a-zA-Z0-9]"""r )

	def domain = """[a-zA-Z0-9-]+\.""".r ~ rep1sep("[a-zA-Z0-9-]+"r, ".") ^^ {case host ~ root => host + root.mkString( "." )}
	
	def query_word = """[a-zA-Z0-9-*._]*"""r
	
	def query = "?" ~ rep1sep(query_word ~ "=" ~ query_word ^^ {case k ~ e ~ v => k + e + v}, "&") ^^
		{case q ~ kvs => q + kvs.mkString( "&" )}

	def url = "(http|https|ftp|file)://".r ~ domain ~ opt(":[0-9]+"r) ~
		opt("/" ~ opt(rep1sep("""(?:[-A-Za-z0-9._~!$&'()*+,;=:@]|%\p{XDigit}\p{XDigit})+"""r, "/") ~ opt("/"))) ~ opt(query) ^^
		{case s ~ d ~ port ~ path ~ q =>
			s + d + port.getOrElse( "" ) +
				path.map( {case s ~ p => s + p.map({case p ~ s => p.mkString("/") + s.getOrElse("")}).getOrElse("")} ).getOrElse( "" ) +
				q.getOrElse( "" )
		}

	def local = rep1sep("""[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+"""r, ".") ^^ (_.mkString( "." ))
	
	def email = local ~ "@" ~ domain ^^ {case l ~ a ~ d => l + a + d}
	
	def autolink =
		(url | "<" ~> url <~ ">") ^^
			{case link => <a href={link}>{link}</a>} |
		(email | "<" ~> email <~ ">") ^^
			{case link => <a href={"mailto:" + link}>{link}</a>}
	
	def text( p: Parser[String], f: String => String = x => x ) = p ^^ {case t => Text( f(t) )}

	def escaped = text( """\\[-\\`\*_\{}\[\]()#\+.!]"""r, s => s.charAt(1).toString )

	def eol = """\n(?![ \t]*\n)""".r
	
	def code_text = text( """[^\n`]+"""r )

//	def code = "`" ~> ((rep1sep(code_text, eol) <~ "`".r ^^ {case es => <code>{concat(es, Text("\n"))}</code>}) | success(Text( "`" )))
	
	def code = "`" ~> (text( """[^`]+"""r ) <~ "`".r ^^ {case c => <code>{c}</code>} | success(Text( "`" )))

	def double_code_text = text( """(?:[^\n]*(?=` ``|\n)`|[^\n]+(?=``|\n))"""r )

	def double_code = ("`` " ~ guard("`[^`]"r) | "``") ~> ((rep1sep(double_code_text, eol) <~ " ?``".r ^^ {case es => <code>{concat(es, Text("\n"))}</code>}) | success(Text( "``" )))
	
	def link_text = text( """[^\n*_`\\\]!]+"""r )
	
	def link_inline: Parser[Node] = rep1(escaped | strong | em | double_code | code | image | link_text) ^^ {case l => Group( l )}
	
	private def ref( id: String ) =
		if (refmap contains id)
			refmap(id)
		else
		{
		val h1 = new Holder( null, null )

			refmap(id) = h1
			h1
		}

	def link = "[" ~> ((link_inline ~ """][ \t]*\(""".r ~ """[^ \t)]+""".r ~ """[ \t]*""".r ~ opt("\"" ~> """[^"\n]+""".r <~ "\"") <~ """[ ]*\)""".r ^^
		{case text ~ _ ~ addr ~ _ ~ title =>
			if (title == None)
				<a href={addr}>{text}</a>
			else
				<a href={addr} title={title.get}>{text}</a>
		}) |
		(link_inline ~ """][ \t]*\[""".r ~ """[^ \t\]]+""".r <~ "]" ^^
		{case text ~ _ ~ id =>
			val h = ref( id )

			if (h.title == None)
				<a href={h.addr}>{text}</a>
			else
				<a href={h.addr} title={h.title.get}>{text}</a>
		}) | success(Text( "[" )))
	
	def image = "!" ~> ("[" ~> (opt(link_inline) ~ """][ \t]*\(""".r ~ """[^ )]+""".r ~ "[ ]*".r ~ opt("\"" ~> """[^"\n]+""".r <~ "\"") <~ """[ ]*\)""".r ^^
		{case alt ~ _ ~ addr ~ _ ~ title =>
			val a = alt getOrElse Text( "" )
			
			if (title == None)
				<img src={addr} alt={a} />
			else
				<img src={addr} title={title.get} alt={a} />
		}) |
		"[" ~> (opt(link_inline) ~ """][ \t]*\[""".r ~ """[^ \t\]]*""".r <~ "]" ^^
		{case alt ~ _ ~ id =>
			val a = alt getOrElse Text( "" )
			val h = ref( id )

			if (h.title == None)
				<img src={h.addr} alt={a} />
			else
				<img src={h.addr} title={h.title.get} alt={a} />
		}) | success(Text( "!" )))
	
	def inline_element: Parser[Node] = escaped | strong | em | inline_no_em_no_strong
	
	def inline_list: Parser[List[Node]] = rep1(inline_element)
	
	def inline: Parser[Node] = inline_list ^^ (Group( _ ))

	def strikethrough_inline: Parser[Node] = rep1(space_delim( "~~" ) | escaped | strong | em | double_code | code | image | link | underscore_word | autolink |
		space | plain) ^^ (Group( _ ))
	
	def strikethrough = "~~" ~> (not(space) ~> strikethrough_inline <~ "~~" ^^ {case t => <del>{t}</del>} | success(Text("~~")))

	def inline_no_em_no_strong = double_code | code | image | link | underscore_word | autolink | strikethrough | space | plain
	
	def inline_no_em_no_strong_allow( allow: String ) = inline_no_em_no_strong | text( allow )

	def space_delim( d: String ) = text( ("""[ \t]+\""" + d)r )
	
	def em_section( d: String, allow: String ) = rep1(escaped | strong_no_em | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ (Group( _ ))

	def _em( d: String, allow: String ) = d ~> not(space) ~> em_section( d, allow ) <~ not(space) <~ d ^^ {case e => <em>{e}</em>} | text( d )

	def em = _em( "*", "_" ) | _em( "_", "*" )
	
	def em_no_strong_section = rep1(escaped | inline_no_em_no_strong) ^^ (Group( _ ))

	def _em_no_strong( d: String ) =
		(d ~ not(d)) ~> ((em_no_strong_section <~ (d ~ not(d)) ^^ {case e => <em>{e}</em>}) | success(Text( d )))
	
	def em_no_strong = _em_no_strong( "*" ) | _em_no_strong( "_" )
	
	def strong_section( d: String, allow: String ) = rep1(escaped | em_no_strong | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ {case l => Group( l )}
	
	def _strong( d: String, allow: String ) = d ~> not(space) ~> strong_section( d, allow ) <~ not(space) <~ d ^^ {case e => <strong>{e}</strong>} | text( d )
	
	def strong: Parser[Node] = _strong( "**", "__" ) | _strong( "__", "**" )
	
	def strong_no_em_section = rep1(escaped | inline_no_em_no_strong) ^^ {case l => Group( l )}
	
	def _strong_no_em( d: String ) = d ~> ((strong_no_em_section <~ d ^^ {case e => <strong>{e}</strong>}) | success(Text( d )))

	def strong_no_em: Parser[Node] = _strong_no_em( "**" ) | _strong_no_em( "__" )

	val HEADING_TAIL_PATTERN = "(.*?)[ ]*#*"r

	def heading_inline = rep1(escaped | strong | em | double_code | code | space | plain) ^^
		{case l =>
			val (front, back) = l.splitAt( l.length - 1 )
			val last =
				back.head match
				{
					case Text( t ) =>
						val HEADING_TAIL_PATTERN( s ) = t
						
						Text( s )
					case item => item
				}

			Group( front :+ last )
		}

	def heading1 = """#{1,6}[ \t]*""".r ~ heading_inline ^^
		{case l ~ e =>
			val level = l.count(_ == '#')

			if (headings ne null)
				headings += Heading( level, e )

			level match
			{
				case 1 => <h1>{e}</h1>
				case 2 => <h2>{e}</h2>
				case 3 => <h3>{e}</h3>
				case 4 => <h4>{e}</h4>
				case 5 => <h5>{e}</h5>
				case 6 => <h6>{e}</h6>
			}
		}
	
	def heading2 = inline ~ """\n(?:-+|=+)[ \t]*(?=\n|\z)""".r ^^
		{case e ~ l =>
			val level = if (l.charAt(1) == '=') 1 else 2
			
			if (headings ne null)
				headings += Heading( level, e )
			
			level match
			{
				case 1 => <h1>{e}</h1>
				case 2 => <h2>{e}</h2>
			}
		}

	def comment = """[ ]{0,3}/\*[^*]*\*+(?:[^*/][^*]*\*+)*/[ \t]*""".r ^^ (_ => Text( "" ))

	def reference = 
		"""[ ]{0,3}\[""".r ~>
			"""[^\]\n]*""".r ~ "]:[ ]*".r ~ """[^ \n\t"]+""".r ~ "[ ]*".r ~ opt("\"" ~> """[^"\n]+""".r <~ "\"") <~ """[ \t]*(?=\n|\z)""".r ^^
		{case id ~ _ ~ addr ~ _ ~ title =>
			if (refmap.contains( id ))
				if (refmap(id).addr != null)
					sys.error( "duplicate reference id: " + id )
				else
				{
					refmap(id).addr = addr
					refmap(id).title = title
				}
			else
				refmap(id) = new Holder( addr, title )

			Text( "" )
		}

	def paragraph = """[ ]{0,3}""".r ~> rep1sep(inline_list, """\n(?![ \t]*\n|#|[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})|```)"""r) ^^
		{lines =>
			val p = concat(lines map (Group), <br/>)

			if (p.toString == "")
				Text( "" )
			else
				<p>{p}</p>
		}

	def preformated_prefix = """[ ]{4}|[ ]{0,3}\t"""r

	def preformated =
		preformated_prefix ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(preformated_prefix) | success("")) ^^ {case c ~ s => c + s}), preformated_prefix) ^^
			{case es =>
				<pre><code>{
					Text( es.reduce(_ + _) )
				}</code></pre>
			}

	def triple_code = ("""```[ \t]*""".r ~> "[^ \t\n]*".r <~ """[ \t]*\n""".r) ~ ("""(?:.|\n)+(?=\n```)""".r <~ "\n```") ^^
		{case l ~ c =>
			<pre><code class={l}>{
				Text( c )
			}</code></pre>
		}

	def table_plain = text( """[^\n*_`\\\[!|]+"""r )

	def table_inline: Parser[Node] = rep1(escaped | strong | em | double_code | code | image | link | autolink | space | table_plain) ^^
		{case l =>
			val (front, back) = l.splitAt( l.length - 1 )
			val last =
				back.head match
				{
					case Text( t ) => Text( t.trim )
					case item => item
				}

			Group( front :+ last )
		}

	def table_row =
		"""[ ]{0,3}(?:\|[ \t]*)?""".r ~> (table_inline <~ """\|[ \t]*""".r) ~ repsep(table_inline, """\|[ \t]*(?!\n|\z)"""r) <~ """(?:\|[ \t]*)?""".r ^^
		{case first ~ rest => first +: rest}

	def table =
		(table_row <~ """\n[ ]{0,3}\|?""".r) ~
			"""(?:[ \t]*|:)-+(?::|[ \t]*)\|(?::|[ \t]*)(?:-+(?::|[ \t]*)(?:\|(?::|[ \t]*)-+(?::|[ \t]*))*\|?(?::|[ \t]*))?""".r ~ rep(eol ~> table_row) ^^
		{
			case head ~ sep ~ body =>
				val buf = new ListBuffer[String]
				
				for (c <- """(?::|[ \t]*)-+(?::|[ \t]*)""".r findAllIn sep)
					if (c.startsWith( ":" ) && c.endsWith( ":" ))
						buf += "center"
					else if (!c.endsWith( ":" ))
						buf += "left"
					else
						buf += "right"

				val aligns = buf.toList
				
				<table><thead><tr>{
					for ((i, a) <- head zip aligns)
						yield
							if (a == "left")
								<th>{i}</th>
							else
								<th align={a}>{i}</th>
					}</tr></thead>{
					if (!body.isEmpty)
						<tbody>{
							for (i <- body)
								yield
									<tr>{
										for ((j, a) <- i zip aligns)
											yield
												if (a == "left")
													<td>{j}</td>
												else
													<td align={a}>{j}</td>
										}</tr>}</tbody>}</table>
		}
	
	def rule = """[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})(?=\n|\z)""".r ^^^ (<hr/>)

	def quote_prefix = """[ ]{0,3}> ?"""r

	def quote: Parser[Node] =
		quote_prefix ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(quote_prefix) | success("")) ^^ {case c ~ s => c + s}), quote_prefix) ^^
				{case es =>
					<blockquote>{Markdown.asXML( es.reduce(_ + _) )}</blockquote>
				}

	def indent = """\t|[ ]{1,4}"""r
	
	def li( start: Regex ): Parser[Node] =
		start ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(indent) | success("")) ^^ {case c ~ s => c + s}), indent) ^^
				{case es =>
					<li>{Markdown.item( es.reduce(_ + _) )}</li>
				}
	
	def item = inline ~ document ^^ {case i ~ b => Group( List(i, b) )} //guard("""[^\n]*\z"""r) ~> 
	
	def ul = rep1sep(li( """[ ]{0,3}[\*-] +"""r ), end_block) ^^ {case es => <ul>{es}</ul>}
	
	def ol = rep1sep(li( """[ ]{0,3}\d+\. +"""r ), end_block) ^^ {case es => <ol>{es}</ol>}
	
	def end_block = """\n([ \t]*\n)*|\n?\z"""r
	
	def block = (comment | rule | ul | ol | quote | table | heading1 | heading2 | preformated | reference | triple_code | paragraph) <~ end_block
	
	def blocks = rep(block) ^^ (Group( _ ))
	
	def document = """(?:[ \t]*\n)*""".r ~> blocks
	
	def concat( es: List[Node], sep: Node ) = es.reduce( (a: Node, b: Node) => Group(List(a, sep, b)) )
}

object Markdown
{
	private def stripReturns( s: String ) = s.replace( "\r\n", "\n" ).replace( "\r", "\n" )

	def withHeadings( s: String ) =
	{
	val headings = new ListBuffer[Heading]
	val parser = new Markdown( headings )
	
		parser.parse( parser.phrase(parser.document), stripReturns(s) ) match
		{
			case parser.Success( result, _ ) => (result.toString, headings.toList)
			case parser.Failure( msg, rest ) => sys.error( msg + " at " + rest.pos + "\n" + rest.pos.longString )
			case parser.Error( msg, _ ) => sys.error( msg )
		}
	}
	
	def asXML( s: String ) =
	{
	val parser = new Markdown( null )
	
		parser.parse( parser.phrase(parser.document), stripReturns(s) ) match
		{
			case parser.Success( result, _ ) => result
			case parser.Failure( msg, rest ) => sys.error( msg + " at " + rest.pos + "\n" + rest.pos.longString )
			case parser.Error( msg, _ ) => sys.error( msg )
		}
	}
	
	def item( s: String ) =
	{
	val parser = new Markdown( null )
	
		parser.parse( parser.phrase(parser.item), s ) match
		{
			case parser.Success( result, _ ) => result
			case parser.Failure( msg, rest ) => sys.error( msg + " at " + rest.pos + "\n" + rest.pos.longString )
			case parser.Error( msg, _ ) => sys.error( msg )
		}
	}
		
	private def format( doc: Node ) =
	{
		var level = 0
		val buf = new StringBuilder
		
		def _formatNodes( ns: Seq[Node] )
		{
			ns foreach _format
		}
		
		def nl = buf += '\n'
		
		def _format( n: Node )
		{
			n match
			{
				case e@Elem( _, label, attribs, _, child @ _* ) =>
					buf append "\t"*level

					label match
					{
						case "h1"|"h2"|"p" =>
							buf append e
						case "div"|"ul"|"ol" =>
							nl
							buf += '<'
							buf append label
							buf += ' '
							buf append attribs
							buf += '>'
						case _ =>
							buf append e
					}

					nl
				case Group( s ) => _formatNodes( s )
				case Text( t ) => buf append t
			}
		}

		_format( doc )
		buf.toString
	}

	def apply( s: String ) = asXML( s ).toString
}