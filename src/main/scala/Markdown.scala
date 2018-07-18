//@
package xyz.hyperreal.markdown

import xml._
import util.parsing.combinator._
import util.matching.Regex
import collection.mutable.{HashMap, ListBuffer}
import scala.collection.mutable
import scala.util.Try
import scala.xml.transform.{RewriteRule, RuleTransformer}

import xyz.hyperreal.highlighter.Highlighters


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
			case "gfm" => featureNewlineBreak = true
			case "commonmark" => featureBackslashBreak = true
			case Markdown.TABSIZE_REGEX( s ) =>
				tabSize = s.toInt
				
				if (tabSize > 0)
					sys.error( "tab-size should be positive: " + tabSize )
			case _ => sys.error( "unrecognized feature: " + f )
		}
	
	override def skipWhitespace = false

	val escapedRegex = """\\(.)"""r

	def plain = text( """(?:\\.|[^\n~*_`\[! \t&])+"""r, s => escapedRegex.replaceAllIn(s, _.matched) )

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
	
	def text( p: Parser[String], f: String => String = x => x ) = p ^^ {t => TextAST( f(t) )}

	def escaped = text( """\\[-\\`\*_\{}\[\]()#\+.!>]"""r, s => s.charAt(1).toString )//todo: maybe unnecessary, see def plain

	def eol = """\n(?![ \t]*\n)""".r
	
//	def code = "`" ~> (" *".r ~> text( """(?:.|\n)+?(?= *`)"""r ) <~ " *`".r ^^ {c => <code>{c}</code>} | success(Text( "`" )))//todo: removed whitespace matching so that ` ` would work

  def code = "`" ~> (text( """[^`]+"""r ) <~ "`" ^^ {c => <code>{c}</code>} | success(TextAST( "`" )))//todo: removed whitespace matching so that ` ` would work

	def double_code = "``" ~> (" *".r ~> text( """(?:.|\n)+?(?= *``)"""r ) <~ " *``".r ^^ {c => <code>{c}</code>} | success(TextAST( "``" )))
	
	def link_text = text( """[^\n*_`\\\[\]!]+|\[[^\n*_`\\\]!]*\]"""r )
	
	def link_inline = rep1(escaped | strong | em | double_code | code | image | link_text) ^^ (SeqAST( _ ))
	
	private def ref( id: String ): Option[(String, Option[String])] =
		if (refmap contains id)
			Some( refmap(id) )
		else
		{
			pass2 = true
			None
		}

	def reference = 
		("""[ ]{0,3}\[""".r ~>
			"""[^\]\n]*""".r <~ "]:[ ]*".r) ~ """[^ \n\t"]+""".r ~ ("[ ]*".r ~> opt("\"" ~> """(?:"(?=[^\n"]*")|[^"\n])*""".r <~ "\"" <~ """[ \t]*(?=\n|\z)""".r)) ^^
		{case id ~ addr ~ title =>
			refmap(id) = (addr, title)

			TextAST( "" )
		}

	def link =
		"[" ~> ((link_inline ~ """][ \t]*\(""".r ~ ("<" ~> """[^ \t>]*""".r <~ ">" | """[^ \t)]*""".r) ~
			"""[ \t]*""".r ~ opt("\"" ~> """(?:"(?! *\))|[^"\n])*""".r <~ "\"") <~ """[ ]*\)""".r ^^
		{case text ~ _ ~ addr ~ _ ~ title =>
			if (title == None)
				LinkAST( addr.toString, None, text )
			else
				LinkAST( addr.toString, Some(title.toString), text )
		}) |
		(link_inline ~ """][ \t\n]*\[""".r ~ """[^ \t\]]*""".r <~ "]" ^^
		{case text ~ sep ~ id =>
			ref( if (id isEmpty) text.toString else id ) match
			{
				case None => TextAST( "[" + text + sep + id + "]" )
				case Some((addr, title)) =>
					if (title == None)
						LinkAST( addr.toString, None, text )
					else
						LinkAST( addr.toString, Some(title.toString), text )
			}
		}) | success(TextAST( "[" )))
	
	def image = "!" ~> ("[" ~> (opt(link_inline) ~ """][ \t]*\(""".r ~ """[^ )]+""".r ~ "[ ]*".r ~ opt("\"" ~> """[^"\n]+""".r <~ "\"") <~ """[ ]*\)""".r ^^
		{case alt ~ _ ~ addr ~ _ ~ title =>
			val a = alt getOrElse TextAST( "" )
			
			if (title == None)
				<img src={addr} alt={a} />
			else
				<img src={addr} title={title.get} alt={a} />
		}) |
		"[" ~> (opt(link_inline) ~ """][ \t]*\[""".r ~ """[^ \t\]]*""".r <~ "]" ^^
		{case alt ~ sep ~ id =>
			val a = alt getOrElse TextAST( "" )
			ref( id ) match
			{
				case None => TextAST( "![" + a + sep + id + "]" )
				case Some((addr, title)) =>
					if (title == None)
						<img src={addr} alt={a} />
					else
						<img src={addr} title={title.get} alt={a} />
			}
		}) | success(TextAST( "!" )))
	
	def inline_element = escaped | strong | em | inline_no_em_no_strong
	
	def inline_list = rep1(inline_element)
	
	def inline = inline_list ^^ (SeqAST( _ ))

	def strikethrough_inline = rep1(space_delim( "~~" ) | escaped | strong | em | double_code | code | image | link | underscore_word | autolink |
		space | plain) ^^ (SeqAST( _ ))
	
	def strikethrough = "~~" ~> (not(space) ~> strikethrough_inline <~ "~~" ^^ {case t => <del>{t}</del>} | success(TextAST("~~")))

	def inline_no_em_no_strong = double_code | code | image | link | underscore_word | autolink | strikethrough | space/* | xml*/ | entity | plain
	
	def inline_no_em_no_strong_allow( allow: String ) = inline_no_em_no_strong | text( allow )

	def space_delim( d: String ) = text( ("""[ \t]+\""" + d)r )
	
	def em_section( d: String, allow: String ) = rep1(escaped | strong_no_em | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ (SeqAST( _ ))

	def _em( d: String, allow: String ) = d ~> not(space) ~> em_section( d, allow ) <~ not(space) <~ d ^^ {case e => <em>{e}</em>} | text( d )

	def em = _em( "*", "_" ) | _em( "_", "*" )
	
	def em_no_strong_section = rep1(escaped | inline_no_em_no_strong) ^^ (SeqAST( _ ))

	def _em_no_strong( d: String ) =
		(d ~ not(d ~ not(d))) ~> ((em_no_strong_section <~ (d ~ not(d ~ not(d))) ^^ { e => EmphasisAST( e )}) | success(text( d )))
	
	def em_no_strong = _em_no_strong( "*" ) | _em_no_strong( "_" )
	
	def strong_section( d: String, allow: String ) = rep1(escaped | em_no_strong | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ (SeqAST( _ ))
	
	def _strong( d: String, allow: String ) = d ~> not(space) ~> strong_section( d, allow ) <~ not(space) <~ d ^^ {case e => <strong>{e}</strong>} | text( d )
	
	def strong = _strong( "**", "__" ) | _strong( "__", "**" )
	
	def strong_no_em_section = rep1(escaped | inline_no_em_no_strong) ^^ (SeqAST( _ ))
	
	def _strong_no_em( d: String ) = d ~> ((strong_no_em_section <~ d ^^ { e => StrongAST( e )}) | success(TextAST( d )))

	def strong_no_em = _strong_no_em( "**" ) | _strong_no_em( "__" )

	val HEADING_TAIL_PATTERN = "(.*?)[ ]*#*"r

	def heading_inline = rep1(escaped | strong | em | double_code | code | space | plain) ^^
		{case l =>
			val (front, back) = l.splitAt( l.length - 1 )
			val last =
				back.head match
				{
					case TextAST( t ) =>
						val HEADING_TAIL_PATTERN( s ) = t
						
						TextAST( s )
					case item => item
				}

			SeqAST( front :+ last )
		}

	def heading1 = """#{1,6}[ \t]*""".r ~ heading_inline ^^
		{case l ~ e =>
			val level = l.count(_ == '#')

      HeadingAST( level, e )
		}
	
	def heading2 = inline ~ """\n(?:-+|=+)[ \t]*(?=\n|\z)""".r ^^
		{case e ~ l =>
			val level = if (l.charAt(1) == '=') 1 else 2
			
      HeadingAST( level, e )
		}

	def comment = """[ ]{0,3}/\*[^*]*\*+(?:[^*/][^*]*\*+)*/[ \t]*""".r ^^^ TextAST( "" )

	def paragraph_inline_element =
		rep1(
			"\\\n" ^^^ (if (featureBackslashBreak) BreakAST else TextAST("\\\n")) |
			"""  +\n""".r ^^^ <br/> |
			"\n" ^^^ (if (featureNewlineBreak) BreakAST else TextAST("\n")) |
			inline_element) ^^
			(SeqAST( _ ))
			
	def paragraph = 
// 		"""[ ]{0,3}""".r ~> """(?:.|\n)+?(?= *(?:\n(?:[ \t]*(?:\n|\z)|#|[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})|```)|\z))""".r <~ " *".r ^^
// 			{p => <p>{parseRule( paragraph_inline_element, p )}</p>}
		rep1sep("""[ ]{0,3}""".r ~> """.+""".r, """\n(?!\n(?:[ \t]*(?:\n|\z)|#|[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})|```)|\z)"""r) <~ " *".r ^^
			{p => <p>{parseRule( paragraph_inline_element, p.reduce(_ + "\n" + _) )}</p>}

	def preformated_prefix = """[ ]{4}|[ ]{0,3}\t"""r

	def preformated =
		preformated_prefix ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(preformated_prefix) | success("")) ^^ {case c ~ s => c + s}), preformated_prefix) ^^
			{case es =>
				<pre><code>{
					TextAST( Markdown.tabs2spaces(es.reduce(_ + _), tabSize) )
				}</code></pre>
			}

	def triple_code =
		("""```[ \t]*""".r ~> "[^ \t\n]*".r ~ opt("""\s*"""".r ~> "[^\"]+".r <~ "\"") <~ """[ \t]*\n""".r) ~ (rep(guard(not("\n```")) ~> elem("", ch => true)) <~ "\n```") ^^ {
    case "" ~ None ~ c =>
      CodeBlockAST( c.mkString, None, None )
    case l ~ None ~ c =>
      Highlighters.registered( l ) match {
        case None => CodeBlockAST( c.mkString, None, None )
        case Some( h ) => CodeBlockAST( h.highlight(c.mkString), Some(l), None )
      }
    case "" ~ Some(cap) ~ c =>
      CodeBlockAST( c.mkString, None, Some(cap) )
    case l ~ Some(cap) ~ c =>
      Highlighters.registered( l ) match {
        case None => CodeBlockAST( c.mkString, None, Some(cap) )
        case Some( h ) => CodeBlockAST( h.highlight(c.mkString), Some(l), Some(cap) )
      }
    }

	def table_plain = text( """[^\n*_`\\\[!|]+"""r )

	def table_inline =
    rep1(escaped | strong | em | double_code | code | image | link | autolink | space | table_plain) ^^
		{case l =>
			val (front, back) = l.splitAt( l.length - 1 )
			val last =
				back.head match
				{
					case TextAST( t ) => TextAST( t.trim )
					case item => item
				}

			SeqAST( front :+ last )
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
	
	def rule = """[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})(?=\n|\z)""".r ^^^ <hr/>

	def entity: Parser[Node] = "&" ~> ("[a-zA-Z]+".r ~ ";" ^^
		{case n ~ s =>
			buf.clear
			
			Utility.unescape( n, buf ) match
			{
				case null => TextAST( '&' + n + s )
				case _ => TextAST( buf.toString )
			}
		} | success(TextAST( "&" )))
	
	def tag_name = "[:_A-Za-z][:_A-Za-z0-9.-]*"r
	
	def xml_string: Parser[String] =
        "<" ~ tag_name ~ """.*?/ ?>[ \t]*""".r ^^ {case o ~ n ~ c => o + n + c} |    // should be [^/]*/> for closing regex
        "<" ~ tag_name ~ """[^>\n]*>[^<]*""".r ~ rep(xml_string ~ "[^<]*".r ^^ {case a ~ b => a + b}) ~ """</""".r ~ tag_name ~ """ ?>[ \t]*""".r ^^
            {case os ~ s ~ cs ~ el ~ oe ~ e ~ ce => os + s + cs + el.mkString + oe + e + ce} |
		"""<!--(?:.|\n)*?-->[ \t]*""".r

	def xml = xml_string ^^
		{s =>
			if (s startsWith "<!--")
				TextAST( "" )
			else
				Try( {/*println( "===\n" + s + "\n===" ); */XML.loadString(s)} ).getOrElse( TextAST(s) )
		}
		
	def quote_prefix = """[ ]{0,3}> ?"""r

	def quote =
		quote_prefix ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(quote_prefix) | success("")) ^^ {case c ~ s => c + s}), quote_prefix) ^^
				{case es =>
					<blockquote>{parseRule( document, es.reduce(_ + _) )}</blockquote>
				}

	def indent = """\t|[ ]{1,4}"""r
	
	def li( start: Regex ) =
		start ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n(?:[ \t]*\n)*""".r <~ guard(indent) | success("")) ^^ {case c ~ s => c + s}), indent) ^^
				{case es =>
					<li>{parseRule( item, es.reduce(_ + _) )}</li>
				}
	
	def item_inline =
		rep1(
			"""\n(?!(?:[ \n]*\n|[*+-]| *[0-9]+\.))""".r ^^^ TextAST("\n") |
			inline_element) ^^
			(SeqAST( _ ))

	def item = item_inline ~ document ^^ {case i ~ b => SeqAST( List(i, b) )} //guard("""[^\n]*\z"""r) ~>
	
	def ul = rep1sep(li( """[ ]{0,3}[*+-](?: +|\t)"""r ), end_block) ^^ {es => <ul>{es}</ul>}
	
	def ol = rep1sep(li( """[ ]{0,3}\d+\.(?: +|\t)"""r ), end_block) ^^ {es => <ol>{es}</ol>}
	
	def end_block = """\n([ \t]*\n)*|\n?\z"""r
	
	def block = (comment | rule | ul | ol | quote | table | heading1 | heading2 | preformated | reference | triple_code | xml | paragraph) <~ end_block
	
	def blocks = rep(block) ^^ SeqAST
	
	def document = """(?:[ \t]*\n)*""".r ~> blocks

	protected def parseRule( rule: Parser[AST], s: String ) =
	{
		parseAll( rule, s ) match
		{
			case Success( result, _ ) => result
			case Failure( msg, rest ) => sys.error( msg + " at " + rest.pos + "\n" + rest.pos.longString )
			case Error( msg, _ ) => sys.error( msg )
		}
	}
	
	def parseDocument( s: String ) =
	{
	val s1 = Markdown.stripReturns( s )
	
		pass2 = false
		refmap.clear
		
	val result = parseRule( document, s1 )
	
		if (pass2)
			parseRule( document, s1 )
		else
			result
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

	private def normalize( doc: Node ) =
	{
		var level = 0
		val buf = new StringBuilder
		
		def _normalizeNodes( ns: Seq[Node] ) = ns foreach _normalize
		
		def nl = buf += '\n'
		
		def _normalize( n: Node )
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
				case SeqAST( s ) => _normalizeNodes( s )
				case TextAST( t ) => buf append t
			}
		}

		_normalize( doc )
		buf.toString
	}

	def asXML( s: String, features: String* ) = {
		val doc = (new Markdown( features: _* )).parseDocument( s )

    val idmap = new mutable.HashMap[String, Int]
    val idset = new mutable.HashSet[String]
    val rule1 = new RewriteRule {
      def id( s: String ) = {
        val ids =
          if (s isEmpty)
            "_"
          else
            s.replace( ' ', '_' ).replace( '\t', '_' ).replace( '\r', '_' ).replace( '\n', '_' )

        if (idset(ids))
          idmap get ids match {
            case None =>
              val rid = s"$ids-1"

              idset += rid
              idmap(ids) = 2
              rid
            case Some( count ) =>
              val rid = s"$ids-$count"

              idset += rid
              idmap(ids) = count + 1
              rid
          }
        else {
          idset += ids
          idmap(ids) = 1
          ids
        }
      }

      def text( n: ElementAST ): String = {
        n match {
          case SeqAST( ns ) => ns map text mkString
          case TextAST( t ) => t
          case e => e.elements map text mkString
        }
      }

      override def transform(n: Node) = n match {
        case e @ ((<h1>{_*}</h1>)|(<h2>{_*}</h2>)|(<h3>{_*}</h3>)|(<h4>{_*}</h4>)|(<h5>{_*}</h5>)|(<h6>{_*}</h6>)) if e attribute "id" isEmpty =>
          e.asInstanceOf[Elem] % Attribute(null, "id", id(TextAST(e)), Null)
        case _ => n
      }
    }

    val res = new RuleTransformer( rule1 ).transform( doc )

		new SeqAST(res)
	}

	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}

object GFM
{
	def asXML( s: String, features: String* ) = (new Markdown( ("gfm" +: features): _* )).parseDocument( s )
	
	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}