//@
package xyz.hyperreal.markdown

import util.parsing.combinator._
import util.matching.Regex
import xml._
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
	
	def text( p: Parser[String], f: String => String = x => x ) = p ^^ {t => Text( f(t) )}

	def escaped = text( """\\[-\\`\*_\{}\[\]()#\+.!>]"""r, s => s.charAt(1).toString )//todo: maybe unnecessary, see def plain

	def eol = """\n(?![ \t]*\n)""".r
	
//	def code = "`" ~> (" *".r ~> text( """(?:.|\n)+?(?= *`)"""r ) <~ " *`".r ^^ {c => <code>{c}</code>} | success(Text( "`" )))//todo: removed whitespace matching so that ` ` would work

  def code = "`" ~> (text( """[^`]+"""r ) <~ "`" ^^ {c => <code>{c}</code>} | success(Text( "`" )))//todo: removed whitespace matching so that ` ` would work

	def double_code = "``" ~> (" *".r ~> text( """(?:.|\n)+?(?= *``)"""r ) <~ " *``".r ^^ {c => <code>{c}</code>} | success(Text( "``" )))
	
	def link_text = text( """[^\n*_`\\\[\]!]+|\[[^\n*_`\\\]!]*\]"""r )
	
	def link_inline: Parser[Node] = rep1(escaped | strong | em | double_code | code | image | link_text) ^^ (Group( _ ))
	
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

			Text( "" )
		}

	def link =
		"[" ~> ((link_inline ~ """][ \t]*\(""".r ~ ("<" ~> """[^ \t>]*""".r <~ ">" | """[^ \t)]*""".r) ~
			"""[ \t]*""".r ~ opt("\"" ~> """(?:"(?! *\))|[^"\n])*""".r <~ "\"") <~ """[ ]*\)""".r ^^
		{case text ~ _ ~ addr ~ _ ~ title =>
			if (title == None)
				<a href={addr}>{text}</a>
			else
				<a href={addr} title={title.get}>{text}</a>
		}) |
		(link_inline ~ """][ \t\n]*\[""".r ~ """[^ \t\]]*""".r <~ "]" ^^
		{case text ~ sep ~ id =>
			ref( if (id isEmpty) text.toString else id ) match
			{
				case None => Text( "[" + text + sep + id + "]" )
				case Some((addr, title)) =>
					if (title == None)
						<a href={addr}>{text}</a>
					else
						<a href={addr} title={title.get}>{text}</a>
			}
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
		{case alt ~ sep ~ id =>
			val a = alt getOrElse Text( "" )
			ref( id ) match
			{
				case None => Text( "![" + a + sep + id + "]" )
				case Some((addr, title)) =>
					if (title == None)
						<img src={addr} alt={a} />
					else
						<img src={addr} title={title.get} alt={a} />
			}
		}) | success(Text( "!" )))
	
	def inline_element: Parser[Node] = escaped | strong | em | inline_no_em_no_strong
	
	def inline_list: Parser[List[Node]] = rep1(inline_element)
	
	def inline: Parser[Node] = inline_list ^^ (Group( _ ))

	def strikethrough_inline: Parser[Node] = rep1(space_delim( "~~" ) | escaped | strong | em | double_code | code | image | link | underscore_word | autolink |
		space | plain) ^^ (Group( _ ))
	
	def strikethrough = "~~" ~> (not(space) ~> strikethrough_inline <~ "~~" ^^ {case t => <del>{t}</del>} | success(Text("~~")))

	def inline_no_em_no_strong = double_code | code | image | link | underscore_word | autolink | strikethrough | space/* | xml*/ | entity | plain
	
	def inline_no_em_no_strong_allow( allow: String ) = inline_no_em_no_strong | text( allow )

	def space_delim( d: String ) = text( ("""[ \t]+\""" + d)r )
	
	def em_section( d: String, allow: String ) = rep1(escaped | strong_no_em | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ (Group( _ ))

	def _em( d: String, allow: String ) = d ~> not(space) ~> em_section( d, allow ) <~ not(space) <~ d ^^ {case e => <em>{e}</em>} | text( d )

	def em = _em( "*", "_" ) | _em( "_", "*" )
	
	def em_no_strong_section = rep1(escaped | inline_no_em_no_strong) ^^ (Group( _ ))

	def _em_no_strong( d: String ) =
		(d ~ not(d ~ not(d))) ~> ((em_no_strong_section <~ (d ~ not(d ~ not(d))) ^^ {case e => <em>{e}</em>}) | success(Text( d )))
	
	def em_no_strong = _em_no_strong( "*" ) | _em_no_strong( "_" )
	
	def strong_section( d: String, allow: String ) = rep1(escaped | em_no_strong | space_delim( d ) | inline_no_em_no_strong_allow(allow)) ^^ (Group( _ ))
	
	def _strong( d: String, allow: String ) = d ~> not(space) ~> strong_section( d, allow ) <~ not(space) <~ d ^^ {case e => <strong>{e}</strong>} | text( d )
	
	def strong: Parser[Node] = _strong( "**", "__" ) | _strong( "__", "**" )
	
	def strong_no_em_section = rep1(escaped | inline_no_em_no_strong) ^^ (Group( _ ))
	
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
			
			level match
			{
				case 1 => <h1>{e}</h1>
				case 2 => <h2>{e}</h2>
			}
		}

	def comment = """[ ]{0,3}/\*[^*]*\*+(?:[^*/][^*]*\*+)*/[ \t]*""".r ^^^ Text( "" )

	def paragraph_inline_element =
		rep1(
			"\\\n" ^^^ (if (featureBackslashBreak) <br/> else Text("\\\n")) |
			"""  +\n""".r ^^^ <br/> |
			"\n" ^^^ (if (featureNewlineBreak) <br/> else Text("\n")) |
			inline_element) ^^
			(Group( _ ))
			
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
					Text( Markdown.tabs2spaces(es.reduce(_ + _), tabSize) )
				}</code></pre>
			}

	def triple_code =
		("""```[ \t]*""".r ~> "[^ \t\n]*".r <~ """[ \t]*\n""".r) ~ (rep(guard(not("\n```")) ~> elem("", ch => true)) <~ "\n```") ^^ {
    case "" ~ c =>
			<pre><code>{
				Text( c.mkString )
			}</code></pre>
    case l ~ c =>
			<pre><code class="highlight">{
				Unparsed( Highlighters.supported(l).highlight(c.mkString) )
			}</code></pre> }

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
	
	def rule = """[ ]{0,3}(?:(?:-[ \t]*){3,}|(?:\*[ \t]*){3,}|(?:_[ \t]*){3,})(?=\n|\z)""".r ^^^ <hr/>

	def entity: Parser[Node] = "&" ~> ("[a-zA-Z]+".r ~ ";" ^^
		{case n ~ s =>
			buf.clear
			
			Utility.unescape( n, buf ) match
			{
				case null => Text( '&' + n + s )
				case _ => Text( buf.toString )
			}
		} | success(Text( "&" )))
	
	def tag_name = "[:_A-Za-z][:_A-Za-z0-9.-]*"r
	
	def xml_string: Parser[String] =
        "<" ~ tag_name ~ """.*?/ ?>[ \t]*""".r ^^ {case o ~ n ~ c => o + n + c} |    // should be [^/]*/> for closing regex
        "<" ~ tag_name ~ """[^>\n]*>[^<]*""".r ~ rep(xml_string ~ "[^<]*".r ^^ {case a ~ b => a + b}) ~ """</""".r ~ tag_name ~ """ ?>[ \t]*""".r ^^
            {case os ~ s ~ cs ~ el ~ oe ~ e ~ ce => os + s + cs + el.mkString + oe + e + ce} |
		"""<!--(?:.|\n)*?-->[ \t]*""".r

	def xml = xml_string ^^
		{s =>
			if (s startsWith "<!--")
				Text( "" )
			else
				Try( {/*println( "===\n" + s + "\n===" ); */XML.loadString(s)} ).getOrElse( Text(s) )
		}
		
	def quote_prefix = """[ ]{0,3}> ?"""r

	def quote: Parser[Node] =
		quote_prefix ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n([ \t]*\n)*""".r <~ guard(quote_prefix) | success("")) ^^ {case c ~ s => c + s}), quote_prefix) ^^
				{case es =>
					<blockquote>{parseRule( document, es.reduce(_ + _) )}</blockquote>
				}

	def indent = """\t|[ ]{1,4}"""r
	
	def li( start: Regex ): Parser[Node] =
		start ~>
			rep1sep(("""[^\n]*""".r ~ ("""\n(?:[ \t]*\n)*""".r <~ guard(indent) | success("")) ^^ {case c ~ s => c + s}), indent) ^^
				{case es =>
					<li>{parseRule( item, es.reduce(_ + _) )}</li>
				}
	
	def item_inline =
		rep1(
			"""\n(?!(?:[ \n]*\n|[*+-]| *[0-9]+\.))""".r ^^^ Text("\n") |
			inline_element) ^^
			(Group( _ ))

	def item = item_inline ~ document ^^ {case i ~ b => Group( List(i, b) )} //guard("""[^\n]*\z"""r) ~> 
	
	def ul = rep1sep(li( """[ ]{0,3}[*+-](?: +|\t)"""r ), end_block) ^^ {es => <ul>{es}</ul>}
	
	def ol = rep1sep(li( """[ ]{0,3}\d+\.(?: +|\t)"""r ), end_block) ^^ {es => <ol>{es}</ol>}
	
	def end_block = """\n([ \t]*\n)*|\n?\z"""r
	
	def block = (comment | rule | ul | ol | quote | table | heading1 | heading2 | preformated | reference | triple_code | xml | paragraph) <~ end_block
	
	def blocks = rep(block) ^^ Group
	
	def document = """(?:[ \t]*\n)*""".r ~> blocks
	
	def concat( es: List[Node], sep: Node ) = es.reduce( (a: Node, b: Node) => Group(List(a, sep, b)) )
	
	protected def parseRule( rule: Parser[Node], s: String ) =
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

case class Heading( heading: String, id: String, level: Int, subheadings: List[Heading] )

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
				case Group( s ) => _normalizeNodes( s )
				case Text( t ) => buf append t
			}
		}

		_normalize( doc )
		buf.toString
	}

	def asXML( s: String, features: String* ) = (new Markdown( features: _* )).parseDocument( s )

  def headings( doc: Node ) = {
    case class HeadingMutable( heading: String, id: String, level: Int, subheadings: ListBuffer[HeadingMutable] )

    val buf = HeadingMutable( "", "", 0, new ListBuffer[HeadingMutable] )
    var trail: List[HeadingMutable] = List( buf )

    def addHeading( n: Node ): Unit = {
      val level = n.label.substring( 1 ).toInt

      if (level > trail.head.level) {
        val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level, new ListBuffer[HeadingMutable] )

        trail.head.subheadings += sub
        trail = sub :: trail
      } else if (level == trail.head.level) {
        val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level, new ListBuffer[HeadingMutable] )

        trail.tail.head.subheadings += sub
        trail = sub :: trail.tail
      } else {
        val sub = HeadingMutable( n.child.mkString, n.attribute("id").get.mkString, level, new ListBuffer[HeadingMutable] )

        do {
          trail = trail.tail
        } while (trail.head.level >= level)

        addHeading( n )
      }
    }

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

      override def transform(n: Node) = n match {
        case e @ ((<h1>{_*}</h1>)|(<h2>{_*}</h2>)|(<h3>{_*}</h3>)|(<h4>{_*}</h4>)|(<h5>{_*}</h5>)|(<h6>{_*}</h6>)) =>
          e.asInstanceOf[Elem] % Attribute(null, "id", id(e.child.mkString), Null)
        case _ => n
      }
    }

    def headings( doc: Node ): Unit =
      doc match {
        case e@Elem( _, label, attribs, _, child @ _* ) =>
          label match {
            case "h1"|"h2"|"h3"|"h4"|"h5"|"h6" => addHeading( e )
            case _ => child foreach headings
          }
        case Group( s ) => s foreach headings
        case _ =>
      }

    def list( b: ListBuffer[HeadingMutable] ): List[Heading] =
      if (b isEmpty)
        Nil
      else
        b map {case HeadingMutable( heading, id, level, subheadings ) => Heading( heading, id, level, list(subheadings) )} toList

    val res = new RuleTransformer( rule1 ).transform( doc )

    headings( new Group(res) )
    (res.mkString, list( buf.subheadings ))
  }

	def withHeadings( s: String ) = {
    val xml = asXML( s )

    headings( xml )
  }

	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}

object GFM
{
	def asXML( s: String, features: String* ) = (new Markdown( ("gfm" +: features): _* )).parseDocument( s )
	
	def apply( s: String, features: String* ) = asXML( s, features: _* ).toString
}