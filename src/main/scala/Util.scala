//@
package xyz.hyperreal.markdown

import scala.collection.mutable


object Util {

  def text( n: AST ): String = {
    n match {
      case leaf: LeafAST => leaf.text
      case e => e.elements map text mkString
    }
  }

  def headingIds( ast: AST ) = {
    val idmap = new mutable.HashMap[String, Int]
    val idset = new mutable.HashSet[String]

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

    def headingIds( ast: AST ): Unit = {
      ast match {
        case SeqAST( s ) => s foreach headingIds
        case h@HeadingAST( _, contents, _ ) => h.id = Some( id(text(contents)) )
        case _ =>
      }
    }

    headingIds( ast )
  }

  def html( doc: AST, tab: Int, codeblock: (String, Option[String], Option[String]) => String = null ) = {
    val buf = new StringBuilder

    def tag( tag: String, contents: AST, attr: (String, String)* ) =
      s"<$tag${if (attr nonEmpty) " " else ""}${attr map {case (k, v) => s"""$k="$v""""} mkString ", "}>${html( contents )}</$tag>"

    def leaf( tag: String, contents: String, attr: (String, String)* ) =
      s"<$tag${if (attr nonEmpty) " " else ""}${attr map {case (k, v) => s"""$k="$v""""} mkString ", "}>${escape( contents )}</$tag>"

    def escape( s: String ) = {
      val buf = new StringBuilder

      s foreach {
        case '&' => buf ++= "&amp;"
        case '<' => buf ++= "&lt;"
        case '>' => buf ++= "&gt;"
        case '"' => buf ++= "&quot;"
        case '\\' => buf ++= "&bsol;"
        case '{' => buf ++= "&lcub;"
        case '}' => buf ++= "&rcub;"
        case c if c > '\u007F' => buf ++= s"&#${c.toInt};"
        case c => buf += c
      }

      buf.toString
    }

    def html( doc: AST ): String =
      doc match {
        case SeqAST( s ) => s map html mkString
        case TextAST( t ) => escape( t )
        case ParagraphAST( contents ) => tag( "p", contents )
        case BlockquoteAST( contents ) => tag( "blockquote", contents )
        case HeadingAST( level, contents, Some(id) ) => tag( s"h$level", contents, "id" -> id )
        case HeadingAST( level, contents, None ) => tag( s"h$level", contents )
        case CodeInlineAST( c ) => leaf( "code", c )
        case CodeBlockAST( c, highlighted, caption ) =>
          val escaped = escape( c )

          if (codeblock eq null)
            s"<pre><code>$escaped</code></pre>"
          else
            codeblock( escaped, highlighted, caption )
        case LinkAST( address, None, contents ) => tag( "a", contents, "href" -> address )
        case LinkAST( address, Some(title), contents ) => tag( "a", contents, "href" -> address, "title" -> title )
        case ListItemAST( contents ) => tag( "li", contents )
        case UnorderedListAST( contents ) => tag( "ul", contents )
        case OrderedListAST( contents ) => tag( "ol", contents )
        case ImageAST( address, None, text ) => leaf( "img", text, "src" -> address )
        case ImageAST( address, Some(title), text ) => leaf( "img", text, "src" -> address, "title" -> title )
        case EmphasisAST( contents ) => tag( "em", contents )
        case StrongAST( contents ) => tag( "strong", contents )
        case StrikethroughAST( contents ) => tag( "del", contents )
        case BreakAST => "<br/>"
        case RuleAST => "<hr/>"
        case TableCellAST( align, contents ) => tag( "td", contents, "align" -> align )
        case TableHeadRowAST( contents ) => tag( "th", contents )
        case TableBodyRowAST( contents ) => tag( "tr", contents )
        case TableHeadAST( contents ) => tag( "thead", contents )
        case TableBodyAST( contents ) => tag( "tbody", contents )
        case TableAST( contents ) => tag( "table", contents )
        case EntityAST( entity, _ ) => s"&$entity;"
      }

    html( doc )
  }

}