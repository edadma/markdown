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

}