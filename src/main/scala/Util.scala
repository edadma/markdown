//@
package xyz.hyperreal.markdown


object Util {

  def text( n: AST ): String = {
    n match {
      case TextAST( t ) => t
      case e => e.elements map text mkString
    }
  }

  def headingIds( ast: AST ) = {
    def headingIds( ast: AST ): Unit = {
      ast match {
        case SeqAST( s ) => s foreach headingIds
        case h@HeadingAST( _, contents, _ ) =>

        case _ =>
      }
    }

    headingIds( ast )
  }

}