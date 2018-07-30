package xyz.hyperreal.markdown


trait Tests {

  def markdown( s: String ) = {
    val ast = Markdown( s )

    Util.headingIds( ast )
    Util.html( ast, 2 )
  }

}