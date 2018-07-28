package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |<=
      |===
      |
      |## Description
      |
      |Tests whether one argument is less than or equal to the other.
    """.trim.stripMargin

  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}