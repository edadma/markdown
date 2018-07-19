package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |asdf
      |====
      |
      |This is *too* cool.
      |
      |asdf
      |====
      |
      |qwer
      |====
    """.trim.stripMargin

  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}