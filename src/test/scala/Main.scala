package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |asdf
      |====
      |
      |asdf
      |====
      |
      |qwer
      |====
    """.trim.stripMargin

  val res = Markdown( input )

  Util.headingIds( res )
  println( res )

}