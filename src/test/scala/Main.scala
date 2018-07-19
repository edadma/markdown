package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |asdf
    """.trim.stripMargin

  println( Markdown(input) )

}