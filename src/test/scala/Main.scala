package xyz.hyperreal.markdown

import java.io._


object Main extends App {

  println( Markdown(
    """
      |This *`is a` test*
      |==================
    """.trim.stripMargin
  ) )

}