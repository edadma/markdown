package xyz.hyperreal.markdown

import java.io._


object Main extends App {

  println( Markdown(
    """
      |asdf
    """.trim.stripMargin
  ) )

}