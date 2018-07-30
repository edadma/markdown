package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |> Example:
      |>
      |>     sub status {
      |>         print "working";
      |>     }
      |>
      |> Or:
      |>
      |>     sub status {
      |>         return "working";
      |>     }
    """.trim.stripMargin

  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}