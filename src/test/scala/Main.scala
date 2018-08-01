package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |* alkj fds fdsa fds fds
      |oiu dsf oiu dsaf oiu
    """.trim.stripMargin
//    """
//      |Asterisks tight:
//      |
//      |*	a 1
//      |*	a 2
//      |*	a 3
//      |
//      |
//      |Asterisks loose:
//      |
//      |*	a 1
//      |
//      |
//      |*	a 2
//      |
//      |
//      |*	a 3
//      |
//      |stuff
//    """.trim.stripMargin

  println( input )
  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}
