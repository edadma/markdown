Markdown Processor for Java/Scala
=================================

## Usage

Add the following lines to your `build.sbt`

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "xyz.hyperreal" %% "scala-markdown" % "0.3.1"

## Example

The following code tests one of the examples given in the [Markdown Syntax Cheatsheet](http://daringfireball.net/projects/markdown/dingus).

	import xyz.hyperreal.markdown._

	object MarkdownTest extends App
	{
		val s =
	"""
	*   Abacus
		* answer
	*   Bubbles
		1.  bunk
		2.  bupkis
			* BELITTLER
		3. burper
	*   Cunning
	"""

		println( Markdown(s) )
	}
	
The above code should print

	<ul><li>Abacus<ul><li>answer</li></ul></li><li>Bubbles<ol><li>bunk</li><li>bupkis<ul><li>BELITTLER</li></ul></li><li>burper</li></ol></li><li>Cunning</li></ul>