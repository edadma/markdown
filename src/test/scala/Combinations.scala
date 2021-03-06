package xyz.hyperreal.markdown

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Combinations extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Tests
{
	"examples" in
	{
		markdown( """
			|> ## This is a header.
			|> 
			|> 1.   This is the first list item.
			|> 2.   This is the second list item.
			|>
			|> > This is nested blockquote.
			|>
			|> 
			|> Here's some example code:
			|> 
			|>     return shell_exec("echo $input | .markdown_script");
			|""".stripMargin
		) shouldBe """<blockquote><h2 id="This_is_a_header.">This is a header.</h2><ol><li>This is the first list item.</li><li>This is the second list item.</li></ol><blockquote><p>This is nested blockquote.</p></blockquote><p>Here's some example code:</p><pre><code>return shell_exec(&quot;echo $input | .markdown_script&quot;);</code></pre></blockquote>"""
	}
}