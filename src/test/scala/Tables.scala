package ca.hyperreal.superMarkdown

import org.scalatest._
import prop.PropertyChecks


class Tables extends FreeSpec with PropertyChecks with Matchers
{
	"mixed" in
	{
		Markdown( """
			|Colons can be used to align columns.
			|
			|| Tables        | Are           | Cool  |
			|| ------------- |:-------------:| -----:|
			|| col 3 is      | right-aligned | $1600 |
			|| col 2 is      | centered      |   $12 |
			|| zebra stripes | are neat      |    $1 |
			|
			|The outer pipes (|) are optional, and you don't need to make the raw Markdown line up prettily. You can also use inline Markdown.
			|
			|Markdown | Less | Pretty
			|--- | --- | ---
			|*Still* | `renders` | **nicely**
			|1 | 2 | 3
			|""".stripMargin
		) shouldBe """<p>Colons can be used to align columns.</p><table><thead><tr><th>Tables</th><th align="center">Are</th><th align="right">Cool</th></tr></thead><tbody><tr><td>col 3 is</td><td align="center">right-aligned</td><td align="right">$1600</td></tr><tr><td>col 2 is</td><td align="center">centered</td><td align="right">$12</td></tr><tr><td>zebra stripes</td><td align="center">are neat</td><td align="right">$1</td></tr></tbody></table><p>The outer pipes (|) are optional, and you don't need to make the raw Markdown line up prettily. You can also use inline Markdown.</p><table><thead><tr><th>Markdown</th><th>Less</th><th>Pretty</th></tr></thead><tbody><tr><td><em>Still</em></td><td><code>renders</code></td><td><strong>nicely</strong></td></tr><tr><td>1</td><td>2</td><td>3</td></tr></tbody></table>"""
	}
}