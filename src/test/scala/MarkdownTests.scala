package ca.hyperreal.__markdown__

import org.scalatest._
import prop.PropertyChecks


class MarkdownTests extends FreeSpec with PropertyChecks with Matchers
{
/*

	"NAME" in
	{
		Markdown(
"""
""" ) shouldBe """OUTPUT"""
	}
*/
	"Amps and angle encoding" in
	{
		Markdown( """
AT&T has an ampersand in their name.

AT&amp;T is another way to write it.

This & that.

4 < 5.

6 > 5.

Here's a [link] [1] with an ampersand in the URL.

Here's a link with an amersand in the link text: [AT&T] [2].

Here's an inline [link](/script?foo=1&bar=2).

Here's an inline [link](</script?foo=1&bar=2>).


[1]: http://example.com/?foo=1&bar=2
[2]: http://att.com/  "AT&T"
""" ) shouldBe """<p>AT&amp;T has an ampersand in their name.</p><p>AT&amp;T is another way to write it.</p><p>This &amp; that.</p><p>4 &lt; 5.</p><p>6 &gt; 5.</p><p>Here's a <a href="http://example.com/?foo=1&amp;bar=2">link</a> with an ampersand in the URL.</p><p>Here's a link with an amersand in the link text: <a href="http://att.com/" title="AT&amp;T">AT&amp;T</a>.</p><p>Here's an inline <a href="/script?foo=1&amp;bar=2">link</a>.</p><p>Here's an inline <a href="/script?foo=1&amp;bar=2">link</a>.</p>"""
	}
	
	"Auto links" in
	{
		Markdown( """
Link: <http://example.com/>.

With an ampersand: <http://example.com/?foo=1&bar=2>

* In a list?
* <http://example.com/>
* It should.

> Blockquoted: <http://example.com/>

Auto-links should not occur here: `<http://example.com/>`

	or here: <http://example.com/>
""" ) shouldBe """<p>Link: <a href="http://example.com/">http://example.com/</a>.</p><p>With an ampersand: <a href="http://example.com/?foo=1&amp;bar=2">http://example.com/?foo=1&amp;bar=2</a></p><ul><li>In a list?</li><li><a href="http://example.com/">http://example.com/</a></li><li>It should.</li></ul><blockquote><p>Blockquoted: <a href="http://example.com/">http://example.com/</a></p></blockquote><p>Auto-links should not occur here: <code>&lt;http://example.com/&gt;</code></p><pre><code>or here: &lt;http://example.com/&gt;</code></pre>"""
	}
	
	"Backslash escapes" in
	{
		Markdown( """
These should all get escaped:

Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: \{

Right brace: \}

Left bracket: \[

Right bracket: \]

Left paren: \(

Right paren: \)

Greater-than: \>

Hash: \#

Period: \.

Bang: \!

Plus: \+

Minus: \-



These should not, because they occur within a code block:

	Backslash: \\

	Backtick: \`

	Asterisk: \*

	Underscore: \_

	Left brace: \{

	Right brace: \}

	Left bracket: \[

	Right bracket: \]

	Left paren: \(

	Right paren: \)

	Greater-than: \>

	Hash: \#

	Period: \.

	Bang: \!

	Plus: \+

	Minus: \-


Nor should these, which occur in code spans:

Backslash: `\\`

Backtick: `` \` ``

Asterisk: `\*`

Underscore: `\_`

Left brace: `\{`

Right brace: `\}`

Left bracket: `\[`

Right bracket: `\]`

Left paren: `\(`

Right paren: `\)`

Greater-than: `\>`

Hash: `\#`

Period: `\.`

Bang: `\!`

Plus: `\+`

Minus: `\-`
""" ) shouldBe """<p>These should all get escaped:</p><p>Backslash: \</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: {</p><p>Right brace: }</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: \\

Backtick: \`

Asterisk: \*

Underscore: \_

Left brace: \{

Right brace: \}

Left bracket: \[

Right bracket: \]

Left paren: \(

Right paren: \)

Greater-than: \&gt;

Hash: \#

Period: \.

Bang: \!

Plus: \+

Minus: \-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash: <code>\\</code></p><p>Backtick: <code>\`</code></p><p>Asterisk: <code>\*</code></p><p>Underscore: <code>\_</code></p><p>Left brace: <code>\{</code></p><p>Right brace: <code>\}</code></p><p>Left bracket: <code>\[</code></p><p>Right bracket: <code>\]</code></p><p>Left paren: <code>\(</code></p><p>Right paren: <code>\)</code></p><p>Greater-than: <code>\&gt;</code></p><p>Hash: <code>\#</code></p><p>Period: <code>\.</code></p><p>Bang: <code>\!</code></p><p>Plus: <code>\+</code></p><p>Minus: <code>\-</code></p>"""
	}
	
	"Blockquotes with code blocks" in
	{
		Markdown(
"""
> Example:
> 
>     sub status {
>         print "working";
>     }
> 
> Or:
> 
>     sub status {
>         return "working";
>     }
""" ) shouldBe """<blockquote><p>Example:</p><pre><code>sub status {
    print &quot;working&quot;;
}</code></pre><p>Or:</p><pre><code>sub status {
    return &quot;working&quot;;
}</code></pre></blockquote>"""
	}

	"Hard-wrapped paragraphs with list-like lines" in
	{
		Markdown(
"""
In Markdown 1.0.0 and earlier. Version
8. This line turns into a list item.
Because a hard-wrapped line in the
middle of a paragraph looked like a
list item.

Here's one with a bullet.
* criminey.
""" ) shouldBe """<p>In Markdown 1.0.0 and earlier. Version
8. This line turns into a list item.
Because a hard-wrapped line in the
middle of a paragraph looked like a
list item.</p><p>Here's one with a bullet.
* criminey.</p>"""
	}

	"Horizontal rules" in
	{
		Markdown(
"""
Dashes:

---

 ---
 
  ---

   ---

	---

- - -

 - - -
 
  - - -

   - - -

	- - -


Asterisks:

***

 ***
 
  ***

   ***

	***

* * *

 * * *
 
  * * *

   * * *

	* * *


Underscores:

___

 ___
 
  ___

   ___

    ___

_ _ _

 _ _ _
 
  _ _ _

   _ _ _

    _ _ _
""" ) shouldBe """<p>Dashes:</p><hr/><hr/><hr/><hr/><pre><code>---</code></pre><hr/><hr/><hr/><hr/><pre><code>- - -</code></pre><p>Asterisks:</p><hr/><hr/><hr/><hr/><pre><code>***</code></pre><hr/><hr/><hr/><hr/><pre><code>* * *</code></pre><p>Underscores:</p><hr/><hr/><hr/><hr/><pre><code>___</code></pre><hr/><hr/><hr/><hr/><pre><code>_ _ _</code></pre>"""
	}

	"Links, inline style" in
	{
		Markdown(
"""
Just a [URL](/url/).

[URL and title](/url/ "title").

[URL and title](/url/  "title preceded by two spaces").

[URL and title](/url/	"title preceded by a tab").

[Empty]().
""" ) shouldBe """<p>Just a <a href="/url/">URL</a>.</p><p><a href="/url/" title="title">URL and title</a>.</p><p><a href="/url/" title="title preceded by two spaces">URL and title</a>.</p><p><a href="/url/" title="title preceded by a tab">URL and title</a>.</p><p><a href="">Empty</a>.</p>"""
	}

	"Links, reference style" in
	{
		Markdown(
"""
Foo [bar] [1].

Foo [bar][1].

Foo [bar]
[1].

[1]: /url/  "Title"


With [embedded [brackets]] [b].


Indented [once][].

Indented [twice][].

Indented [thrice][].

Indented [four][] times.

 [once]: /url

  [twice]: /url

   [thrice]: /url

    [four]: /url


[b]: /url/
""" ) shouldBe """<p>Foo <a href="/url/" title="Title">bar</a>.</p><p>Foo <a href="/url/" title="Title">bar</a>.</p><p>Foo <a href="/url/" title="Title">bar</a>.</p><p>With <a href="/url/">embedded [brackets]</a>.</p><p>Indented <a href="/url">once</a>.</p><p>Indented <a href="/url">twice</a>.</p><p>Indented <a href="/url">thrice</a>.</p><p>Indented [four][] times.</p><pre><code>[four]: /url</code></pre>"""
	}

	"Nested blockquotes" in
	{
		Markdown(
"""
> foo
>
> > bar
>
> foo
""" ) shouldBe """<blockquote><p>foo</p><blockquote><p>bar</p></blockquote><p>foo</p></blockquote>"""
	}

	"Strong and em together" in
	{
		Markdown(
"""
***This is strong and em.***

So is ***this*** word.

___This is strong and em.___

So is ___this___ word.
""" ) shouldBe """<p><strong><em>This is strong and em.</em></strong></p><p>So is <strong><em>this</em></strong> word.</p><p><strong><em>This is strong and em.</em></strong></p><p>So is <strong><em>this</em></strong> word.</p>"""
	}
	
	"Tabs" in
	{
		Markdown( """
+	this is a list item
	indented with tabs

+   this is a list item
    indented with spaces

Code:

	this code block is indented by one tab

And:

		this code block is indented by two tabs

And:

	+	this is an example list item
		indented with tabs
	
	+   this is an example list item
	    indented with spaces
""" ) shouldBe """<ul><li>this is a list item
indented with tabs</li><li>this is a list item
indented with spaces</li></ul><p>Code:</p><pre><code>this code block is indented by one tab</code></pre><p>And:</p><pre><code>    this code block is indented by two tabs</code></pre><p>And:</p><pre><code>+   this is an example list item
    indented with tabs
    
+   this is an example list item
    indented with spaces</code></pre>"""
	}
	
	"Tidyness" in
	{
		Markdown( """
> A list within a blockquote:
> 
> *	asterisk 1
> *	asterisk 2
> *	asterisk 3
""" ) shouldBe """<blockquote><p>A list within a blockquote:</p><ul><li>asterisk 1</li><li>asterisk 2</li><li>asterisk 3</li></ul></blockquote>"""
	}

}