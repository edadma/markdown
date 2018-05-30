package xyz.hyperreal.__markdown__

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

    "Inline HTML (Advanced)" in
    {
        Markdown(
"""
Simple block on one line:

<div>foo</div>

And nested without indentation:

<div>
<div>
<div>
foo
</div>
</div>
<div>bar</div>
</div>
""" ) shouldBe """<p>Simple block on one line:</p><div>foo</div><p>And nested without indentation:</p><div>
<div>
<div>
foo
</div>
</div>
<div>bar</div>
</div>"""
    }

    "Inline HTML (Simple)" in
    {
        Markdown(
"""
Here's a simple block:

<div>
    foo
</div>

This should be a code block, though:

    <div>
        foo
    </div>

As should this:

    <div>foo</div>

Now, nested:

<div>
    <div>
        <div>
            foo
        </div>
    </div>
</div>

This should just be an HTML comment:

<!-- Comment -->

Multiline:

<!--
Blah
Blah
-->

Code block:

    <!-- Comment -->

Just plain comment, with trailing spaces on the line:

<!-- foo -->   

Code:

    <hr />
    
Hr's:

<hr/>

<hr />

<hr/>  

<hr /> 

<hr class="foo" id="bar" />

<hr class="foo" id="bar"/>
""" ) shouldBe """<p>Here's a simple block:</p><div>
    foo
</div><p>This should be a code block, though:</p><pre><code>&lt;div&gt;
    foo
&lt;/div&gt;</code></pre><p>As should this:</p><pre><code>&lt;div&gt;foo&lt;/div&gt;</code></pre><p>Now, nested:</p><div>
    <div>
        <div>
            foo
        </div>
    </div>
</div><p>This should just be an HTML comment:</p><p>Multiline:</p><p>Code block:</p><pre><code>&lt;!-- Comment --&gt;</code></pre><p>Just plain comment, with trailing spaces on the line:</p><p>Code:</p><pre><code>&lt;hr /&gt;</code></pre><p>Hr's:</p><hr/><hr/><hr/><hr/><hr id="bar" class="foo"/><hr id="bar" class="foo"/>"""
    }

    "Inline HTML comments" in
    {
        Markdown(
"""
Paragraph one.

<!-- This is a simple comment -->

<!--
    This is another comment.
-->

Paragraph two.

<!-- one comment block -- -- with two comments -->

The end.
""" ) shouldBe """<p>Paragraph one.</p><p>Paragraph two.</p><p>The end.</p>"""
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

	"Literal quotes in titles" in
	{
		Markdown(
"""
Foo [bar][].

Foo [bar](/url/ "Title with "quotes" inside").


  [bar]: /url/ "Title with "quotes" inside"
""" ) shouldBe """<p>Foo <a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p><p>Foo <a href="/url/" title="Title with &quot;quotes&quot; inside">bar</a>.</p>"""
	}

    "Markdown Documentation - Basics" in
    {
        Markdown(
"""
Markdown: Basics
================

<ul id="ProjectSubmenu">
    <li><a href="/projects/markdown/" title="Markdown Project Page">Main</a></li>
    <li><a class="selected" title="Markdown Basics">Basics</a></li>
    <li><a href="/projects/markdown/syntax" title="Markdown Syntax Documentation">Syntax</a></li>
    <li><a href="/projects/markdown/license" title="Pricing and License Information">License</a></li>
    <li><a href="/projects/markdown/dingus" title="Online Markdown Web Form">Dingus</a></li>
</ul>


Getting the Gist of Markdown's Formatting Syntax
------------------------------------------------

This page offers a brief overview of what it's like to use Markdown.
The [syntax page] [s] provides complete, detailed documentation for
every feature, but Markdown should be very easy to pick up simply by
looking at a few examples of it in action. The examples on this page
are written in a before/after style, showing example syntax and the
HTML output produced by Markdown.

It's also helpful to simply try Markdown out; the [Dingus] [d] is a
web application that allows you type your own Markdown-formatted text
and translate it to XHTML.

**Note:** This document is itself written using Markdown; you
can [see the source for it by adding '.text' to the URL] [src].

  [s]: /projects/markdown/syntax  "Markdown Syntax"
  [d]: /projects/markdown/dingus  "Markdown Dingus"
  [src]: /projects/markdown/basics.text


## Paragraphs, Headers, Blockquotes ##

A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing spaces or tabs is considered
blank.) Normal paragraphs should not be intended with spaces or tabs.

Markdown offers two styles of headers: *Setext* and *atx*.
Setext-style headers for `<h1>` and `<h2>` are created by
"underlining" with equal signs (`=`) and hyphens (`-`), respectively.
To create an atx-style header, you put 1-6 hash marks (`#`) at the
beginning of the line -- the number of hashes equals the resulting
HTML header level.

Blockquotes are indicated using email-style '`>`' angle brackets.

Markdown:

    A First Level Header
    ====================
    
    A Second Level Header
    ---------------------

    Now is the time for all good men to come to
    the aid of their country. This is just a
    regular paragraph.

    The quick brown fox jumped over the lazy
    dog's back.
    
    ### Header 3

    > This is a blockquote.
    > 
    > This is the second paragraph in the blockquote.
    >
    > ## This is an H2 in a blockquote


Output:

    <h1>A First Level Header</h1>
    
    <h2>A Second Level Header</h2>
    
    <p>Now is the time for all good men to come to
    the aid of their country. This is just a
    regular paragraph.</p>
    
    <p>The quick brown fox jumped over the lazy
    dog's back.</p>
    
    <h3>Header 3</h3>
    
    <blockquote>
        <p>This is a blockquote.</p>
        
        <p>This is the second paragraph in the blockquote.</p>
        
        <h2>This is an H2 in a blockquote</h2>
    </blockquote>



### Phrase Emphasis ###

Markdown uses asterisks and underscores to indicate spans of emphasis.

Markdown:

    Some of these words *are emphasized*.
    Some of these words _are emphasized also_.
    
    Use two asterisks for **strong emphasis**.
    Or, if you prefer, __use two underscores instead__.

Output:

    <p>Some of these words <em>are emphasized</em>.
    Some of these words <em>are emphasized also</em>.</p>
    
    <p>Use two asterisks for <strong>strong emphasis</strong>.
    Or, if you prefer, <strong>use two underscores instead</strong>.</p>
   


## Lists ##

Unordered (bulleted) lists use asterisks, pluses, and hyphens (`*`,
`+`, and `-`) as list markers. These three markers are
interchangable; this:

    *   Candy.
    *   Gum.
    *   Booze.

this:

    +   Candy.
    +   Gum.
    +   Booze.

and this:

    -   Candy.
    -   Gum.
    -   Booze.

all produce the same output:

    <ul>
    <li>Candy.</li>
    <li>Gum.</li>
    <li>Booze.</li>
    </ul>

Ordered (numbered) lists use regular numbers, followed by periods, as
list markers:

    1.  Red
    2.  Green
    3.  Blue

Output:

    <ol>
    <li>Red</li>
    <li>Green</li>
    <li>Blue</li>
    </ol>

If you put blank lines between items, you'll get `<p>` tags for the
list item text. You can create multi-paragraph list items by indenting
the paragraphs by 4 spaces or 1 tab:

    *   A list item.
    
        With multiple paragraphs.

    *   Another item in the list.

Output:

    <ul>
    <li><p>A list item.</p>
    <p>With multiple paragraphs.</p></li>
    <li><p>Another item in the list.</p></li>
    </ul>
    


### Links ###

Markdown supports two styles for creating links: *inline* and
*reference*. With both styles, you use square brackets to delimit the
text you want to turn into a link.

Inline-style links use parentheses immediately after the link text.
For example:

    This is an [example link](http://example.com/).

Output:

    <p>This is an <a href="http://example.com/">
    example link</a>.</p>

Optionally, you may include a title attribute in the parentheses:

    This is an [example link](http://example.com/ "With a Title").

Output:

    <p>This is an <a href="http://example.com/" title="With a Title">
    example link</a>.</p>

Reference-style links allow you to refer to your links by names, which
you define elsewhere in your document:

    I get 10 times more traffic from [Google][1] than from
    [Yahoo][2] or [MSN][3].

    [1]: http://google.com/        "Google"
    [2]: http://search.yahoo.com/  "Yahoo Search"
    [3]: http://search.msn.com/    "MSN Search"

Output:

    <p>I get 10 times more traffic from <a href="http://google.com/"
    title="Google">Google</a> than from <a href="http://search.yahoo.com/"
    title="Yahoo Search">Yahoo</a> or <a href="http://search.msn.com/"
    title="MSN Search">MSN</a>.</p>

The title attribute is optional. Link names may contain letters,
numbers and spaces, but are *not* case sensitive:

    I start my morning with a cup of coffee and
    [The New York Times][NY Times].

    [ny times]: http://www.nytimes.com/

Output:

    <p>I start my morning with a cup of coffee and
    <a href="http://www.nytimes.com/">The New York Times</a>.</p>


### Images ###

Image syntax is very much like link syntax.

Inline (titles are optional):

    ![alt text](/path/to/img.jpg "Title")

Reference-style:

    ![alt text][id]

    [id]: /path/to/img.jpg "Title"

Both of the above examples produce the same output:

    <img src="/path/to/img.jpg" alt="alt text" title="Title" />



### Code ###

In a regular paragraph, you can create code span by wrapping text in
backtick quotes. Any ampersands (`&`) and angle brackets (`<` or
`>`) will automatically be translated into HTML entities. This makes
it easy to use Markdown to write about HTML example code:

    I strongly recommend against using any `<blink>` tags.

    I wish SmartyPants used named entities like `&mdash;`
    instead of decimal-encoded entites like `&#8212;`.

Output:

    <p>I strongly recommend against using any
    <code>&lt;blink&gt;</code> tags.</p>
    
    <p>I wish SmartyPants used named entities like
    <code>&amp;mdash;</code> instead of decimal-encoded
    entites like <code>&amp;#8212;</code>.</p>


To specify an entire block of pre-formatted code, indent every line of
the block by 4 spaces or 1 tab. Just like with code spans, `&`, `<`,
and `>` characters will be escaped automatically.

Markdown:

    If you want your page to validate under XHTML 1.0 Strict,
    you've got to put paragraph tags in your blockquotes:

        <blockquote>
            <p>For example.</p>
        </blockquote>

Output:

    <p>If you want your page to validate under XHTML 1.0 Strict,
    you've got to put paragraph tags in your blockquotes:</p>
    
    <pre><code>&lt;blockquote&gt;
        &lt;p&gt;For example.&lt;/p&gt;
    &lt;/blockquote&gt;
    </code></pre>
""" ) shouldBe """<h1>Markdown: Basics</h1><ul id="ProjectSubmenu">
    <li><a title="Markdown Project Page" href="/projects/markdown/">Main</a></li>
    <li><a title="Markdown Basics" class="selected">Basics</a></li>
    <li><a title="Markdown Syntax Documentation" href="/projects/markdown/syntax">Syntax</a></li>
    <li><a title="Pricing and License Information" href="/projects/markdown/license">License</a></li>
    <li><a title="Online Markdown Web Form" href="/projects/markdown/dingus">Dingus</a></li>
</ul><h2>Getting the Gist of Markdown's Formatting Syntax</h2><p>This page offers a brief overview of what it's like to use Markdown.
The <a href="/projects/markdown/syntax" title="Markdown Syntax">syntax page</a> provides complete, detailed documentation for
every feature, but Markdown should be very easy to pick up simply by
looking at a few examples of it in action. The examples on this page
are written in a before/after style, showing example syntax and the
HTML output produced by Markdown.</p><p>It's also helpful to simply try Markdown out; the <a href="/projects/markdown/dingus" title="Markdown Dingus">Dingus</a> is a
web application that allows you type your own Markdown-formatted text
and translate it to XHTML.</p><p><strong>Note:</strong> This document is itself written using Markdown; you
can <a href="/projects/markdown/basics.text">see the source for it by adding '.text' to the URL</a>.</p><h2>Paragraphs, Headers, Blockquotes </h2><p>A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing spaces or tabs is considered
blank.) Normal paragraphs should not be intended with spaces or tabs.</p><p>Markdown offers two styles of headers: <em>Setext</em> and <em>atx</em>.
Setext-style headers for <code>&lt;h1&gt;</code> and <code>&lt;h2&gt;</code> are created by
&quot;underlining&quot; with equal signs (<code>=</code>) and hyphens (<code>-</code>), respectively.
To create an atx-style header, you put 1-6 hash marks (<code>#</code>) at the
beginning of the line -- the number of hashes equals the resulting
HTML header level.</p><p>Blockquotes are indicated using email-style '<code>&gt;</code>' angle brackets.</p><p>Markdown:</p><pre><code>A First Level Header
====================
    
A Second Level Header
---------------------

Now is the time for all good men to come to
the aid of their country. This is just a
regular paragraph.

The quick brown fox jumped over the lazy
dog's back.
    
### Header 3

&gt; This is a blockquote.
&gt; 
&gt; This is the second paragraph in the blockquote.
&gt;
&gt; ## This is an H2 in a blockquote</code></pre><p>Output:</p><pre><code>&lt;h1&gt;A First Level Header&lt;/h1&gt;
    
&lt;h2&gt;A Second Level Header&lt;/h2&gt;
    
&lt;p&gt;Now is the time for all good men to come to
the aid of their country. This is just a
regular paragraph.&lt;/p&gt;
    
&lt;p&gt;The quick brown fox jumped over the lazy
dog's back.&lt;/p&gt;
    
&lt;h3&gt;Header 3&lt;/h3&gt;
    
&lt;blockquote&gt;
    &lt;p&gt;This is a blockquote.&lt;/p&gt;
        
    &lt;p&gt;This is the second paragraph in the blockquote.&lt;/p&gt;
        
    &lt;h2&gt;This is an H2 in a blockquote&lt;/h2&gt;
&lt;/blockquote&gt;</code></pre><h3>Phrase Emphasis </h3><p>Markdown uses asterisks and underscores to indicate spans of emphasis.</p><p>Markdown:</p><pre><code>Some of these words *are emphasized*.
Some of these words _are emphasized also_.
    
Use two asterisks for **strong emphasis**.
Or, if you prefer, __use two underscores instead__.</code></pre><p>Output:</p><pre><code>&lt;p&gt;Some of these words &lt;em&gt;are emphasized&lt;/em&gt;.
Some of these words &lt;em&gt;are emphasized also&lt;/em&gt;.&lt;/p&gt;
    
&lt;p&gt;Use two asterisks for &lt;strong&gt;strong emphasis&lt;/strong&gt;.
Or, if you prefer, &lt;strong&gt;use two underscores instead&lt;/strong&gt;.&lt;/p&gt;</code></pre><h2>Lists </h2><p>Unordered (bulleted) lists use asterisks, pluses, and hyphens (<code>*</code>,
<code>+</code>, and <code>-</code>) as list markers. These three markers are
interchangable; this:</p><pre><code>*   Candy.
*   Gum.
*   Booze.</code></pre><p>this:</p><pre><code>+   Candy.
+   Gum.
+   Booze.</code></pre><p>and this:</p><pre><code>-   Candy.
-   Gum.
-   Booze.</code></pre><p>all produce the same output:</p><pre><code>&lt;ul&gt;
&lt;li&gt;Candy.&lt;/li&gt;
&lt;li&gt;Gum.&lt;/li&gt;
&lt;li&gt;Booze.&lt;/li&gt;
&lt;/ul&gt;</code></pre><p>Ordered (numbered) lists use regular numbers, followed by periods, as
list markers:</p><pre><code>1.  Red
2.  Green
3.  Blue</code></pre><p>Output:</p><pre><code>&lt;ol&gt;
&lt;li&gt;Red&lt;/li&gt;
&lt;li&gt;Green&lt;/li&gt;
&lt;li&gt;Blue&lt;/li&gt;
&lt;/ol&gt;</code></pre><p>If you put blank lines between items, you'll get <code>&lt;p&gt;</code> tags for the
list item text. You can create multi-paragraph list items by indenting
the paragraphs by 4 spaces or 1 tab:</p><pre><code>*   A list item.
    
    With multiple paragraphs.

*   Another item in the list.</code></pre><p>Output:</p><pre><code>&lt;ul&gt;
&lt;li&gt;&lt;p&gt;A list item.&lt;/p&gt;
&lt;p&gt;With multiple paragraphs.&lt;/p&gt;&lt;/li&gt;
&lt;li&gt;&lt;p&gt;Another item in the list.&lt;/p&gt;&lt;/li&gt;
&lt;/ul&gt;</code></pre><h3>Links </h3><p>Markdown supports two styles for creating links: <em>inline</em> and
<em>reference</em>. With both styles, you use square brackets to delimit the
text you want to turn into a link.</p><p>Inline-style links use parentheses immediately after the link text.
For example:</p><pre><code>This is an [example link](http://example.com/).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot;&gt;
example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Optionally, you may include a title attribute in the parentheses:</p><pre><code>This is an [example link](http://example.com/ &quot;With a Title&quot;).</code></pre><p>Output:</p><pre><code>&lt;p&gt;This is an &lt;a href=&quot;http://example.com/&quot; title=&quot;With a Title&quot;&gt;
example link&lt;/a&gt;.&lt;/p&gt;</code></pre><p>Reference-style links allow you to refer to your links by names, which
you define elsewhere in your document:</p><pre><code>I get 10 times more traffic from [Google][1] than from
[Yahoo][2] or [MSN][3].

[1]: http://google.com/        &quot;Google&quot;
[2]: http://search.yahoo.com/  &quot;Yahoo Search&quot;
[3]: http://search.msn.com/    &quot;MSN Search&quot;</code></pre><p>Output:</p><pre><code>&lt;p&gt;I get 10 times more traffic from &lt;a href=&quot;http://google.com/&quot;
title=&quot;Google&quot;&gt;Google&lt;/a&gt; than from &lt;a href=&quot;http://search.yahoo.com/&quot;
title=&quot;Yahoo Search&quot;&gt;Yahoo&lt;/a&gt; or &lt;a href=&quot;http://search.msn.com/&quot;
title=&quot;MSN Search&quot;&gt;MSN&lt;/a&gt;.&lt;/p&gt;</code></pre><p>The title attribute is optional. Link names may contain letters,
numbers and spaces, but are <em>not</em> case sensitive:</p><pre><code>I start my morning with a cup of coffee and
[The New York Times][NY Times].

[ny times]: http://www.nytimes.com/</code></pre><p>Output:</p><pre><code>&lt;p&gt;I start my morning with a cup of coffee and
&lt;a href=&quot;http://www.nytimes.com/&quot;&gt;The New York Times&lt;/a&gt;.&lt;/p&gt;</code></pre><h3>Images </h3><p>Image syntax is very much like link syntax.</p><p>Inline (titles are optional):</p><pre><code>![alt text](/path/to/img.jpg &quot;Title&quot;)</code></pre><p>Reference-style:</p><pre><code>![alt text][id]

[id]: /path/to/img.jpg &quot;Title&quot;</code></pre><p>Both of the above examples produce the same output:</p><pre><code>&lt;img src=&quot;/path/to/img.jpg&quot; alt=&quot;alt text&quot; title=&quot;Title&quot; /&gt;</code></pre><h3>Code </h3><p>In a regular paragraph, you can create code span by wrapping text in
backtick quotes. Any ampersands (<code>&amp;</code>) and angle brackets (<code>&lt;</code> or
<code>&gt;</code>) will automatically be translated into HTML entities. This makes
it easy to use Markdown to write about HTML example code:</p><pre><code>I strongly recommend against using any `&lt;blink&gt;` tags.

I wish SmartyPants used named entities like `&amp;mdash;`
instead of decimal-encoded entites like `&amp;#8212;`.</code></pre><p>Output:</p><pre><code>&lt;p&gt;I strongly recommend against using any
&lt;code&gt;&amp;lt;blink&amp;gt;&lt;/code&gt; tags.&lt;/p&gt;
    
&lt;p&gt;I wish SmartyPants used named entities like
&lt;code&gt;&amp;amp;mdash;&lt;/code&gt; instead of decimal-encoded
entites like &lt;code&gt;&amp;amp;#8212;&lt;/code&gt;.&lt;/p&gt;</code></pre><p>To specify an entire block of pre-formatted code, indent every line of
the block by 4 spaces or 1 tab. Just like with code spans, <code>&amp;</code>, <code>&lt;</code>,
and <code>&gt;</code> characters will be escaped automatically.</p><p>Markdown:</p><pre><code>If you want your page to validate under XHTML 1.0 Strict,
you've got to put paragraph tags in your blockquotes:

    &lt;blockquote&gt;
        &lt;p&gt;For example.&lt;/p&gt;
    &lt;/blockquote&gt;</code></pre><p>Output:</p><pre><code>&lt;p&gt;If you want your page to validate under XHTML 1.0 Strict,
you've got to put paragraph tags in your blockquotes:&lt;/p&gt;
    
&lt;pre&gt;&lt;code&gt;&amp;lt;blockquote&amp;gt;
    &amp;lt;p&amp;gt;For example.&amp;lt;/p&amp;gt;
&amp;lt;/blockquote&amp;gt;
&lt;/code&gt;&lt;/pre&gt;</code></pre>"""
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