package xyz.hyperreal.markdown


object Main extends App {

  val input =
    """
      |Here's a simple block:
      |
      |<div>
      |    foo
      |</div>
    """.trim.stripMargin

  val doc = Markdown( input )

  Util.headingIds( doc )

  val html = Util.html( doc, 2 )

  println( html )

}

/*
<p>These should all get escaped:</p><p>Backslash: &bsol;</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: &lcub;</p><p>Right brace: &rcub;</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: &bsol;&bsol;

Backtick: &bsol;`

Asterisk: &bsol;*

Underscore: &bsol;_

Left brace: &bsol;&lcub;

Right brace: &bsol;&rcub;

Left bracket: &bsol;[

Right bracket: &bsol;]

Left paren: &bsol;(

Right paren: &bsol;)

Greater-than: &bsol;&gt;

Hash: &bsol;#

Period: &bsol;.

Bang: &bsol;!

Plus: &bsol;+

Minus: &bsol;-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash: <code>&bsol;&bsol;</code></p><p>Backtick: <code>&bsol;`</code></p><p>Asterisk: <code>&bsol;*</code></p><p>Underscore: <code>&bsol;_</code></p><p>Left brace: <code>&bsol;&lcub;</code></p><p>Right brace: <code>&bsol;&rcub;</code></p><p>Left bracket: <code>&bsol;[</code></p><p>Right bracket: <code>&bsol;]</code></p><p>Left paren: <code>&bsol;(</code></p><p>Right paren: <code>&bsol;)</code></p><p>Greater-than: <code>&bsol;&gt;</code></p><p>Hash: <code>&bsol;#</code></p><p>Period: <code>&bsol;.</code></p><p>Bang: <code>&bsol;!</code></p><p>Plus: <code>&bsol;+</code></p><p>Minus: <code>&bsol;-</code></p>
*/

/*
<p>These should all get escaped:</p><p>Backslash: \</p><p>Backtick: `</p><p>Asterisk: *</p><p>Underscore: _</p><p>Left brace: {</p><p>Right brace: }</p><p>Left bracket: [</p><p>Right bracket: ]</p><p>Left paren: (</p><p>Right paren: )</p><p>Greater-than: &gt;</p><p>Hash: #</p><p>Period: .</p><p>Bang: !</p><p>Plus: +</p><p>Minus: -</p><p>These should not, because they occur within a code block:</p><pre><code>Backslash: \\

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

Minus: \-</code></pre><p>Nor should these, which occur in code spans:</p><p>Backslash: <code>\\</code></p><p>Backtick: <code>\`</code></p><p>Asterisk: <code>\*</code></p><p>Underscore: <code>\_</code></p><p>Left brace: <code>\{</code></p><p>Right brace: <code>\}</code></p><p>Left bracket: <code>\[</code></p><p>Right bracket: <code>\]</code></p><p>Left paren: <code>\(</code></p><p>Right paren: <code>\)</code></p><p>Greater-than: <code>\&gt;</code></p><p>Hash: <code>\#</code></p><p>Period: <code>\.</code></p><p>Bang: <code>\!</code></p><p>Plus: <code>\+</code></p><p>Minus: <code>\-</code></p>*/
