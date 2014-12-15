package ca.hyperreal.markdown

import org.scalatest._
import prop.PropertyChecks


class Emphasis extends FreeSpec with PropertyChecks with Matchers
{
	"italic" in
	{
		Markdown( "*asdf*" ) shouldBe "<p><em>asdf</em></p>"
		Markdown( "_asdf_" ) shouldBe "<p><em>asdf</em></p>"
		Markdown( "_asdf*" ) shouldBe "<p>_asdf*</p>"
		Markdown( "*asdf_" ) shouldBe "<p>*asdf_</p>"
		Markdown( "*as_df*" ) shouldBe "<p><em>as_df</em></p>"
		Markdown( "_as*df_" ) shouldBe "<p><em>as*df</em></p>"
		Markdown( "_a*s_ df*" ) shouldBe "<p><em>a*s</em> df*</p>"
		Markdown( "_a*s_ df_" ) shouldBe "<p><em>a*s</em> df_</p>"
		Markdown( "_a*s*df_" ) shouldBe "<p><em>a*s*df</em></p>"
		Markdown( "_a_ s*df_" ) shouldBe "<p><em>a</em> s*df_</p>"
		Markdown( "*a_s*df_" ) shouldBe "<p><em>a_s</em>df_</p>"
		Markdown( "_ a_" ) shouldBe "<p>_ a_</p>"
		Markdown( "* a*" ) shouldBe "<ul><li>a*</li></ul>"
		Markdown( "_a*s _df*" ) shouldBe "<p>_a<em>s _df</em></p>"
		Markdown( "_a*s _df_" ) shouldBe "<p><em>a*s _df</em></p>"
		Markdown( "_a _s*df_" ) shouldBe "<p><em>a _s*df</em></p>"
	}

	"underscore in word" in
	{
		Markdown( "_a*s_df*" ) shouldBe "<p>_a<em>s_df</em></p>"
		Markdown( "_a*s_df_" ) shouldBe "<p><em>a*s_df</em></p>"
		Markdown( "_a_s*df_" ) shouldBe "<p><em>a_s*df</em></p>"
		Markdown( "_a_s_" ) shouldBe "<p><em>a_s</em></p>"
		Markdown( "a_s_" ) shouldBe "<p>a_s_</p>"
		Markdown( "_a_s" ) shouldBe "<p>_a_s</p>"
		Markdown( "a_s" ) shouldBe "<p>a_s</p>"
	}
	
	"strong" in
	{
		Markdown( "**asdf**" ) shouldBe "<p><strong>asdf</strong></p>"
		Markdown( "__asdf__" ) shouldBe "<p><strong>asdf</strong></p>"
		Markdown( "__asdf**" ) shouldBe "<p>__asdf**</p>"
		Markdown( "**asdf__" ) shouldBe "<p>**asdf__</p>"
		Markdown( "**as__df**" ) shouldBe "<p><strong>as__df</strong></p>"
		Markdown( "__as**df__" ) shouldBe "<p><strong>as**df</strong></p>"
		Markdown( "__a**s__ df**" ) shouldBe "<p><strong>a**s</strong> df**</p>"
		Markdown( "__a**s__ df__" ) shouldBe "<p><strong>a**s</strong> df__</p>"
		Markdown( "__a**s**df__" ) shouldBe "<p><strong>a**s**df</strong></p>"
		Markdown( "__a__ s**df__" ) shouldBe "<p><strong>a</strong> s**df__</p>"
		Markdown( "**a__s**df__" ) shouldBe "<p><strong>a__s</strong>df__</p>"
		Markdown( "__ a__" ) shouldBe "<p>__ a__</p>"
		Markdown( "** a**" ) shouldBe "<p>** a**</p>"
		Markdown( "__a**s __df**" ) shouldBe "<p>__a<strong>s __df</strong></p>"
 		Markdown( "__a**s __df__" ) shouldBe "<p><strong>a**s __df</strong></p>"
 		Markdown( "__a __s**df__" ) shouldBe "<p><strong>a __s**df</strong></p>"
	}
	
	"mixing" in
	{
		Markdown( "*as**df*" ) shouldBe "<p><em>as**df</em></p>"
		Markdown( "*as__df*" ) shouldBe "<p><em>as__df</em></p>"
		Markdown( "_as**df_" ) shouldBe "<p><em>as**df</em></p>"
		Markdown( "_as__df_" ) shouldBe "<p><em>as__df</em></p>"
		Markdown( "**as*df**" ) shouldBe "<p><strong>as*df</strong></p>"
		Markdown( "**as_df**" ) shouldBe "<p><strong>as_df</strong></p>"
		Markdown( "__as*df__" ) shouldBe "<p><strong>as*df</strong></p>"
		Markdown( "__as_df__" ) shouldBe "<p><strong>as_df</strong></p>"
	}
}