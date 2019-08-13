package xyz.hyperreal.markdown

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks


class Emphasis extends FreeSpec with ScalaCheckPropertyChecks with Matchers with Tests
{
	"italic" in
	{
		markdown( "*asdf*" ) shouldBe "<p><em>asdf</em></p>"
		markdown( "_asdf_" ) shouldBe "<p><em>asdf</em></p>"
		markdown( "_asdf*" ) shouldBe "<p>_asdf*</p>"
		markdown( "*asdf_" ) shouldBe "<p>*asdf_</p>"
		markdown( "*as_df*" ) shouldBe "<p><em>as_df</em></p>"
		markdown( "_as*df_" ) shouldBe "<p><em>as*df</em></p>"
		markdown( "_a*s_ df*" ) shouldBe "<p><em>a*s</em> df*</p>"
		markdown( "_a*s_ df_" ) shouldBe "<p><em>a*s</em> df_</p>"
		markdown( "_a*s*df_" ) shouldBe "<p><em>a*s*df</em></p>"
		markdown( "_a_ s*df_" ) shouldBe "<p><em>a</em> s*df_</p>"
		markdown( "*a_s*df_" ) shouldBe "<p><em>a_s</em>df_</p>"
		markdown( "_ a_" ) shouldBe "<p>_ a_</p>"
		markdown( "* a*" ) shouldBe "<ul><li>a*</li></ul>"
		markdown( "_a*s _df*" ) shouldBe "<p>_a<em>s _df</em></p>"
		markdown( "_a*s _df_" ) shouldBe "<p><em>a*s _df</em></p>"
		markdown( "_a _s*df_" ) shouldBe "<p><em>a _s*df</em></p>"
	}

	"underscore in word" in
	{
		markdown( "_a*s_df*" ) shouldBe "<p>_a<em>s_df</em></p>"
		markdown( "_a*s_df_" ) shouldBe "<p><em>a*s_df</em></p>"
		markdown( "_a_s*df_" ) shouldBe "<p><em>a_s*df</em></p>"
		markdown( "_a_s_" ) shouldBe "<p><em>a_s</em></p>"
		markdown( "a_s_" ) shouldBe "<p>a_s_</p>"
		markdown( "_a_s" ) shouldBe "<p>_a_s</p>"
		markdown( "a_s" ) shouldBe "<p>a_s</p>"
	}
	
	"strong" in
	{
		markdown( "**asdf**" ) shouldBe "<p><strong>asdf</strong></p>"
		markdown( "__asdf__" ) shouldBe "<p><strong>asdf</strong></p>"
		markdown( "__asdf**" ) shouldBe "<p>__asdf**</p>"
		markdown( "**asdf__" ) shouldBe "<p>**asdf__</p>"
		markdown( "**as__df**" ) shouldBe "<p><strong>as__df</strong></p>"
		markdown( "__as**df__" ) shouldBe "<p><strong>as**df</strong></p>"
		markdown( "__a**s__ df**" ) shouldBe "<p><strong>a**s</strong> df**</p>"
		markdown( "__a**s__ df__" ) shouldBe "<p><strong>a**s</strong> df__</p>"
		markdown( "__a**s**df__" ) shouldBe "<p><strong>a**s**df</strong></p>"
		markdown( "__a__ s**df__" ) shouldBe "<p><strong>a</strong> s**df__</p>"
		markdown( "**a__s**df__" ) shouldBe "<p><strong>a__s</strong>df__</p>"
		markdown( "__ a__" ) shouldBe "<p>__ a__</p>"
		markdown( "** a**" ) shouldBe "<p>** a**</p>"
		markdown( "__a**s __df**" ) shouldBe "<p>__a<strong>s __df</strong></p>"
 		markdown( "__a**s __df__" ) shouldBe "<p><strong>a**s __df</strong></p>"
 		markdown( "__a __s**df__" ) shouldBe "<p><strong>a __s**df</strong></p>"
	}
	
	"mixing" in
	{
		markdown( "*as**df*" ) shouldBe "<p><em>as**df</em></p>"
		markdown( "*as__df*" ) shouldBe "<p><em>as__df</em></p>"
		markdown( "_as**df_" ) shouldBe "<p><em>as**df</em></p>"
		markdown( "_as__df_" ) shouldBe "<p><em>as__df</em></p>"
		markdown( "**as*df**" ) shouldBe "<p><strong>as*df</strong></p>"
		markdown( "**as_df**" ) shouldBe "<p><strong>as_df</strong></p>"
		markdown( "__as*df__" ) shouldBe "<p><strong>as*df</strong></p>"
		markdown( "__as_df__" ) shouldBe "<p><strong>as_df</strong></p>"
	}
}