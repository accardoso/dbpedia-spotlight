package org.dbpedia.spotlight.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Unit test for class org.dbpedia.spotlight.model.Token.scala.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class TokenTest extends FlatSpec with ShouldMatchers {
  
  val inputToken: String = "Berlin"
  val inputOffset: Int = 517
  val inputTokenType1: TokenType = TokenType.UNKNOWN
  val inputTokenType2: TokenType = TokenType.STOPWORD

  val token1: Token = new Token(inputToken, inputOffset, inputTokenType1)
  val token2: Token = new Token(inputToken, inputOffset + 10, inputTokenType2)

  "The Token instance hash code" should "be equal to hash code of the string: '<token>, <offset>'" in {
    token1.hashCode() should be === "%s, %d".format(inputToken, inputOffset).hashCode
  }

  "Two Token instances" should "be equal if theirs token attribute is equal (do not matter the offset or the tokenType)" in {
    token1.equals(token2) should be === true

    val token3: Token = new Token("", inputOffset, inputTokenType1)
    token1.equals(token3) should be === false
  }

  "The Token toString" should "inform the token, the tokenType id and the tokenType count" in {
    token1.toString.equals(inputToken+" (ID: "+inputTokenType1.id+", count: "+inputTokenType1.count+")") should be === true
  }

  /* todo Fixes the Token.toString error. When tokenType is null, tokenType.id is not valid
  it should "inform the token, the tokenType id and the tokenType count as 'unknown', if the tokenType is null" in {
    token1.tokenType = null
    token1.toString.equals(inputToken+" (ID: "+inputTokenType1.id+", count: unknown)") should be === true
  }*/

}
