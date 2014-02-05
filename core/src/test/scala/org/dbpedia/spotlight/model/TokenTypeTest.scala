package org.dbpedia.spotlight.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Unit test for class org.dbpedia.spotlight.model.Text.scala, which represents a input string for spotlight.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class TokenTypeTest extends FlatSpec with ShouldMatchers {

  val inputId: Int = 10
  val inputTypeStr: String = "TEST"
  val inputCount: Int = 50

  val tokenType: TokenType = new TokenType(inputId, inputTypeStr, inputCount)

  "The TokenType instance hash code" should "be equals to the attribute tokenType hash code" in {
    tokenType.hashCode() should be === inputTypeStr.hashCode
    tokenType.hashCode() should be === tokenType.tokenType.hashCode
  }

  "The default token types UNKNOWN and STOPWORD" should "differ only by the tokenType attribute" in {
    TokenType.UNKNOWN.id should be === TokenType.STOPWORD.id
    TokenType.UNKNOWN.count should be === TokenType.STOPWORD.count

    TokenType.UNKNOWN.tokenType.equals(TokenType.STOPWORD.tokenType) should be === false
  }

  it should "have id 0" in{
    TokenType.UNKNOWN.id should be === 0
    TokenType.STOPWORD.id should be === 0
  }

  "The TokenType toString" should "inform the tokenType and the count attributes" in {
    TokenType.UNKNOWN.toString.equals("<<UNKNOWN>> (1)") should be === true
    TokenType.STOPWORD.toString.equals("<<STOPWORD>> (1)") should be === true
    tokenType.toString.equals(inputTypeStr + " (" + inputCount +")") should be === true
  }

  "Two TokenType instances" should "be equal if they have the same tokenType attribute (do not matter the id and the count)" in {
    TokenType.UNKNOWN.equals(TokenType.STOPWORD) should be === false
    TokenType.UNKNOWN.equals(tokenType) should be === false

    tokenType.equals(new TokenType(inputId+1, inputTypeStr, inputCount+20)) should be === true
  }
}