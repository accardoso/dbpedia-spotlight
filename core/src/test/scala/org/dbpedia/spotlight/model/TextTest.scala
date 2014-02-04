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
class TextTest extends FlatSpec with ShouldMatchers {

  val input: String = "Germany’s capital is Berlin.\nThe cities below are not the Brazil’s capital:\n Rio de Janeiro \t São Paulo \t Buenos Aires."
  val expected: String = "Germany's capital is Berlin.\nThe cities below are not the Brazil's capital:\n Rio de Janeiro \t São Paulo \t Buenos Aires."
  val textInstance: Text = new Text(input)

  "The Text constructor" should "clean up the text attribute correctly" in {
    textInstance.text.equals(expected) should be === true
  }

  "The Text instance toString" should "be 'Text[<the text attribute>]' " in {
    textInstance.toString.equals("Text["+expected+"]") should be === true
  }

  "Two Text instances" should "be equal if and only if both text attribute are equal" in {
    textInstance.equals(new Text(textInstance.text)) should be === true
  }

  "The Text instance hash code" should "be equal to its text attribute hash code" in {
    textInstance.hashCode() should be === textInstance.text.hashCode
  }

  it should "be 0 (zero) if the text attribute is null (note: null is different of empty: \"\")" in {
    val temp: Text = new Text("")
    temp.hashCode() should be === "".hashCode

    temp.text = null
    temp.hashCode() should be === 0
  }
}

