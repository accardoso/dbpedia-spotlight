package org.dbpedia.spotlight.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Unit test for class org.dbpedia.spotlight.model.SurfaceForm.scala.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class SurfaceFormTest extends FlatSpec with ShouldMatchers {
  
  val inputSfName: String = "Bob’s"
  val expectedSfName: String = "Bob's"

  val manualID: Int = 1
  val manualAnnotatedCount: Int = 10
  val manualTotalCount: Int = 100

  val sfDefault: SurfaceForm = new SurfaceForm(inputSfName)
  val sfManual: SurfaceForm = new SurfaceForm(inputSfName, manualID, manualAnnotatedCount, manualTotalCount)

  "The SurfaceForm constructor" should "clean up the name attribute correctly" in {
    sfDefault.name.equals(expectedSfName) should be === true
    sfManual.name.equals(expectedSfName) should be === true
  }
  
  it should "initialize with 0 (zero) for uninformed attributes" in {
    sfDefault.id should be === 0
    sfDefault.annotatedCount should be === 0
    sfDefault.totalCount should be === 0
  }

  it should "initialize with the informed attributes" in {
    sfDefault.id should be === manualID
    sfDefault.annotatedCount should be === manualAnnotatedCount
    sfDefault.totalCount should be === manualTotalCount
  }

  /*todo Uncomment this (already done) unit test after implement this feature to the SurfaceForm class
  it should "not allow invalid attributes" in {
    //Could not accept negative counts
    var allow = true
    try{
      val temp: SurfaceForm = new SurfaceForm("", -1, -10, -100)
    } catch {
      case e: IllegalArgumentException => allow = false
    }
    allow should be === false

    //Could not accept annotationCount > totalCount
    allow = true
    try{
      val temp: SurfaceForm = new SurfaceForm("", 1, 100, 10)
    } catch {
      case e: IllegalArgumentException => allow = false
    }
    allow should be === false
  }*/

  "The SurfaceForm instance toString" should "be 'SurfaceForm[<the name attribute>]' " in {
    sfDefault.toString.equals("SurfaceForm["+expectedSfName+"]") should be === true
  }

  "Two SurfaceForm instances" should "be equal if and only if both name attribute are equal" in {
    sfDefault.equals(sfManual) should be === true
  }

  it should "have the same hash code if theis names are equals (do not metter the other attributes)" in{
    sfDefault.hashCode() should be === sfManual.hashCode
  }

  "The SurfaceForm instance hash code" should "be equal to its name attribute hash code" in {
    sfDefault.hashCode() should be === sfDefault.name.hashCode
  }

  it should "be 0 (zero) if the name attribute is null (note: null is different of empty: \"\")" in {
    val temp: SurfaceForm = new SurfaceForm("")
    temp.hashCode() should be === "".hashCode

    temp.name = null
    temp.hashCode() should be === 0
  }

  "The SurfaceForm instance annotation probability" should "be correctly calculated" in {
    sfManual.annotationProbability should be === manualAnnotatedCount / manualTotalCount.toDouble
  }

  /*todo This unit test after implement this feature to the SurfaceForm class
  it should "treat division by zero (when totalCount attribute is zero)" in {
    //Not implemented at the SurfaceForm class yet
  }*/
  
}
