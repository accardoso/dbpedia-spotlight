package org.dbpedia.spotlight.model

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.JavaConverters._

/**
 * Unit test for class org.dbpedia.spotlight.model.Paragraph.scala.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class ParagraphTest extends FlatSpec with ShouldMatchers {

  val inputNonEmptyId: String = "paragraph-0001"
  val inputText: Text = new Text("First documented in the 13th century, Berlin was the capital of the Kingdom of Prussia (1701–1918), the German Empire (1871–1918), the Weimar Republic (1919–33), the Third Reich (1933–45) and is the current unified Germany's capital.")
  val inputOccs: List[SurfaceFormOccurrence] = List(new SurfaceFormOccurrence(new SurfaceForm("Berlin"), inputText, 39), new SurfaceFormOccurrence(new SurfaceForm("Kingdom of Prussia"), inputText, 68))

  val nonemptyIdParagraph: Paragraph = new Paragraph(inputNonEmptyId, inputText, inputOccs)
  val emptyIdParagraph: Paragraph = new Paragraph(inputText, inputOccs)
  
  "The Paragraph default id" should "be empty" in {
    emptyIdParagraph.id.equals("") should be === true
  }

  "The Paragraph constructor" should "accept empty attributes" in {
    val paragraph: Paragraph = new Paragraph("", new Text(""), List[SurfaceFormOccurrence]())
    
    paragraph.id.equals("") should be === true
    paragraph.text.equals(new Text("")) should be === true
    paragraph.occurrences.isEmpty should be === true
  }

  "The Paragraph attributes" should "be got using their names" in {
    nonemptyIdParagraph.id.equals(inputNonEmptyId) should be === true
    nonemptyIdParagraph.text.equals(inputText) should be === true
    nonemptyIdParagraph.occurrences.equals(inputOccs) should be === true

    emptyIdParagraph.id.equals("") should be === true
    emptyIdParagraph.text.equals(inputText) should be === true
    emptyIdParagraph.occurrences.equals(inputOccs) should be === true
  }

  "The Paragraph occurrences attribute" should "be got as a java list using it get method" in {
    nonemptyIdParagraph.getOccurrences.equals(nonemptyIdParagraph.occurrences.asJava) should be === true
  }

  /* Todo Activate this test after equals method implementation at Paragraph class
 "Two Paragraphs with only different IDs" should "be equals" in {
   nonemptyIdParagraph.equals(emptyIdParagraph)
 } */

  "The toString of Paragraph instance" should "inform its id, the first 50 char of the text and the occurrences" in {
    val expectedEmptyIdToString: String = "Text[First documented in the 13th century, Berlin was t ...]SurfaceForm[Berlin] - at position *39* in - Text[... First documented in the 13th century, Berlin was the capital of the Kingdom of Prussia (1 ...]\nSurfaceForm[Kingdom of Prussia] - at position *68* in - Text[... n the 13th century, Berlin was the capital of the Kingdom of Prussia (1701–1918), the German Empire  ...]"
    emptyIdParagraph.toString.equals(expectedEmptyIdToString) should be === true
    nonemptyIdParagraph.toString.equals(inputNonEmptyId + ": " + expectedEmptyIdToString) should be === true
  }

}