package org.dbpedia.spotlight.web.rest.resources

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.dbpedia.spotlight.model.SpotlightFeedback

/**
 * This ScalaTest test for the xml and json feedback batch parsers, which are implemented at the class FeedbackParser.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class FeedbackParserTest extends FlatSpec with ShouldMatchers {

  "The feedback batch in xml and json" should "be equivalent to each other" in {
    FeedbackParser.fromXml(FeedbackParserTest.batchXml).toString() should be === FeedbackParser.fromJson(FeedbackParserTest.batchJson).toString()
  }

  "The xml feedback batch" should "be equivalent to the expected tsv storage" in {
    FeedbackParser.fromXml(FeedbackParserTest.batchXml).toString() should be === FeedbackParserTest.batchSpotlightFeedback.toString()
  }

  "The json feedback batch" should "be equivalent to the expected tsv storage" in {
    FeedbackParser.fromJson(FeedbackParserTest.batchJson).toString() should be === FeedbackParserTest.batchSpotlightFeedback.toString()
  }

}

object FeedbackParserTest {

  /* The same feedback batch in 3 different formats: tsv storage, xml batch and json batch*/
  val batchSpotlightFeedback: List[SpotlightFeedback] = List(
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Johann_Wolfgang_von_Goethe", "Goethe",	211,	"correct",	"spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Leipzig", "Leipzig", 202, "correct", "spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Christian_Fürchtegott_Gellert", "Christian Fürchtegott Gellert", 168, "correct", "spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Anna_Katharina_Schönkopf", "Käthchen Schönkopf", 236, "correct", "spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Rococo",	"Rococo", 298, "correct", "spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Auerbachs_Keller",	"Auerbachs Keller", 414, "correct", "spotlight_statistical alchemy_api zemanta", "false", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Goethe's_Faust",	"Faust", 362, "incorrect", "user_1", "true", "english"),
    new SpotlightFeedback("Goethe studied law in University of Leipzig from 1765 to 1768. He detested learning age-old judicial rules by heart, preferring instead to attend the poetry lessons of Christian Fürchtegott Gellert. In Leipzig, Goethe fell in love with Käthchen Schönkopf and wrote cheerful verses about her in the Rococo genre. The restaurant Auerbachs Keller and its legend of Faust's 1525 barrel ride impressed him so much that Auerbachs Keller became the only real place in his closet drama Faust Part One.", "http://rdface.aksw.org/new/tinymce/examples/rdface.html", "biography", "http://dbpedia.org/resource/Leipzig_University",	"University of Leipzig", 22, "correct", "user_1", "true", "english")
  )
  //TODO use the samples xml and json from a web repo
  val batchXml: String = scala.io.Source.fromURL("file:///home/alexandre/Projects/test-files-spotlight/feedback-interface/batch.xml").mkString("")
  val batchJson: String = scala.io.Source.fromURL("file:///home/alexandre/Projects/test-files-spotlight/feedback-interface/batch.json").mkString("")
}