package org.dbpedia.spotlight.web.rest.resources

import org.dbpedia.spotlight.model.SpotlightFeedback

object FeedbackParser{

  def fromJson(jsonFeedbackBatch: String): List[SpotlightFeedback] = {
    println("parse json")
    List()
  }

  def fromXml(xmlFeedbackBatch: String): List[SpotlightFeedback] = fromXml(scala.xml.XML.loadString(xmlFeedbackBatch))

  def fromXml(xmlFeedbackBatch:  scala.xml.Elem): List[SpotlightFeedback] = {
    var parsed: List[SpotlightFeedback] = List()

    val feedbackBatch = xmlFeedbackBatch \\ "spotlight_batch_feedback"

    val text: String = (feedbackBatch \\ "text").text.toString
    val docUrlString: String = (feedbackBatch \\ "url").text.toString
    val discourseType:String = (feedbackBatch \\ "discourse_type").text.toString
    val language:String = (feedbackBatch \\ "language").text.toString

    val entities = feedbackBatch \\ "entity"
    entities.foreach{ entity =>
      val entityUri:String = (entity \\ "entity_uri").text.toString
      val surfaceForm:String = (entity \\ "surface_form").text.toString
      var offset:Int = -1 //the default value (a invalid one)
      try{
        offset = (entity \\ "offset").text.toInt
      } catch {
        case e: NumberFormatException => //Do nothing, because the SpotlightFeedback constructor shall validate it, and do not accept the invalid default value.
      }
      val feedback:String = (entity \\ "feedback").text.toString
      val systemIds:String = (entity \\ "systems").text.toString
      val isManual:String = (entity \\ "is_manual").text.toString

      parsed = parsed :+ new SpotlightFeedback(text, docUrlString, discourseType, entityUri, surfaceForm, offset, feedback, systemIds, isManual, language)

    }

    parsed
  }


}