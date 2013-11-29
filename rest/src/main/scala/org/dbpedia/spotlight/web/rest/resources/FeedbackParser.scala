package org.dbpedia.spotlight.web.rest.resources

import org.dbpedia.spotlight.model.SpotlightFeedback

/**
 * This object implements a parser from a batch of feedback xml/json formatted to a list (batch) of SpotlightFeedback
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object FeedbackParser{

  /* Parses the informed batch of feedback in the json formatted string to a list (batch) of SpotlightFeedback */
  def fromJson(jsonFeedbackBatch: String): List[SpotlightFeedback] = {
    var parsed: List[SpotlightFeedback] = List()
    val result = scala.util.parsing.json.JSON.parseFull(jsonFeedbackBatch)
    var resultMap: Map[String, Any] = null

    //Get the json batch root
    result match {
      // Matches if jsonStr is valid JSON and represents a Map of Strings to Any
      case Some(map: Map[String, Any]) => resultMap = map
      case None => throw new Exception("Parsing failed.")
      case other => throw new IllegalArgumentException("Unknown data structure: " + other)
    }

    //Parse the common fields
    val text: String = resultMap("text") match {
      case s: String => s
    }
    val docUrl: String = resultMap("url") match {
      case s: String => s
    }
    val gender: String = resultMap("gender") match {
      case s: String => s
    }
    val language: String = resultMap("language") match {
      case s: String => s
    }

    //Get the entities list
    val entities: List[Map[String, Any]] = resultMap("entities") match {
      case l: List[Map[String, Any]] => l
    }
    //Parse the specific fields
    entities.foreach{ entity =>
      val entityUri:String = entity("entity_uri") match {
        case s: String => s
      }
      val surfaceForm:String = entity("surface_form") match {
        case s: String => s
      }
      var offset:Int = -1 //the default value (a invalid one)
      try{
        offset = entity("offset") match {
          case d: Double => d.toInt
          case i: Int => i
          case s: Short => s.toInt
          case f: Float => f.toInt
          case l: Long => l.toInt
          case s: String => s.toInt
          case b: BigInt => b.toInt
          case b: BigDecimal => b.toInt
        }
      } catch {
        case e: NumberFormatException => //Do nothing, because the SpotlightFeedback constructor shall validate it, and do not accept the invalid default value.
      }
      val feedback:String = entity("feedback") match {
        case s: String => s
      }
      val systemIds:String = entity("systems") match {
        case s: String => s
      }
      val manual:String = entity("manual") match {
        case s: String => s
      }

      //Create a new SpotlightFeedback for this entity using the common and specific fields and add it to the list of parsed feedback
      parsed = parsed :+ new SpotlightFeedback(text, docUrl, gender, entityUri, surfaceForm, offset, feedback, systemIds, manual, language)
    }

    //Return all feedback of the batch as a SpotlightFeedback
    parsed
  }

  /* Parses the informed batch of feedback in the xml formatted string to a list (batch) of SpotlightFeedback */
  def fromXml(xmlFeedbackBatch: String): List[SpotlightFeedback] = fromXml(scala.xml.XML.loadString(xmlFeedbackBatch))
  /* Parses the informed batch of feedback in the xml (scala element) to a list (batch) of SpotlightFeedback */
  def fromXml(xmlFeedbackBatch:  scala.xml.Elem): List[SpotlightFeedback] = {
    var parsed: List[SpotlightFeedback] = List()

    //Get the xml batch root
    val feedbackBatch = xmlFeedbackBatch \\ "batch"

    //Parse the common fields
    val text: String = (feedbackBatch \\ "text").text.toString
    val docUrl: String = (feedbackBatch \\ "url").text.toString
    val gender:String = (feedbackBatch \\ "gender").text.toString
    val language:String = (feedbackBatch \\ "language").text.toString

    //Get the entities list
    val entities = feedbackBatch \\ "entity"
    //Parse the specific fields
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
      val manual:String = (entity \\ "manual").text.toString

      //Create a new SpotlightFeedback for this entity using the common and specific fields and add it to the list of parsed feedback
      parsed = parsed :+ new SpotlightFeedback(text, docUrl, gender, entityUri, surfaceForm, offset, feedback, systemIds, manual, language)

    }

    //Return all feedback of the batch as a SpotlightFeedback
    parsed
  }

}