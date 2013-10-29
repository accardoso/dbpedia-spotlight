package org.dbpedia.spotlight.io.feedback

import java.net.URL
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResource, Text}
import org.dbpedia.spotlight.exceptions.InputException

/**
 *
 * @author pablomendes
 * @author Alexandre Cançado Cardoso - accardoso
 */

object FeedbackValidator {

  val feedbackPossibilities: List[String] = List("correct", "incorrect")

  val automaticSystemsIds: List[String] = List("spotlight_lucene", "spotlight_statistical", "alchemy_api", "zemanta", "open_calais")


  def validateAndStandardize(clientIp:String, key: String, text: String, docUrlString: String, discourseType: String, entityUri: String,
                             entityUriSuggestion: String, surfaceForm: String, offset: Int, feedback: String, systemIds: String,
                             isManualFeedback: Boolean, language: String) : StandardFeedback = {

    // Obligatory fields filled validation
    if (text == "")
      throw new InputException("text must be filled!")
    if (entityUri == "")
      throw new InputException("entity_uri must be filled!")
    if (systemIds == "")
      throw new InputException("systems must be filled!")
    if (surfaceForm == "")
      throw new InputException("surface_form must be filled!")
    if (offset<=0)
      throw new InputException("offset must be filled with a entity position (a positive value)!")
    //TODO language validation or language classification

    // Feedback standardization and validation
    val standardFeedback =feedback.toLowerCase
    if(!feedbackPossibilities.contains(standardFeedback))
      throw new IllegalArgumentException("\"%s\" is not a valid feedback. List of valid ones: %s".format(feedback, feedbackPossibilities.toString()))

    // SystemIds and isManualFeedback standardization and validation
    val systems: Array[String] = systemIds.toLowerCase.split(" ")
    if(!isManualFeedback){
      for (system:String <- systems){
        if(!automaticSystemsIds.contains(system))
          throw new IllegalArgumentException("\"%s\" is not registered automatic system. List of valid ones: %s . If it is a new system, please contact us for registration.".format(system, automaticSystemsIds))
      }
    }

    // Document url translation from String to URL or generation from the text
    var docUrl: URL = null
    if (!(docUrlString == "")) {
      try {
        docUrl = new URL(docUrlString)
      }
      catch {
        case e: Exception => throw new InputException("The informed &url is not a valid one.")
      }
    } else {
      docUrl = new URL("http://spotlight.dbpedia.org/id/" + text.hashCode)
    }

    //Store the feedback
    val fb = new StandardFeedback(new Text(text), docUrl, new DBpediaResource(entityUri), new SurfaceForm(surfaceForm), offset,
      standardFeedback, systems, isManualFeedback)
    if (discourseType != "")
      fb.setDiscourseType(discourseType)
    if (entityUriSuggestion != "")
      fb.setEntityUriSuggestion(new DBpediaResource(entityUriSuggestion))
    if (language != "")
      fb.setLanguage(language)

    //Return the validated and standardized feedback
    fb
  }

}