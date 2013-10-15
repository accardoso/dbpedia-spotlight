package org.dbpedia.spotlight.web.rest.resources

import javax.ws.rs.WebApplicationException
import javax.ws.rs.core.Response
import org.dbpedia.spotlight.exceptions.InputException
import java.net.URL
import org.dbpedia.spotlight.io.{CSVFeedbackStore, FeedbackStore}
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResource, Text}

/**
 * This class format, standardize, clean-up and validate the received feedback before store it to be used.
 *
 * @author pablomendes (original java implementation at Feedback.java)
 * @author Alexandre Cançado Cardoso - accardoso
 */

object FeedbackScala {

  val feedbackPossibilities: List[String] = List("correct", "incorrect")

  val automaticSystemsIds: List[String] = List("alchemy_api", "open_calais", "spotlight_lucene")


  def process(clientIp:String, key: String, text: String, docUrlString: String, discourseType: String, entityUri: String,
              entityUriSuggestion: String, surfaceForm: String, offset: Int, feedback: String, systemIds: String,
              isManualFeedback: Boolean, language: String) : String = {

    // Authentication
    /*if(registeredClients.contains(clientIp))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED) */
    if (!(key == "2013_Helping_Spotlight_improvement_with_feedback_for_that_I_have_the_key"))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)
    //TODO definitive professional authentication

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
    val output: FeedbackStore = new CSVFeedbackStore(System.out)
    output.add(docUrl, new Text(text), new DBpediaResource(entityUri), new SurfaceForm(surfaceForm), offset, standardFeedback, systems)

    //Answer to the HTTP request
    "ok"
  }

}
