package org.dbpedia.spotlight.model

import java.net.URL
import org.dbpedia.spotlight.exceptions.InputException

/**
 * An abstract data type for the Feedback (after standardization)
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

class SpotlightFeedback(text: Text, var docUrl: URL, var discourseType: String, entity: DBpediaResource,
                        surfaceForm: SurfaceForm, offset: Int, var feedback: String, var systems: List[String],
                        isManualFeedback: Boolean, var language: String) {
  /* Constructor for obligatory attributes only */
  def this(text: Text, entity: DBpediaResource, surfaceForm: SurfaceForm, offset: Int, feedback: String,
           systems: List[String], isManualFeedback: Boolean) = this(text, SpotlightFeedback.createDefaultDocUrl(text),
           "", entity, surfaceForm, offset, feedback, systems, isManualFeedback, "")
  /* Constructor for obligatory attributes only as received by plain text HTTP interface*/
  def this(text: String, entityUri: String, surfaceFormString: String, offset: Int, feedback: String,
           systemsString: String, isManualFeedback: Boolean) = this(new Text(text), new DBpediaResource(entityUri),
           new SurfaceForm(surfaceFormString), offset, feedback, systemsString.split(" ").toList, isManualFeedback)
  /* Constructor for all attributes as received by plain text HTTP interface*/
  def this(text: String, docUrlString: String, discourseType: String, entityUri: String, surfaceFormString: String,
           offset: Int, feedback: String, systemsString: String, isManualFeedback: Boolean, language:String)
    = this(new Text(text), SpotlightFeedback.createDocUrl(docUrlString, text), discourseType,
           new DBpediaResource(entityUri), new SurfaceForm(surfaceFormString), offset, feedback,
           systemsString.split(" ").toList, isManualFeedback, language)

  /* Standardize obligatory attributes */
  //Feedback must be lower case
  feedback = feedback.toLowerCase
  //All system at systems must be lower case
  systems = systems.map(_.toLowerCase())

  /* Standardize optional attributes */
  //Discourse type must be lower case or empty
  discourseType = discourseType.toLowerCase
  //Language must be lower case or empty
  language = language.toLowerCase

  /* Validate obligatory attributes */
  SpotlightFeedback.validate(text, entity, surfaceForm, offset, feedback, systems, isManualFeedback)

  //End-constructor

  /* Methods */

  /* Setters for the optional attributes receiving the HTTP interface format
  *  Allow to construct the SpotlightFeedback with the obligatory attributes and add only the useful optional ones.
  */
  def setDocUrl(docUrlString: String) = this.docUrl = SpotlightFeedback.createDocUrl(docUrlString, text)
  def setDiscourseType(discourseType: String) = this.discourseType = discourseType.toLowerCase
  def setLanguage(language: String) = this.language = language.toLowerCase

  /* Getters */
  def getText(): Text = text
  def getDocUrl(): URL = docUrl
  def getDiscourseType(): String = discourseType
  def getEntity(): DBpediaResource = entity
  def getEntityFullUri() = entity.getFullUri
  def getSurfaceForm(): SurfaceForm = surfaceForm
  def getSurfaceFormName(): SurfaceForm = surfaceForm
  def getOffset(): Int = offset
  def getFeedback(): String = feedback
  def getSystems(): List[String] = systems
  def getIsManualFeedback(): Boolean = isManualFeedback
  def getLanguage(): String = language

  def mkString(sep: String): String = {
    if(sep == " ")
      throw new IllegalArgumentException("SpotlightFeedback attributes can not be separated by \" \", because it is already used to separate the systems list elements")
    var list: List[String] = List(text.text.replace(sep, ""), docUrl.toString) //Need to remove sep from the text to do not generate confusion with where is a text element and where is a separator //TODO a real escape for the sep that are text elements (suggestion: encode to url text)
    if(discourseType == "")
      list = list :+ SpotlightFeedback.emptyFieldRepresentation
    else
      list = list :+ discourseType
    list = list ++ List(entity.getFullUri, surfaceForm.getName, offset.toString, feedback, systems.mkString(" "), isManualFeedback.toString)
    if(language == "")
      list = list :+ SpotlightFeedback.emptyFieldRepresentation
    else
      list = list :+ language

    list.mkString(sep)
  }

  override def toString(): String = {
    "SpotlightFeedback[" + this.mkString(" | ") + "]"
  }

  def toDBpediaResourceOccurrence():DBpediaResourceOccurrence = {
    new DBpediaResourceOccurrence(entity, surfaceForm, text, offset)
  }
}

object SpotlightFeedback {

  val emptyFieldRepresentation: String = "_"
  val nunOfFeedbackFields: Int = 11

  private val allFeedbackPossibilities: List[String] = List("correct", "incorrect")

  private val automaticSystemsIds: List[String] = List("spotlight_lucene", "spotlight_statistical", "alchemy_api", "zemanta", "open_calais")

  private val defaultDocURLRoot: String = "http://spotlight.dbpedia.org/id/" //If no doc_url is informed, Spotlight produce a default one with the root below and the text hash

  /* Create the default docUrl using the informed text */
  private def createDefaultDocUrl(text: Text): URL = {
    if(text.text != "")
      new URL(SpotlightFeedback.defaultDocURLRoot + text.hashCode)
    else
      null //Text must be not empty, and it is verified at validate()
  }

  /* Create a valid docUrl of the correct type (URL) from the informed string document url (used at the HTTP request) or, if not possible, the default docUrl from the text */
  private def createDocUrl(docUrlString: String, text: Text): URL = {
    if (!(docUrlString == "")) {
      try {
        return new URL(docUrlString)
      }
      catch {
        case e: Exception => throw new InputException("url is not valid.")
      }
    } else {
      return createDefaultDocUrl(text)
    }
  }
  /* Create a valid docUrl of the correct type (URL) from the informed string document url (used at the HTTP request) or, if not possible, the default docUrl from the string text */
  private def createDocUrl(docUrlString: String, text: String): URL = createDocUrl(docUrlString, new Text(text))

  /* Validate the obligatory attributes of SpotlightFeedback */
  private def validate(text: Text, entityUri: DBpediaResource, surfaceForm: SurfaceForm, offset: Int,
                       feedback: String, systems: List[String], isManualFeedback: Boolean): Boolean = {

    if (text == "")
      throw new InputException("text must be filled!")
    if (entityUri == "")
      throw new InputException("entity_uri must be filled!")
    if (systems.length == 0)
      throw new InputException("systems must be filled!")
    if (surfaceForm == "")
      throw new InputException("surface_form must be filled!")
    if (offset<=0)
      throw new InputException("offset must be filled with a entity position (a positive value)!")
    if(!allFeedbackPossibilities.contains(feedback))
      throw new IllegalArgumentException("\"%s\" is not a valid feedback. List of valid ones: %s".format(feedback, allFeedbackPossibilities.toString()))
    if(!isManualFeedback){
      if(!systems.forall(automaticSystemsIds.contains(_)))
        throw new IllegalArgumentException("One or more systems are not registered. List of valid ones: %s . If it is a new system, please contact us for registration.".format(automaticSystemsIds))
    }

    //It is valid
    true
  }
  
  def getAllFeedbackPossibilities(): List[String] = {
    allFeedbackPossibilities
  }

}
