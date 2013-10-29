package org.dbpedia.spotlight.model

import java.net.URL
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResource, Text}

/**
 * An abstract data type for the Feedback (after standardization)
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

class StandardFeedback(text: Text, docUrl: URL, entityUri: DBpediaResource, surfaceForm: SurfaceForm, offset: Int,
                       feedback: String, systems: Array[String], isManualFeedback: Boolean) {
  //Declare the optional fields
  var discourseType: String = ""
  var entityUriSuggestion: DBpediaResource = null
  var language: String = ""

  //Setters for the optional fields
  def setDiscourseType(discourseType: String) = this.discourseType = discourseType
  def setEntityUriSuggestion(entityUriSuggestion: DBpediaResource) = this.entityUriSuggestion = entityUriSuggestion
  def setLanguage(language: String) = this.language = language

  //Getters
  def getText(): Text = text
  def getDocUrl(): URL = docUrl
  def getDiscourseType(): String = discourseType
  def getEntityUri(): DBpediaResource = entityUri
  def getEntityUriSuggestion(): DBpediaResource = entityUriSuggestion
  def getSurfaceForm(): SurfaceForm = surfaceForm
  def getOffset(): Int = offset
  def getFeedback(): String = feedback
  def getSystems(): Array[String] = systems
  def getIsManualFeedback(): Boolean = isManualFeedback
  def getLanguage(): String = language

}
