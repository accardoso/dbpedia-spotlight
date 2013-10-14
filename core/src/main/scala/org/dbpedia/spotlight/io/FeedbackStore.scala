package org.dbpedia.spotlight.io

import java.net.URL
import org.dbpedia.spotlight.model.{Text, SurfaceForm, DBpediaResource}
import java.io.PrintStream
import java.lang.Boolean


/**
 * Stores user-provided feedback on annotations (correct, incorrect, etc.)
 *
 */
trait FeedbackStore {
    /*
     */

  val feedbackPossibilities: List[String] = List("correct", "incorrect")

  def add(docUrl: URL, text: Text, resource: DBpediaResource, surfaceForm: SurfaceForm, offset: Int, feedback: String, systems: Array[String])
}

class CSVFeedbackStore(val output: PrintStream) extends FeedbackStore {



  def add(docUrl: URL, text: Text, resource: DBpediaResource, surfaceForm: SurfaceForm, offset: Int, feedback: String, systems: Array[String]) = {
    val feedbackStandardized = feedback.toLowerCase

    if(!feedbackPossibilities.contains(feedbackStandardized))
      throw new IllegalArgumentException("\"%s\" is not a valid feedback. List of valid ones: %s".format(feedback, feedbackPossibilities.toString()))

    output.println(List("Document URL","Standardized Feedback","Entity URI","Surface Form","Text","Offset",systems.mkString(" ")).mkString("\t"))
    output.println(List(docUrl,feedback.toLowerCase,resource.uri,surfaceForm.name,text.text,offset,systems.mkString(" ")).mkString("\t"))

  }

}

