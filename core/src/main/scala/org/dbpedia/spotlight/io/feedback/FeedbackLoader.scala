package org.dbpedia.spotlight.io.feedback

import java.io.File
import org.dbpedia.spotlight.model.SpotlightFeedback
import scala.io.Source
import java.nio.file.NoSuchFileException
import java.nio.file._
import org.dbpedia.spotlight.log.SpotlightLog

/**
 * Load Spotlight Feedback
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

trait FeedbackLoader{
  /* Load all feedback stored at the source file/database to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback]
}

/**
 * Load the feedback from a TSV file
 *
 */
class TSVFeedbackLoader(src: File) extends FeedbackLoader {

  /* Load all feedback stored at source file to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback] = {
    if(!src.exists())
      throw new NoSuchFileException("Could not find the file: %s".format(src.getCanonicalPath))

    var list:  List[SpotlightFeedback] =  List[SpotlightFeedback]()
    Source.fromFile(src).getLines.foreach{
      line =>
        if(line != "")
          list = list :+ parserLine(line)
    }

    list
  }
  
  private def parserLine(line: String): SpotlightFeedback = {
    val values: Array[String] = line.split("\t")

    if(values.length != SpotlightFeedback.nunOfFeedbackFields)
      throw new UnsupportedOperationException(("Impossible to load feedback from file: %s\n" +
                                               "All feedback must have %d fields").format(src.getCanonicalPath, SpotlightFeedback.nunOfFeedbackFields))

    if(values(2) == SpotlightFeedback.emptyFieldRepresentation)
      values(2) = ""
    if(values(4) == SpotlightFeedback.emptyFieldRepresentation)
      values(4) = ""
    if(values(10) == SpotlightFeedback.emptyFieldRepresentation)
      values(10) = ""

    new SpotlightFeedback(values(0), values(1), values(2), values(3), values(4), values(5),
    values(6).toInt, values(7), values(8), values(9).toBoolean, values(10))
  }
  
}