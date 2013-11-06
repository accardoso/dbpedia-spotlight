package org.dbpedia.spotlight.io.feedback

import java.io.File
import org.dbpedia.spotlight.model.SpotlightFeedback
import scala.io.Source
import java.nio.file._

/**
 * Load Spotlight Feedback
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

trait FeedbackLoader{
  /* Load all feedback stored at the source file/database to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback]
}

abstract class FeedbackFileLoader(src: File) extends FeedbackLoader{
  /* Load all feedback stored at the source file (in a specific format) to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback]

  /* Load all feedback stored at the source file to a list of SpotlightFeedback using informed field SEParator */
  def load(sep: String): List[SpotlightFeedback] = {
    if(!src.exists())
      throw new NoSuchFileException("Could not find the file: %s".format(src.getCanonicalPath))

    var list:  List[SpotlightFeedback] =  List[SpotlightFeedback]()
    Source.fromFile(src).getLines.foreach{
      line =>
        if(line != "")
          list = list :+ parserLine(line, sep)
    }

    list
  }

  /* Parse the informed line using informed field SEParator into a SpotlightFeedback*/
  private def parserLine(line: String, sep: String): SpotlightFeedback = {
    val values: Array[String] = line.split(sep)

    if(values.length != SpotlightFeedback.nunOfFeedbackFields)
      throw new UnsupportedOperationException(("Impossible to load feedback from file: %s\n" +
        "All feedback must have %d fields").format(src.getCanonicalPath, SpotlightFeedback.nunOfFeedbackFields))

    if(values(2) == SpotlightFeedback.emptyFieldRepresentation)
      values(2) = ""
    if(values(9) == SpotlightFeedback.emptyFieldRepresentation)
      values(9) = ""

    new SpotlightFeedback(values(0), values(1), values(2), values(3), values(4), values(5)toInt,
      values(6), values(7), values(8).toBoolean, values(9))
  }

}

/**
 * Load the feedback from a TSV file
 */
class TSVFeedbackLoader(src: File) extends FeedbackFileLoader(src) {
  /* Load all feedback stored at the source file/database to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback] = {
    load("\t")
  }
}

/**
 * Load the feedback from a CSV file
 */
class CSVFeedbackLoader(src: File) extends FeedbackFileLoader(src) {
  /* Load all feedback stored at the source file to a list of SpotlightFeedback */
  def load(): List[SpotlightFeedback] = {
    load(",")
  }
}