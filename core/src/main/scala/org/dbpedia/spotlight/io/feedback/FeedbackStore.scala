package org.dbpedia.spotlight.io.feedback

import java.io.{FileWriter, PrintWriter, OutputStream, Writer, File}


/**
 * Stores standard Feedback
 *
 * @author pablomendes
 * @author Alexandre Cançado Cardoso - accardoso
 */

trait FeedbackStore {
  /* Add (append) the new feedback at the end of the storing file/database using the specific format */
  def add(feedback: StandardFeedback)
}

/**
 * Stores the feedback in a TSV file
 */
class TSVFeedbackStore(output: Writer) extends FeedbackStore {
  /* Constructor with a OutputStream as output and converting it to a Writer */
  def this(stream: OutputStream) = this(new PrintWriter(stream))
  /* Constructor with a informed File as output and converting it to a appendable FileWriter */
  def this(file: File) = this(new FileWriter(file, true))
  /* Constructor with new File as output and convert it to a appendable FileWriter */
  def this(storageFolderPath: String, storageFileName: String) =
    this(new FileWriter(new File(storageFolderPath + File.separator + storageFileName + ".tsv"), true))
  /* Constructor with default storage file name */
  def this(storageFolderPath: String) = this(storageFolderPath, "feedbackStore")


  /* Add (append) the new feedback at the end of the storing file/database using the tsv format */
  def add(feedback: StandardFeedback) = {

    var entry:String = feedback.getText.toString +"\t"+ feedback.getDocUrl.toString +"\t"+ feedback.getDiscourseType +"\t"+
      feedback.getEntityUri.toString +"\t"
    if(feedback.getEntityUriSuggestion() != null)
      entry += feedback.getEntityUriSuggestion.toString
    entry += "\t"+ feedback.getSurfaceForm.toString +"\t"+ feedback.getOffset.toString +"\t"+ feedback +"\t"+
      feedback.getSystems.toString +"\t"+ feedback.getIsManualFeedback.toString +"\t"+ feedback.getLanguage +"\n"

    output.append(entry)
    output.flush()
  }

}

/**
 * Stores the feedback in a CSV file
 */
class CSVFeedbackStore(output: Writer) extends FeedbackStore {
  /* Constructor with a OutputStream as output and converting it to a Writer */
  def this(stream: OutputStream) = this(new PrintWriter(stream))
  /* Constructor with a informed File as output and converting it to a appendable FileWriter */
  def this(file: File) = this(new FileWriter(file, true))
  /* Constructor with new File as output and convert it to a appendable FileWriter */
  def this(storageFolderPath: String, storageFileName: String) =
    this(new FileWriter(new File(storageFolderPath + File.separator + storageFileName + ".csv"), true))
  /* Constructor with default storage file name */
  def this(storageFolderPath: String) = this(storageFolderPath, "feedbackStore")


  /* Add (append) the new feedback at the end of the storing file/database using the csv format */
  def add(feedback: StandardFeedback) = {

    var entry:String = feedback.getText.toString +"\t"+ feedback.getDocUrl.toString +"\t"+ feedback.getDiscourseType +"\t"+
      feedback.getEntityUri.toString +"\t"
    if(feedback.getEntityUriSuggestion() != null)
      entry += feedback.getEntityUriSuggestion.toString
    entry += "\t"+ feedback.getSurfaceForm.toString +"\t"+ feedback.getOffset.toString +"\t"+ feedback +"\t"+
             feedback.getSystems.toString +"\t"+ feedback.getIsManualFeedback.toString +"\t"+ feedback.getLanguage +"\n"

    output.append(entry)
    output.flush()
  }

}