package org.dbpedia.spotlight.io.feedback

import java.io.{FileWriter, PrintWriter, OutputStream, Writer, File}
import org.dbpedia.spotlight.model.SpotlightFeedback

/**
 * Store Spotlight Feedback
 *
 * @author pablomendes
 * @author Alexandre Cançado Cardoso - accardoso
 */

trait FeedbackStore {
  /* Add (append) the new feedback at the end of the storing file/database using the specific format */
  def add(feedback: SpotlightFeedback)

  /* Add (append) a batch of new feedback at the end of the storing file/database using the specific format */
  def addAll(src: List[SpotlightFeedback]) = src.foreach(add(_))

  /* Convert all feedback loaded at the FeedbackLoader and add them at the end of the storing file/database using the specific format */
  def convertFrom(src: FeedbackLoader) = addAll(src.load())

  /* Finalize (close) the storage file/database (only if the storage format need). */
  def finalizeStorage()
}

/**
 * Stores the feedback in a Character-Separated Values file
 */
abstract class CharacterSVFeedbackStore(output: Writer, separator: Char)  extends FeedbackStore {

  /* Add (append) the new feedback at the end of the storing file/database using the separator */
  def add(feedback: SpotlightFeedback) = {
    output.append("\n")
    output.append(feedback.mkString(separator.toString))
    output.flush()
  }

  /* Close the CharacterSVFeedbackStore is not needed. Because any usage problem happens if do not close the output (Writer). */
  def finalizeStorage() {}
}

/**
* Stores the feedback in a Tab-Separated Values (.tsv) file
*
* @constructor (output: Writer) -> Main constructor, receive a Writer where will append all new feedback.
* @constructor (stream: OutputStream) -> receive a OutputStream and pass it into a Writer.
* @constructor (file: File) -> receive a File and pass it into a Writer.
* @constructor (storageFolderPath: String , storageFileName: String) -> create a new File named as informed at storageFileName in the paht informed at storageFolderPath, and pass it into a Writer.
* @constructor (storageFolderPath: String) -> create a new File with the default name ("feedbackStore") in the path informed at storageFolderPath, and pass it into a Writer.
*/
class TSVFeedbackStore(output: Writer) extends CharacterSVFeedbackStore(output, '\t') {
  /* Constructor with a OutputStream as output and converting it to a Writer */
  def this(stream: OutputStream) = this(new PrintWriter(stream))
  /* Constructor with a informed File as output and converting it to a appendable FileWriter */
  def this(file: File) = this(new FileWriter(file, true))
  /* Constructor with new File as output and convert it to a appendable FileWriter */
  def this(storageFolderPath: String, storageFileName: String) =
    this(new FileWriter(new File(storageFolderPath + File.separator + storageFileName + ".tsv"), true))
  /* Constructor with default storage file name */
  def this(storageFolderPath: String) = this(storageFolderPath, "feedbackStore")
}

/**
* Stores the feedback in a Comma-Separated Values (.csv) file
*
* @constructor output: Writer -> Main constructor, receive a Writer where will append all new feedback.
* @constructor stream: OutputStream -> receive a OutputStream and pass it into a Writer.
* @constructor file: File -> receive a File and pass it into a Writer.
* @constructor storageFolderPath: String , storageFileName: String -> create a new File named as informed at storageFileName in the paht informed at storageFolderPath, and pass it into a Writer.
* @constructor storageFolderPath: String -> create a new File with the default name ("feedbackStore") in the path informed at storageFolderPath, and pass it into a Writer.
*/
class CommaSVFeedbackStore(output: Writer) extends CharacterSVFeedbackStore(output, ',') {
  /* Constructor with a OutputStream as output and converting it to a Writer */
  def this(stream: OutputStream) = this(new PrintWriter(stream))
  /* Constructor with a informed File as output and converting it to a appendable FileWriter */
  def this(file: File) = this(new FileWriter(file, true))
  /* Constructor with new File as output and convert it to a appendable FileWriter */
  def this(storageFolderPath: String, storageFileName: String) =
    this(new FileWriter(new File(storageFolderPath + File.separator + storageFileName + ".csv"), true))
  /* Constructor with default storage file name */
  def this(storageFolderPath: String) = this(storageFolderPath, "feedbackStore")
}

/*
class LuceneFeedbackStore(output: List[FeedbackOccurrences]) extends FeedbackStore {
  *IMPORTANT*: It is implemented at index/org.dbpedia.spotlight.io.LuceneFeedbackStore.scala
}
*/