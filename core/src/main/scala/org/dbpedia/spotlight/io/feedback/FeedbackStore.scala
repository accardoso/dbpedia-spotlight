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
}

/**
 * Stores the feedback in a TSV file
 *
 * @constructor (output: Writer) -> Main constructor, receive a Writer where will append all new feedback.
 * @constructor (stream: OutputStream) -> receive a OutputStream and pass it into a Writer.
 * @constructor (file: File) -> receive a File and pass it into a Writer.
 * @constructor (storageFolderPath: String , storageFileName: String) -> create a new File named as informed at storageFileName in the paht informed at storageFolderPath, and pass it into a Writer.
 * @constructor (storageFolderPath: String) -> create a new File with the default name ("feedbackStore") in the path informed at storageFolderPath, and pass it into a Writer.
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
  def add(feedback: SpotlightFeedback) = {
    output.append("\n")
    output.append(feedback.mkString("\t"))
    output.flush()
  }

}

/**
 * Stores the feedback in a CSV file
 *
 * @constructor output: Writer -> Main constructor, receive a Writer where will append all new feedback.
 * @constructor stream: OutputStream -> receive a OutputStream and pass it into a Writer.
 * @constructor file: File -> receive a File and pass it into a Writer.
 * @constructor storageFolderPath: String , storageFileName: String -> create a new File named as informed at storageFileName in the paht informed at storageFolderPath, and pass it into a Writer.
 * @constructor storageFolderPath: String -> create a new File with the default name ("feedbackStore") in the path informed at storageFolderPath, and pass it into a Writer.
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
  def add(feedback: SpotlightFeedback) = {
    output.append("\n")
    output.append(feedback.mkString(","))
    output.flush
  }

}

class LuceneIndexStore(rootDirectory: File, languageCode: String) extends FeedbackStore {
  def this(rootDirectoryPath: String, languageCode: String) = this(new File(rootDirectoryPath), languageCode)
  def this(rootDirectory: File) = this(rootDirectory, "en")
  def this(rootDirectoryPath: String) = this(new File(rootDirectoryPath), "en")

  if(!rootDirectory.exists() || !rootDirectory.isDirectory)
    throw new IllegalArgumentException("Could not find the root directory: %s".format(rootDirectory))

  val storageDirectory = LuceneIndexStore.createStorageFolder(rootDirectory + File.separator + "feedbackStore.en-LuceneIndex")

  var outputFolders: List[File] = List()
  SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
    try{
      outputFolders = outputFolders :+ LuceneIndexStore.createStorageFolder(storageDirectory.getCanonicalPath + File.separator + possibility + "Feedback")
    } catch {
      case e: IllegalAccessError =>
        throw new IllegalAccessError(("Was not possible to store the %s feedback as lucene index. " +
                                      "Because %s file exists and is not a directory.")
                                      .format(possibility,
                                      storageDirectory.getCanonicalPath + File.separator + possibility + "Feedback" ))
    }
  }

  var instanceTypesWriters: List[Writer] = List()
  var labelsWriters: List[Writer] = List()
  var redirectsWriters: List[Writer] = List()
  var disambiguationsWriters: List[Writer] = List()
  var dumpWriters: List[Writer] = List()
  var i: Int = 0
  outputFolders.foreach{ folder =>
    instanceTypesWriters = instanceTypesWriters :+
      new FileWriter(new File(folder.getCanonicalPath + File.separator
                              + SpotlightFeedback.getAllFeedbackPossibilities()(i) + "Feedback_instance_types.nq"), true)
    labelsWriters = labelsWriters :+
      new FileWriter(new File(folder.getCanonicalPath + File.separator
                              + SpotlightFeedback.getAllFeedbackPossibilities()(i) + "Feedback_labels.nq"), true)
    redirectsWriters = redirectsWriters :+
      new FileWriter(new File(folder.getCanonicalPath + File.separator
                              + SpotlightFeedback.getAllFeedbackPossibilities()(i) + "Feedback_redirects.nq"), true)
    disambiguationsWriters = disambiguationsWriters :+
      new FileWriter(new File(folder.getCanonicalPath + File.separator
                              + SpotlightFeedback.getAllFeedbackPossibilities()(i) + "Feedback_disambiguations.nq"), true)
    dumpWriters = dumpWriters :+
      new FileWriter(new File(folder.getCanonicalPath + File.separator
                              + SpotlightFeedback.getAllFeedbackPossibilities()(i) + "Feedback_dump.xml"), true)
    i += 1
  }

  //End-constructor


  /* Add the new feedback to the storing lucene indexes */
  def add(feedback: SpotlightFeedback) = {
    val index = SpotlightFeedback.getAllFeedbackPossibilities().indexOf(feedback.getFeedback())
    instanceTypesWriters(index).append(generateInstanceType(feedback))
    instanceTypesWriters(index).flush()

    labelsWriters(index).append(generateLabel(feedback))
    labelsWriters(index).flush()

    redirectsWriters(index).append(generateRedirect(feedback))
    redirectsWriters(index).flush()

    disambiguationsWriters(index).append(generateDisambiguation(feedback))
    disambiguationsWriters(index).flush()

    dumpWriters(index).append(generateDump(feedback))
    dumpWriters(index).flush()
  }

  private def generateInstanceType(feedback: SpotlightFeedback): String = {
    ""
  }
  private def generateLabel(feedback: SpotlightFeedback): String = {
    List("<"+feedback.getEntityFullUri()+">",
         LuceneIndexStore.labelTripleMiddle,
         "\""+feedback.getSurfaceFormName()+"\"@"+languageCode).mkString(" ")
  }
  private def generateRedirect(feedback: SpotlightFeedback): String = {
    ""
  }
  private def generateDisambiguation(feedback: SpotlightFeedback): String = {
    ""
  }
  private def generateDump(feedback: SpotlightFeedback): String = {
    ""
  }

}

object LuceneIndexStore {

  private val instanceTypeTripleMiddle: String = "<www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  private val labelTripleMiddle: String = "<http://www.w3.org/2000/01/rdf-schema#label>"
  private val redirectTripleMiddle: String = "<dbpedia.org/ontology/wikiPageRedirects>"

  /* Create the folder where the files with the feedback will be placed */
  def createStorageFolder (storageFolderName: String) : File = {
    val warehouse = new File(storageFolderName)
    val created:Boolean = warehouse.mkdir()
    if (!created){
      if (warehouse.exists() && !warehouse.isDirectory)
        throw new IllegalAccessError("File exists but is not a directory.")
    }
    warehouse
  }
}