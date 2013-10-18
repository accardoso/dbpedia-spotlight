package org.dbpedia.spotlight.io

import java.net.URL
import org.dbpedia.spotlight.model.{Text, SurfaceForm, DBpediaResource}
import java.io.{File, FileWriter}

/**
 * Stores processed feedback on annotations (correct, incorrect, etc.)
 *
 * @author pablomendes
 * @author Alexandre Cançado Cardoso - accardoso
 */

trait FeedbackStore {

  /* Define the folder where the files/databases with the feedback shall be stored */
  val storageFolderName : String = "FeedbackWarehouse"

  /* Create the folder where the files with the feedback shall be stored */
  def createStorageFolder () : File = {
    val storage = new File(storageFolderName)
    val created:Boolean = storage.mkdir()
    if (!created){
      if (storage.exists() && !storage.isDirectory)
          throw new IllegalAccessError("File exists and is not a directory.")
    }

    storage.getAbsoluteFile
  }

  /* Abstract methods */

  /* Create the file/database where the files with the feedback shall be stored */
  def createStorageFile () : File

  /* Add (append) the new feedback at the end of the storing file/database using the specific format */
  def add(text: Text, docUrl: URL, discourseType: String, entityUri: DBpediaResource, entityUriSuggestion: DBpediaResource,
          surfaceForm: SurfaceForm, offset: Int, feedback: String, systems: Array[String], isManualFeedback: Boolean, language: String)

}

/**
 * Stores the feedback in a TSV file
 */
object TSVFeedbackStore extends FeedbackStore {

  var storage : File = createStorageFile()

  def createStorageFile() : File = new File (createStorageFolder().getAbsolutePath + File.separator + "feedbackStorage.tsv")

  def add(text: Text, docUrl: URL, discourseType: String, entityUri: DBpediaResource, entityUriSuggestion: DBpediaResource,
          surfaceForm: SurfaceForm, offset: Int, feedback: String, systems: Array[String], isManualFeedback: Boolean, language: String) = {

    //Create the .tsv file to store feedback if it do not exists
    if (!storage.exists())
      storage = createStorageFile()

    val entry = text.toString +"\t"+ docUrl.toString +"\t"+ discourseType +"\t"+ entityUri.toString +"\t"+ entityUriSuggestion.toString +"\t"+ surfaceForm.toString +"\t"+ offset.toString +"\t"+ feedback +"\t"+ systems.toString +"\t"+ isManualFeedback.toString +"\t"+ language +"\n"

    //Append feedback to the end of storage file
    val fw = new FileWriter(storage, true)
    fw.write(entry)
    fw.close()

  }

}

