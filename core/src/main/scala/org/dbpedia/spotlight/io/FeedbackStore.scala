package org.dbpedia.spotlight.io

import java.net.URL
import org.dbpedia.spotlight.model.{Text, SurfaceForm, DBpediaResource}
import java.io.{File, FileWriter}


/**
 * Stores user-provided feedback on annotations (correct, incorrect, etc.)
 *
 */
abstract class FeedbackStore {

  val storageFolderName : String = "FeedbackWarehouse"
  def createStorageFolder () : File = {
    val storage = new File(storageFolderName)
    val created:Boolean = storage.mkdir()
    if (!created){
      if (storage.exists() && !storage.isDirectory)
          throw new IllegalAccessError("File exists and is not a directory.")
    }

    storage.getAbsoluteFile
  }

  def createStorageFile () : File

  def add(text: Text, docUrl: URL, discourseType: String, entityUri: DBpediaResource, entityUriSuggestion: DBpediaResource,
          surfaceForm: SurfaceForm, offset: Int, feedback: String, systems: Array[String], isManualFeedback: Boolean, language: String)

}

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

  //def add() {}

}

