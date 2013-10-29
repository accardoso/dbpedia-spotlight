package org.dbpedia.spotlight.io.feedback

import java.io.File

/**
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

class FeedbackMultiStore(var stores: List[FeedbackStore]) {
  def this() = this(List[FeedbackStore]())


  def addStore(store: FeedbackStore) = {
    stores = stores :+ store
  }

  def addFeedback(feedback: StandardFeedback) = {
    for (store <- stores){
      store.add(feedback)
    }
  }

  override def toString(): String = {
    var str = "FeedbackMultiStore's current stores:"
    for(store <-stores){
      str += "\n\t" + store.toString()
    }
    str
  }

}


object FeedbackMultiStore {
  /* Create the folder where the files with the feedback shall be stored */
  def createStorageFolder (storageFolderName: String) : String = {
    val warehouse = new File(storageFolderName)
    val created:Boolean = warehouse.mkdir()
    if (!created){
      if (warehouse.exists() && !warehouse.isDirectory)
        throw new IllegalAccessError("File exists and is not a directory.")
    }
    warehouse.getCanonicalPath
  }
}