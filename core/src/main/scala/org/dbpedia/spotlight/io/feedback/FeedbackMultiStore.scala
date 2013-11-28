package org.dbpedia.spotlight.io.feedback

import java.io.File
import org.dbpedia.spotlight.model.SpotlightFeedback

/**
 * This class manage many feedback stores (implementations of FeedbackStore trait).
 * Allowing to add many stores (storage files, stream, or databases) and add a feedback to all of then with only one addFeedback call.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 *
 * @constructor (stores: List[FeedbackStore]) -> Create a Multiple Store Manager and register a list o Stores
 * @constructor () -> Create a Multiple Store Manager without any registered Store
 */

class FeedbackMultiStore(private var stores: List[FeedbackStore]) {
  def this() = this(List[FeedbackStore]())

  /* Add (Register) a new store (a FeedbackStore implementation - a storage file, stream, or database) to the multi store instance. */
  def registerStore(store: FeedbackStore) = stores = stores :+ store

  private def validateStoreRequest(): Boolean = {
    if(stores.isEmpty)
      throw new NullPointerException("Multi Store Manager has no registered store. Please, register at least one before store a feedback.")

    return true
  }

  /* Add the SpotlightFeedback to all registered stores */
  def storeFeedback(feedback: SpotlightFeedback) = {
    validateStoreRequest()
    stores.foreach(_.add(feedback))
  }

  /* Add each SpotlightFeedback at the list to all registered stores */
  def storeFeedbackBatch(feedbackList: List[SpotlightFeedback]) = {
    validateStoreRequest()
    stores.foreach(_.addAll(feedbackList))
  }

  /* Close the the files/databases tha request to be closed to allow future writings and readings */
  def close() = stores.foreach(_.finalizeStorage())

  override def toString(): String = "FeedbackMultiStore[" + stores.mkString(" , ") + "]"

}


object FeedbackMultiStore {
  /* Create the folder where the files with the feedback will be placed */
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