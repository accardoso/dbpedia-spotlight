package org.dbpedia.spotlight.io.feedback

import org.dbpedia.spotlight.model.SpotlightFeedback
import org.dbpedia.spotlight.lucene.index.MergedOccurrencesContextIndexer
import java.io.File
import org.dbpedia.spotlight.lucene.LuceneManager
import org.apache.lucene.store.{AlreadyClosedException, FSDirectory}

/**
 * Stores the feedback in its respective Lucene index. Each feedback possibility must have one respective index.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 *
 * @constructor (output: List[(String,MergedOccurrencesContextIndexer)]) -> A list o Tuple2 where the first element is the feedback possibility and the second is the respective Lucene index.
 * @constructor (indexDirectoryByFeedbackPossibility: Array[(String, File)]) -> A list o Tuple2 where the first element is the feedback possibility and the second is the respective Lucene index folder (a OccurrenceContextIndexer is auto-created to open and write in this index).  (If the directories do not exist they will be created with a empty index, default named)
 * @constructor (storageRootFolderPath: String) -> The directory path where has one index folder for each possibility. This folders must be named as: "index-< possibility_name >" (If the directories do not exist they will be created with a empty index)
 */

class LuceneFeedbackStore(output: List[(String,MergedOccurrencesContextIndexer)]) extends FeedbackStore {
  def this(indexDirectoryByFeedbackPossibility: Array[(String, File)]) = this(LuceneFeedbackStore.toStandardParam(indexDirectoryByFeedbackPossibility))
  def this(storageRootFolderPath: String) = this(LuceneFeedbackStore.createDefaultFiles(storageRootFolderPath).toArray)

  //Validate the output: verify if there is at least 1 and just 1 output indexer for each feedback possibility
  if(output.length != SpotlightFeedback.getAllFeedbackPossibilities().length)
    throw new ArrayIndexOutOfBoundsException("Must be informed 1 output index for each of the %d feedback possibilities.".format(SpotlightFeedback.getAllFeedbackPossibilities().length))
  output.foreach{ element =>
    if(!SpotlightFeedback.getAllFeedbackPossibilities().contains(element._1))
      throw new NoSuchFieldException(("%s is not a feedback possibility: %s").format(element._1))
  }
  //End-Constructor

  /* Store (add/append) the informed feedback to the index of the respective possibility and close the index after storage */
  def add(feedback: SpotlightFeedback) {
    val id = output.indexWhere(_._1 == feedback.getFeedback())
    output(id)._2.add(feedback.toDBpediaResourceOccurrence())
  }

  /* Close all MergedOccurrencesContextIndexer of the output */
  def finalizeStorage() {
    output.foreach{ element =>
      LuceneFeedbackStore.closeIndexer(element._2)
    }
  }

}

object LuceneFeedbackStore {

  /* Create the default indexes directories inside the informed directory */
  def createDefaultFiles(outputRootDirectoryPath: String): List[(String, File)] = {
    val outputDir = new File(outputRootDirectoryPath)
    if(outputDir.exists() && !outputDir.isDirectory)
      throw new IllegalArgumentException("%s already exist and it is not a directory.".format(outputRootDirectoryPath))

    var indexes: List[(String, File)] = List()

    SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
      indexes = indexes :+ (possibility, new File(outputRootDirectoryPath+File.separator+"index-"+possibility))
    }

    indexes
  }

  /* Convert the output index reference of each possibility Directory to a OccurrenceContextIndexer */
  def toStandardParam(list: Array[(String, File)]): List[(String, MergedOccurrencesContextIndexer)] = {
    var output: List[(String, MergedOccurrencesContextIndexer)] = List()
    list.foreach{ element =>
      output = output :+ (element._1, toOccurrenceContextIndexer(element._2))
    }
    output.toList
  }

  /* Convert the informed index directory to a OccurrencesContextIndexer */
  def toOccurrenceContextIndexer(indexDirectory: File) : MergedOccurrencesContextIndexer = {
    /* Based on /org/dbpedia/spotlight/lucene/index/IndexMergedOccurrences.scala (by maxjakob) */
    val lucene = new LuceneManager.BufferedMerging(FSDirectory.open(indexDirectory), 200000, false)

    if (!indexDirectory.exists()) {
      lucene.shouldOverwrite = true
      indexDirectory.mkdir()
    } else {
      if(!indexDirectory.isDirectory)
        throw new IllegalArgumentException("%s already exist and it is not a directory.".format(indexDirectory.getCanonicalPath))

      lucene.shouldOverwrite = false
    }

    new MergedOccurrencesContextIndexer(lucene)
  }

  /* Close the informed indexer (if it is already closed then disregard) */
  def closeIndexer(indexer: MergedOccurrencesContextIndexer) {
    try{
      indexer.close()
    }catch {
      case e: AlreadyClosedException =>
    }
  }

}