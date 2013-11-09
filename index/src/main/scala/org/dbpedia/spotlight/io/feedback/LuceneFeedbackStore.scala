package org.dbpedia.spotlight.io.feedback

import org.dbpedia.spotlight.model.{Factory, SpotlightFeedback}
import org.dbpedia.spotlight.lucene.index.{MergedOccurrencesContextIndexer, OccurrenceContextIndexer}
import java.io.File
import org.dbpedia.spotlight.lucene.LuceneManager
import org.apache.lucene.store.FSDirectory
import org.dbpedia.spotlight.util.IndexingConfiguration


/**
 * Stores the feedback in its respective Lucene index. Each feedback possibility has a respective index.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

class LuceneFeedbackStore(output: List[(String,OccurrenceContextIndexer)]) extends FeedbackStore {
  def this(baseLuceneIndex: File) = this(LuceneFeedbackStore.createDefaultFiles(baseLuceneIndex))
  def this(output: Array[(String, File)]) = this(LuceneFeedbackStore.toStandardParam(output.toList))

  if(output.length != SpotlightFeedback.getAllFeedbackPossibilities().length)
    throw new ArrayIndexOutOfBoundsException("Must be informed 1 output index for each of the %d feedback possibilities.".format(SpotlightFeedback.getAllFeedbackPossibilities().length))

  output.foreach{ index =>
    if(!SpotlightFeedback.getAllFeedbackPossibilities().contains(index._1))
      throw new NoSuchFieldException(("%s is not a feedback possibility: %s").format(index._1))
  }

  //End-Constructor

  private def addWithoutClose(feedback: SpotlightFeedback): Int = {
    val i = output.indexWhere(_._1 == feedback.getFeedback())
    output(i)._2.add(feedback.toDBpediaResourceOccurrence())
    i
  }

  def add(feedback: SpotlightFeedback) {
    val openIndexer = addWithoutClose(feedback)
    output(openIndexer)._2.close()
  }

  override def addBatch(src: List[SpotlightFeedback]) {
    var openIndexersID: List[Int] = List()
    var openIndexer: Int = -1
    src.foreach{ feedback =>
      openIndexer = addWithoutClose(feedback)
      openIndexersID = openIndexersID :+ openIndexer
    }
    openIndexersID.distinct.foreach(output(_)._2.close())
  }
}

object LuceneFeedbackStore {

  val indexingConfigFileName: String = "/home/alexandre/Projects/empty-index/server.properties"

  def createDefaultFiles(baseLuceneIndex: File): List[(String, OccurrenceContextIndexer)] = {
    throw new Exception("Method implementation not finished!")

    if(!baseLuceneIndex.exists())
      throw new NoSuchElementException("Could not find the lucene index directory at %s".format(baseLuceneIndex.getCanonicalPath))
    if(!baseLuceneIndex.isDirectory())
      throw new IllegalArgumentException("The informed base lucene index must be a directory containing a lucene index.")

    var defaultParam: List[(String, File)] = List()
    var indexPossibilityDir: File = null
    SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
      //TODO -> indexPossibilityDir = //copy the base Index directory and rename the copy as "index-<feedback-possibility>"
      defaultParam = defaultParam :+ (possibility, indexPossibilityDir)
    }

    toStandardParam(defaultParam)
  }

  def toStandardParam(list: List[(String, File)]): List[(String, OccurrenceContextIndexer)] = {
    var output: List[(String, OccurrenceContextIndexer)] = List()
    list.foreach{ element =>
      output = output :+ (element._1, toOccurrenceContextIndexer(element._2))
    }
    output
  }

  def toOccurrenceContextIndexer(file: File) : OccurrenceContextIndexer = {
    val config = new IndexingConfiguration(indexingConfigFileName)
    val similarity = Factory.Similarity.fromName("InvCandFreqSimilarity")
    val analyzer = config.getAnalyzer

    val lucene = new LuceneManager.BufferedMerging(FSDirectory.open(file), 200000, false)
    lucene.setContextSimilarity(similarity)
    lucene.setDefaultAnalyzer(analyzer)

    if (!file.exists()) {
      lucene.shouldOverwrite = true
      file.mkdir()
    } else {
      lucene.shouldOverwrite = false
    }

    new MergedOccurrencesContextIndexer(lucene)
  }

  /* Example of how call and run this storage class */
  def main(arg: Array[String]) {
    val indexOutputBaseDir: String = "/home/alexandre/Projects/empty-index/"
    val outputC = ("correct", new File(indexOutputBaseDir + "index-correct"))
    val outputI = ("incorrect", new File(indexOutputBaseDir + "index-incorrect"))

    val lstore = new LuceneFeedbackStore(Array(outputC,outputI))

    val f1 = new SpotlightFeedback("Berlin is capital of Germany", "Berlin", "Berlin", 1, "correct", "user usere usera", true)
    lstore.add(f1)
    val f2 = new SpotlightFeedback("Berlin is capital of Germany", "", "news" , "Berlin_(band)", "Berlin", 1, "incorrect", "user user user", true, "english")
    lstore.add(f2)
  }
}