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
 *
 * @constructor output: List[(String,OccurrenceContextIndexer)] -> A list o Tuple2 where the first element is the feedback possibility and the second is the respective Lucene index.
 * @constructor output: List[(String,File)] -> A list o Tuple2 where the first element is the feedback possibility and the second is the respective Lucene index folder (a OccurrenceContextIndexer is auto-created to open and write in this index).
 */

class LuceneFeedbackStore(output: List[(String,OccurrenceContextIndexer)]) extends FeedbackStore {
  def this(output: Array[(String, File)]) = this(LuceneFeedbackStore.toStandardParam(output.toList))
  /*def this(baseLuceneIndex: File) = this(LuceneFeedbackStore.createDefaultFiles(baseLuceneIndex))*/

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

  override def addAll(src: List[SpotlightFeedback]) {
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

  /*def createDefaultFiles(baseLuceneIndex: File): List[(String, OccurrenceContextIndexer)] = {
    if(!baseLuceneIndex.exists())
      throw new NoSuchElementException("Could not find the lucene index directory at %s".format(baseLuceneIndex.getCanonicalPath))
    if(!baseLuceneIndex.isDirectory())
      throw new IllegalArgumentException("The informed base lucene index must be a directory containing a lucene index.")

    var defaultParam: List[(String, File)] = List()
    var indexPossibilityDir: File = null
    SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
      throw new Exception("Method implementation is not finished!")
      //TODO: indexPossibilityDir = /* copy of the base Index directory, and this copy is renamed as "index-<feedback_possibility>" */
      defaultParam = defaultParam :+ (possibility, indexPossibilityDir)
    }

    toStandardParam(defaultParam)
  }
*/

  def toStandardParam(list: List[(String, File)]): List[(String, OccurrenceContextIndexer)] = {
    var output: List[(String, OccurrenceContextIndexer)] = List()
    list.foreach{ element =>
      output = output :+ (element._1, toOccurrenceContextIndexer(element._2))
    }
    output
  }

  def toOccurrenceContextIndexer(file: File) : OccurrenceContextIndexer = {
    /* Reference: /org/dbpedia/spotlight/lucene/index/IndexMergedOccurrences.scala (by maxjakob) */
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

  /*
   Examples of how call and run this storage class
   (created when the feedback possibilities were "correct"/"incorrect" only)
  */
  def usageExample1() {
    val outputToFeedbackPossibility0 = ("correct", new File("/home/alexandre/Projects/empty-index/index-correct"))
    val outputToFeedbackPossibility1 = ("incorrect", new File("/home/alexandre/Projects/empty-index/index-incorrect"))

    val luceneStore = new LuceneFeedbackStore(Array(outputToFeedbackPossibility0,outputToFeedbackPossibility1))

    val feedback1 = new SpotlightFeedback("Berlin is capital of Germany", "Berlin", "Berlin", 1, "correct", "user0 user1 user2", true)
    val feedback2 = new SpotlightFeedback("Berlin is capital of Germany", "", "news" , "Berlin_(band)", "Berlin", 1, "incorrect", "user2 user4", true, "english")
    luceneStore.addAll(List(feedback1, feedback2))
  }
  def usageExample2() {
    val outputToFeedbackPossibility0 = ("correct", new File("/home/alexandre/Projects/empty-index/index-correct"))
    val outputToFeedbackPossibility1 = ("incorrect", new File("/home/alexandre/Projects/empty-index/index-incorrect"))

    val multiStore: FeedbackMultiStore = new FeedbackMultiStore
    multiStore.registerStore(new LuceneFeedbackStore(Array(outputToFeedbackPossibility0,outputToFeedbackPossibility1)))

    val feedback1 = new SpotlightFeedback("Berlin is capital of Germany", "Berlin", "Berlin", 1, "correct", "user0 user1 user2", true)
    multiStore.storeFeedback(feedback1)
  }

  def main(arg: Array[String]) {
    usageExample1()
  }
}