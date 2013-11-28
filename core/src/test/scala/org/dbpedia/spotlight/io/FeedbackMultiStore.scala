package org.dbpedia.spotlight.io.feedback

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.{FileWriter, File}
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResource, Text, SpotlightFeedback}
import java.net.URL
import scala.io.Source
import java.lang.NullPointerException

/**
 * This ScalaTest test the multiples stores management class FeedbackMultiStore.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class FeedbackMultiStoreTest extends FlatSpec with ShouldMatchers {

  "A FeedbackMultiStore with no FeedbackStore registered" should "not store any SpotlightFeedback" in {
    var success = false
    try{
      (new FeedbackMultiStore()).storeFeedback(FeedbackMultiStoreTest.feedback)
    } catch {
      case e: NullPointerException =>  success = true
    }

    success should be === true
  }

  it  should "not store any batch of SpotlightFeedback" in {
    var success = false
    try{
      (new FeedbackMultiStore()).storeFeedbackBatch(List(FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback))
    } catch {
      case e: NullPointerException =>  success = true
    }

    success should be === true
  }
  
  "A FeedbackStore registration" should "be done when the long constructor is used" in {    
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
                                                new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage),
                                                new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage)))

    multiStore.toString() should not be === (new FeedbackMultiStore()).toString()
  }
  
  it should "be done by the registration method" in {
    val multiStore = new FeedbackMultiStore()
    val noRegisteredStoreToString:String = multiStore.toString()
    
    multiStore.registerStore(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage))
    multiStore.registerStore(new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage))
    multiStore.registerStore(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage))

    multiStore.toString() should not be === (noRegisteredStoreToString)
  }

  "A FeedbackMultiStore with registered FeedbackStores" should "store some SpotlightFeedback in every store" in {
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
                                                 new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage)))

    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.tsvStorage)
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.csvStorage)

    var expectedStoredSpotlightFeedback: List[SpotlightFeedback] = List()

    var success = false
    try{
      for(i <- 1 to 3){
        val feedbackToStore = FeedbackMultiStoreTest.feedback
        expectedStoredSpotlightFeedback = expectedStoredSpotlightFeedback :+ feedbackToStore
        multiStore.storeFeedback(feedbackToStore)
      }
      success = true
    } catch {
      case e: NullPointerException => success = false
    }
    success should be === true
    var expectedStoredSpotlightFeedbackString: String = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString("\t")
    } 
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.tsvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
    expectedStoredSpotlightFeedbackString = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString(",")
    }
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.csvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
  }

  it  should "store the batch of SpotlightFeedback in every store" in {
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
      new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage)))

    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.tsvStorage)
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.csvStorage)

    var expectedStoredSpotlightFeedback: List[SpotlightFeedback] = List()

    var success = false
    try{
      for(i <- 1 to 3){
        val feedbackToStore:List[SpotlightFeedback] = List(FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback)
        expectedStoredSpotlightFeedback = expectedStoredSpotlightFeedback ++ feedbackToStore
        multiStore.storeFeedbackBatch(feedbackToStore)
      }
      success = true
    } catch {
      case e: NullPointerException => success = false
    }
    success should be === true
    var expectedStoredSpotlightFeedbackString: String = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString("\t")
    }
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.tsvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
    expectedStoredSpotlightFeedbackString = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString(",")
    }
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.csvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
  }

  "The temporary storage file create for this test" should "be deleted" in {
    if(FeedbackMultiStoreTest.tsvStorage.exists()) FeedbackMultiStoreTest.tsvStorage.delete() should be === true
    if(FeedbackMultiStoreTest.csvStorage.exists()) FeedbackMultiStoreTest.csvStorage.delete() should be === true
    if(FeedbackMultiStoreTest.testWarehouseDirectory.exists()) FeedbackMultiStoreTest.testWarehouseDirectory.delete() should be === true
  }
}

object FeedbackMultiStoreTest {
  val testWarehouseDirectory = new File(FeedbackMultiStore.createStorageFolder("FeedbackMultiStoreTest-Warehouse"))

  val tsvStorage = new File(testWarehouseDirectory.getCanonicalPath() + File.separator + "FeedbackMultiStoreTest.tmp.tsv")
  val csvStorage = new File(testWarehouseDirectory.getCanonicalPath() + File.separator + "FeedbackMultiStoreTest.tmp.csv")

  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))
  
  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
    new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
    "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}