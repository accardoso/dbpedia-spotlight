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
      //Try to store a unique feedback into a multiStore with no store registered
      (new FeedbackMultiStore()).storeFeedback(FeedbackMultiStoreTest.feedback)
    } catch {
      case e: NullPointerException =>  success = true //Multi store has not accepted this try of storage
    }

    success should be === true
  }

  it  should "not store any batch of SpotlightFeedback" in {
    var success = false
    try{
      //Try to store a batch of feedback into a multiStore with no store registered
      (new FeedbackMultiStore()).storeFeedbackBatch(List(FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback, FeedbackMultiStoreTest.feedback))
    } catch {
      case e: NullPointerException => success = true  //Multi store has not accepted this try of storage
    }

    success should be === true
  }
  
  "A FeedbackStore registration" should "be done when the long constructor is used" in {
    //Construct the multiStore using the constructor with the list of stores
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
                                                new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage),
                                                new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage)))

    //Close multiStore registered stores that need to be finalized
    multiStore.close()

    //The above constructor should registered all informed stores (so its toString should be different of an empty multiStore toString
    multiStore.toString() should not be === (new FeedbackMultiStore()).toString()
  }
  
  it should "be done by the registration method" in {
    //Create a empty multiStore
    val multiStore = new FeedbackMultiStore()
    //Get its toString when it is empty
    val noRegisteredStoreToString:String = multiStore.toString()

    //Register some stores to the multiStore
    multiStore.registerStore(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage))
    multiStore.registerStore(new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage))
    multiStore.registerStore(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage))

    //Close multiStore registered stores that need to be finalized
    multiStore.close()

    //The above constructor should registered all informed stores (so its toString should be different of an empty multiStore toString
    multiStore.toString() should not be === (noRegisteredStoreToString)
  }

  "A FeedbackMultiStore with registered FeedbackStores" should "store some SpotlightFeedback in every store" in {
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
                                                 new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage)))
    //Ensure that storage files is empty
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.tsvStorage)
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.csvStorage)

    //Store some feedback and keep its records to verification
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

    //Define the expected content of the tsv storage
    var expectedStoredSpotlightFeedbackString: String = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString("\t")
    }
    //The content of the storage file should be the expected (as the file was empty before)
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.tsvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString

    //Define the expected content of the csv storage
    expectedStoredSpotlightFeedbackString = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString(",")
    }
    //The content of the storage file should be the expected (as the file was empty before)
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.csvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
  }

  it  should "store the batch of SpotlightFeedback in every store" in {
    val multiStore = new FeedbackMultiStore(List(new TSVFeedbackStore(FeedbackMultiStoreTest.tsvStorage),
      new CommaSVFeedbackStore(FeedbackMultiStoreTest.csvStorage)))

    //Ensure that storage files is empty
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.tsvStorage)
    FeedbackMultiStoreTest.cleanFile(FeedbackMultiStoreTest.csvStorage)

    //Store some feedback and keep its recods to verification
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

    //Define the expected content of the tsv storage
    var expectedStoredSpotlightFeedbackString: String = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString("\t")
    }
    //The content of the storage file should be the expected (as the file was empty before)
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.tsvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString

    //Define the expected content of the csv storage
    expectedStoredSpotlightFeedbackString = ""
    expectedStoredSpotlightFeedback.foreach{ feedback =>
      expectedStoredSpotlightFeedbackString = expectedStoredSpotlightFeedbackString + feedback.mkString(",")
    }
    //The content of the storage file should be the expected (as the file was empty before)
    FeedbackMultiStoreTest.getFileContent(FeedbackMultiStoreTest.csvStorage).mkString("") should be === expectedStoredSpotlightFeedbackString
  }

  "The temporary storage files and folder create for this test" should "be deleted" in {
    //Remove the temporary storage files used by the test
    if(FeedbackMultiStoreTest.tsvStorage.exists()) FeedbackMultiStoreTest.tsvStorage.delete() should be === true
    if(FeedbackMultiStoreTest.csvStorage.exists()) FeedbackMultiStoreTest.csvStorage.delete() should be === true
    //Remove the temporary storage directory used by the test
    if(FeedbackMultiStoreTest.testWarehouseDirectory.exists()) FeedbackMultiStoreTest.testWarehouseDirectory.delete() should be === true
  }
}

object FeedbackMultiStoreTest {
  //The temp directory to be used by the tests
  val testWarehouseDirectory = new File(FeedbackMultiStore.createStorageFolder("FeedbackMultiStoreTest.temp"))
  //The temp feedback storage files to be used by the tests
  val tsvStorage = new File(testWarehouseDirectory.getCanonicalPath() + File.separator + "FeedbackMultiStoreTest.tmp.tsv")
  val csvStorage = new File(testWarehouseDirectory.getCanonicalPath() + File.separator + "FeedbackMultiStoreTest.tmp.csv")

  //Clean up (empty) the informed file. Is the same as overwrite the informed file with nothing ("")
  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  //Get the lines of the informed file without the empty lines
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))

  //The SpotlightFeedback that will be used during the tests
  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
    new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
    "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}