package org.dbpedia.spotlight.io.feedback

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.{FileWriter, File}
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResource, Text, SpotlightFeedback}
import java.net.URL
import scala.io.Source

/**
 * This ScalaTest test the storage class CommaSVFeedbackStore, which extends FeedbackStore.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class CommaSVFeedbackStoreTest extends FlatSpec with ShouldMatchers {

  //The csv storage to be used in the tests
  val storage: CommaSVFeedbackStore = new CommaSVFeedbackStore(CommaSVFeedbackStoreTest.storageFile.getCanonicalPath.replace(CommaSVFeedbackStoreTest.storageFile.getName, "")) //Use the constructor farthest from the root constructor, so all the class constructors are tested here

  "One SpotlightFeedback" should "be stored (appended) correctly into the csv storage" in {
    //Ensure that the storage file is empty
    CommaSVFeedbackStoreTest.cleanFile(CommaSVFeedbackStoreTest.storageFile)
    //Add (store) a feedback to the storage
    storage.add(CommaSVFeedbackStoreTest.feedback)
    //The storage content should be exactly the stored feedback (as the storage empty was ensured before)
    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === CommaSVFeedbackStoreTest.feedback.mkString(",")
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("\n")
    //Add (store) some feedback to the storage (even the storage was empty it will store feedback before another storage)
    for(i <- 1 to 3){
      storage.add(CommaSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }
    //The storage content should be the stored feedback before this test and the stored feedback during this test
    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  "All SpotlightFeedback batch/list" should "be stored (appended) correctly into the csv storage" in {
    //Ensure that the storage file is empty
    CommaSVFeedbackStoreTest.cleanFile(CommaSVFeedbackStoreTest.storageFile)
    //Create a batch of feedback
    var batch: List[SpotlightFeedback] = List()
    var expectedStorageContent = ""
    for(i <- 1 to 5){
      batch = batch :+ CommaSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }
    //Add (store) the batch of feedback to the storage
    storage.addAll(batch)
    //The storage content should be exactly the stored feedback (as the storage empty was ensured before)
    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("")
    //Add a minimum feedback (ensure that the storage will have some feedback even it was empty before)
    for(i <- 1 to 3){
      storage.add(CommaSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }
    //Create a batch of feedback
    var batch: List[SpotlightFeedback] = List()
    for(i <- 1 to 5){
      batch = batch :+ CommaSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }
    //Add (store) the batch of feedback to the storage
    storage.addAll(batch)
    //The storage content should be the stored feedback before this test and the stored feedback during this test
    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  "The temporary storage file create for this test" should "be deleted" in {
    //Remove the temporary storage file used by the test
    CommaSVFeedbackStoreTest.storageFile.delete() should be === true
  }

}

object CommaSVFeedbackStoreTest {
  //The storage file setted to the FeedbackStore that will be used by the tests
  val storageFile: File = new File("feedbackStore.csv")

  //Clean up (empty) the informed file. Is the same as overwrite the informed file with nothing ("")
  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  //Get the lines of the informed file without the empty lines
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))

  //The SpotlightFeedback that will be used during the tests
  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
    new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
    "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}