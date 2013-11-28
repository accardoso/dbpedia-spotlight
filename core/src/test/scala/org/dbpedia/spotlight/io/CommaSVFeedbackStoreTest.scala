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

  val storage: CommaSVFeedbackStore = new CommaSVFeedbackStore(CommaSVFeedbackStoreTest.storageFile.getCanonicalPath.replace(CommaSVFeedbackStoreTest.storageFile.getName, "")) //Use the constructor farthest from the root constructor, so all the class constructors are tested here

  "One SpotlightFeedback" should "be stored (appended) correctly into the csv storage" in {
    CommaSVFeedbackStoreTest.cleanFile(CommaSVFeedbackStoreTest.storageFile)

    storage.add(CommaSVFeedbackStoreTest.feedback)

    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === CommaSVFeedbackStoreTest.feedback.mkString(",")
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("\n")

    for(i <- 1 to 3){
      storage.add(CommaSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }

    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  "All SpotlightFeedback batch/list" should "be stored (appended) correctly into the csv storage" in {
    CommaSVFeedbackStoreTest.cleanFile(CommaSVFeedbackStoreTest.storageFile)

    var batch: List[SpotlightFeedback] = List()
    var expectedStorageContent = ""
    for(i <- 1 to 5){
      batch = batch :+ CommaSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }

    storage.addAll(batch)

    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("")
    for(i <- 1 to 3){
      storage.add(CommaSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }

    var batch: List[SpotlightFeedback] = List()
    for(i <- 1 to 5){
      batch = batch :+ CommaSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + CommaSVFeedbackStoreTest.feedback.mkString(",")
    }
    storage.addAll(batch)

    CommaSVFeedbackStoreTest.getFileContent(CommaSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  "The temporary storage file create for this test" should "be deleted" in {
    CommaSVFeedbackStoreTest.storageFile.delete() should be === true
  }

}

object CommaSVFeedbackStoreTest {
  val storageFile: File = new File("feedbackStore.csv")

  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))

  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
    new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
    "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}