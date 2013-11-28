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
 * This ScalaTest test the storage class TSVFeedbackStore, which extends FeedbackStore.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class TSVFeedbackStoreTest extends FlatSpec with ShouldMatchers {

  val storage: TSVFeedbackStore = new TSVFeedbackStore(TSVFeedbackStoreTest.storageFile.getCanonicalPath.replace(TSVFeedbackStoreTest.storageFile.getName, "")) //Use the constructor farthest from the root constructor, so all the class constructors are tested here

  "One SpotlightFeedback" should "be stored (appended) correctly into the tsv storage" in {
    //Ensure that the storage file is empty
    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFile)
    //Add (store) a feedback to the storage
    storage.add(TSVFeedbackStoreTest.feedback)
    //The storage content should be exactly the stored feedback (as the storage empty was ensured before)
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === TSVFeedbackStoreTest.feedback.mkString("\t")
  }
  
  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("\n")
    //Add (store) some feedback to the storage (even the storage was empty it will store feedback before another storage)
    for(i <- 1 to 3){
      storage.add(TSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    //The storage content should be the stored feedback before this test and the stored feedback during this test
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }
  
  "All SpotlightFeedback batch/list" should "be stored (appended) correctly into the tsv storage" in {
    //Ensure that the storage file is empty
    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFile)
    //Create a batch of feedback
    var batch: List[SpotlightFeedback] = List()
    var expectedStorageContent = ""
    for(i <- 1 to 5){
      batch = batch :+ TSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    //Add (store) the batch of feedback to the storage
    storage.addAll(batch)
    //The storage content should be exactly the stored feedback (as the storage empty was ensured before)
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("")
    for(i <- 1 to 3){
      storage.add(TSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    //Create a batch of feedback
    var batch: List[SpotlightFeedback] = List()
    for(i <- 1 to 5){
      batch = batch :+ TSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    //Add (store) the batch of feedback to the storage
    storage.addAll(batch)
    //The storage content should be the stored feedback before this test and the stored feedback during this test
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }
  
  "The temporary storage file create for this test" should "be deleted" in {
    //Remove the temporary storage file used by the test
    TSVFeedbackStoreTest.storageFile.delete() should be === true
  }

}

object TSVFeedbackStoreTest {
  //The storage file setted to the FeedbackStore that will be used by the tests
  val storageFile: File = new File("feedbackStore.tsv")

  //Clean up (empty) the informed file. Is the same as overwrite the informed file with nothing ("")
  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  //Get the lines of the informed file without the empty lines
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))

  //The SpotlightFeedback that will be used during the tests
  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
                  new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
                  "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}
