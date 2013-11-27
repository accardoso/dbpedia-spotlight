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
 * This ScalaTest test the storage class TSVFeedbackStoreTest, which extends FeedbackStore.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class TSVFeedbackStoreTest extends FlatSpec with ShouldMatchers {

  val storage: TSVFeedbackStore = new TSVFeedbackStore(TSVFeedbackStoreTest.storageFile.getCanonicalPath.replace(TSVFeedbackStoreTest.storageFile.getName, "")) //Use the constructor farthest from the root constructor, so all the class constructors are tested here

  "One SpotlightFeedback" should "be stored (appended) correctly into the tsv storage" in {
    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFile)
    
    storage.add(TSVFeedbackStoreTest.feedback)
    
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === TSVFeedbackStoreTest.feedback.mkString("\t")
  }
  
  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("\n")

    for(i <- 1 to 3){
      storage.add(TSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }

    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }
  
  "All SpotlightFeedback batch/list" should "be stored (appended) correctly into the tsv storage" in {
    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFile)
    
    var batch: List[SpotlightFeedback] = List()
    var expectedStorageContent = ""
    for(i <- 1 to 5){
      batch = batch :+ TSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    
    storage.addAll(batch)
    
    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  it should "be stored without removing any other already stored feedback" in {
    var expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("")
    for(i <- 1 to 3){
      storage.add(TSVFeedbackStoreTest.feedback)
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }

    var batch: List[SpotlightFeedback] = List()
    for(i <- 1 to 5){
      batch = batch :+ TSVFeedbackStoreTest.feedback
      expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.feedback.mkString("\t")
    }
    storage.addAll(batch)

    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }

  "All SpotlightFeedback loaded by a FeedbackLoader" should "be converted correctly into the tsv storage" in {
    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFileToBeLoaded)
    (new TSVFeedbackStore(TSVFeedbackStoreTest.storageFileToBeLoaded)).addAll(List(TSVFeedbackStoreTest.feedback, TSVFeedbackStoreTest.feedback, TSVFeedbackStoreTest.feedback))
    val expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFileToBeLoaded).mkString("")

    TSVFeedbackStoreTest.cleanFile(TSVFeedbackStoreTest.storageFile)
    storage.convertFrom(new TSVFeedbackLoader(TSVFeedbackStoreTest.storageFileToBeLoaded))

    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }
  
  it should "be converted correctly into the tsv storage without removing any other already stored feedback" in {
    var expectedStorageContent = TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("")

    (new TSVFeedbackStore(TSVFeedbackStoreTest.storageFileToBeLoaded)).addAll(List(TSVFeedbackStoreTest.feedback, TSVFeedbackStoreTest.feedback, TSVFeedbackStoreTest.feedback))
    expectedStorageContent = expectedStorageContent + TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFileToBeLoaded).mkString("")
    
    storage.convertFrom(new TSVFeedbackLoader(TSVFeedbackStoreTest.storageFileToBeLoaded))    

    TSVFeedbackStoreTest.getFileContent(TSVFeedbackStoreTest.storageFile).mkString("") should be === expectedStorageContent
  }
  
  "The temporary storage file create for this test" should "be deleted" in {
    TSVFeedbackStoreTest.storageFile.delete() should be === true
    TSVFeedbackStoreTest.storageFileToBeLoaded.delete() should be === true
  }

}

object TSVFeedbackStoreTest {
  val storageFile: File = new File("feedbackStore.tsv")
  val storageFileToBeLoaded = new File("feedbackStoreToBeLoaded.tmp.tsv")
  
  def cleanFile(file: File) = (new FileWriter(file, false)).write("")
  def getFileContent(file: File) = Source.fromFile(file).getLines().filterNot(_.equals(""))

  def feedback: SpotlightFeedback = new SpotlightFeedback(new Text("Berlin is capital of Germany"),
                  new URL("http://www.berlin.de"), "news", new DBpediaResource("Berlin"), new SurfaceForm("Berlin"), 1,
                  "correct", List[String]("spotlight_lucene", "spotlight_statistical"), false, "english")
}
