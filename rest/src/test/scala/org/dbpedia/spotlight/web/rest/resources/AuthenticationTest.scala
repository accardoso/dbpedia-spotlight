package org.dbpedia.spotlight.web.rest.resources

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.io.Source
import scala.Console
import org.dbpedia.spotlight.log.SpotlightLog

/**
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class AuthenticationTest extends FlatSpec with ShouldMatchers {
  AuthenticationTest.initialization()

  /* Generation and Registration Interface Tests */

  "Authentication main without any argument" should "execute the new key generation and registration interface" in {
    AuthenticationTest.runNewKeyInterface("n")

    //Get the first printed line, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  "Authentication main without any argument (if user has not confirm the operation, type \"n\")" should
    "not execute the new key generation and registration interface" in {
    AuthenticationTest.runNewKeyInterface("n")

    //Get the last printed line, which shall be the warning that no api key registration.
    val output: String = AuthenticationTest.getOutputContent.mkString
    //The last printed line of the executed interface should be the warning that no api key registration.
    val warnMsg = "No api key was registered."
    output.substring(output.length - warnMsg.length) should be === warnMsg
  }

  it should "not generate neither register any key" in {
    AuthenticationTest.newKey should be === ""
  }

  "Authentication main without any argument (if user has confirm the operation, type \"y\")" should
    "execute the new key generation and registration interface" in {

    AuthenticationTest.runNewKeyInterface("y")

    //Get the first line printed, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  it should "generate and register a valid (25 characters) api key" in {
    AuthenticationTest.newKey.length should be === 25
  }

  it should "register and inform the user the same new api key" in {
    //Get the key which was informed to the user. (Important: assume that the key is the last thing to be printed.)
    var newKeyAtOutputStream: String = Source.fromFile(AuthenticationTest.testOutputStream).getLines().mkString
    newKeyAtOutputStream = newKeyAtOutputStream.substring(newKeyAtOutputStream.length - AuthenticationTest.newKey.length)

    //Both the stored new key and the key informed to the user should be the same
    AuthenticationTest.newKey should be === newKeyAtOutputStream
  }

  /* Api key authentication Tests */



  /* Removal Interface Tests */



  /* Statistical key validation Tests */

  "Authentication" should "generate and register a set (30 samples) of valid (25 characters) api keys" in {

  }

  //AuthenticationTest.finalization()
}

object AuthenticationTest {

  private var apiKeysFile: File = null
  private var bkpApiKeysFile: File = null

  private val testOutputStream: File = new File("AuthenticationTest.out.tmp")

  private def getApiKeysFileContent = Source.fromFile(AuthenticationTest.apiKeysFile).getLines()
  private def getOutputContent = Source.fromFile(AuthenticationTest.testOutputStream).getLines()

  private var newKey: String = ""

  private def runNewKeyInterface(userInput: String) {
    cleanFile(apiKeysFile)
    runAuthenticationMain(Array[String](), userInput)
    //Get the key stored at api keys file (to be used by the all tests without need to run this interface again)
    newKey = AuthenticationTest.getApiKeysFileContent.mkString("").replace("\n", "")
  }

  private def runAuthenticationMain(mainArgs: Array[String], userInput: String): OutputStream = {
    val inputStream = new ByteArrayInputStream(userInput.getBytes())
    val outputStream = new FileOutputStream(AuthenticationTest.testOutputStream)
    Console.withIn(inputStream){
      Console.withOut(outputStream){
        Authentication.main(mainArgs)
      }
    }
    outputStream
  }

  def copyFile(src: File, dest: File) {
    if(!src.exists())
      cleanFile(src) //It will write a empty file at src, and this will create a empty file
    if(!src.isFile)
      throw new IllegalArgumentException("The informed source to be copied must be a file and %s is not.".format(src.getCanonicalPath))

    val reader = Source.fromFile(src)
    val writer = new FileWriter(dest, true)
    var firstLine: Boolean = true
    reader.getLines.foreach{ line =>
      if(firstLine){
        firstLine = false
        writer.append("\n")
      }
      writer.append(line)
      writer.flush()
    }
    reader.close()
    writer.close()
  }

  private def createBkpApiKeysFile(): File = {
    val bkp = new File(apiKeysFile.getCanonicalPath + ".bkp.authentication.test.tmp")
    //Ensures that bkp is empty
    cleanFile(bkp)
    //Make a back up of the the original spotlight api keys file
    copyFile(apiKeysFile, bkp)
    //Return the bkp
    bkp
  }

  private def cleanFile(file: File) {
    val writer = new FileWriter(file, false)
    writer.write("")
    writer.close()
  }

  private def restoreApiKeysFile() {
    //Clean and copy the bkp back to the api keys file. It is slower but safer then delete the current apiKeysFile and rename the bkp file
    cleanFile(apiKeysFile)
    copyFile(bkpApiKeysFile, apiKeysFile)

    //Delete the temp bkp
    if(!bkpApiKeysFile.delete())
      SpotlightLog.warn(this.getClass, "Could not delete the temporary back up of the api keys file: %s " +
                                        "But the original api keys file was successfully restored.", bkpApiKeysFile.getCanonicalPath)
  }

  private def initialization(){
    apiKeysFile = new File(Authentication.getApiKeysFilePath)
    bkpApiKeysFile = createBkpApiKeysFile()
  }

  private def finalization() = restoreApiKeysFile()

}