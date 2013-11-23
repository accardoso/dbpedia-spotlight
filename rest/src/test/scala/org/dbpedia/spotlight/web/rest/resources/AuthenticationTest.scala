package org.dbpedia.spotlight.web.rest.resources

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io._
import scala.io.Source
import scala.Console
import org.dbpedia.spotlight.log.SpotlightLog
import javax.ws.rs.WebApplicationException
import javax.ws.rs.core.Response

/**
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class AuthenticationTest extends FlatSpec with ShouldMatchers {
  AuthenticationTest.initialization()

  /* Generation and Registration Interface Tests */

  "Authentication.main without any argument" should "execute the new key generation and registration interface" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "")

    //Get the first printed line, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  "Authentication.main without any argument, if user has not confirm the operation (type \"n\")" should
    "abort the execution the new key generation and registration interface" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "n")

    //Get the last printed line, which shall be the warning that no api key registration.
    val output: String = AuthenticationTest.getOutputContent.mkString
    //The last printed line of the executed interface should be the warning that no api key registration.
    val warnMsg = "No api key was registered."
    output.substring(output.length - warnMsg.length) should be === warnMsg
  }

  it should "not generate neither register any key" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "n")
    AuthenticationTest.getApiKeysFileContent.mkString("") should be === ""
  }

  "Authentication.main without any argument, if user has confirm the operation (type \"y\")" should
    "execute the new key generation and registration interface" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")

    //Get the first line printed, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  it should "generate and register a valid (25 characters) api key" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    AuthenticationTest.getApiKeysFileContent.mkString("").length should be === 25
  }

  it should "register and inform the user the same new api key" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    val newKeyAtApiKeysFile = AuthenticationTest.getApiKeysFileContent.mkString("")
    //Get the key which was informed to the user. (Important: assume that the key is the last thing to be printed.)
    var newKeyAtOutputStream: String = AuthenticationTest.getOutputContent.mkString
    newKeyAtOutputStream = newKeyAtOutputStream.substring(newKeyAtOutputStream.length - newKeyAtApiKeysFile.length)

    //Both the stored new key and the key informed to the user should be the same
    newKeyAtApiKeysFile should be === newKeyAtOutputStream
  }

  /* Api key authentication Tests */

  "Authentication.authenticate" should "accept a registered key" in{
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    var authorized = false
    try{
      authorized = Authentication.authenticate("127.0.0.1",AuthenticationTest.getApiKeysFileContent.mkString(""))
    } catch {
      case e: WebApplicationException => //authorized is false already
    }
    authorized should be === true
  }

  it  should "not accept a valid unregistered key" in{
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    val newKey = AuthenticationTest.getApiKeysFileContent.mkString("")
    var authorized = false
    try{
      var unregisteredKey: String = "1" + newKey.substring(2)
      if(unregisteredKey == newKey)
        unregisteredKey = "2" + newKey.substring(2)
      authorized = Authentication.authenticate("127.0.0.1", unregisteredKey)
    } catch {
      case e: WebApplicationException => e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
    }
    authorized should be === false
  }

  it  should "not accept \"\\n\" as key" in{
    var authorized = false
    try{
      authorized = Authentication.authenticate("127.0.0.1","\n")
    } catch {
      case e: WebApplicationException => e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
    }
    authorized should be === false
  }

  it  should "not accept \"\" as key" in{
    var authorized = false
    try{
      authorized = Authentication.authenticate("127.0.0.1","")
    } catch {
      case e: WebApplicationException => e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
    }
    authorized should be === false
  }

  it  should "not accept \" \" as key" in{
    var authorized = false
    try{
      authorized = Authentication.authenticate("127.0.0.1"," ")
    } catch {
      case e: WebApplicationException => e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
    }
    authorized should be === false
  }

  /* Removal Interface Tests */

  "Authentication.main with an argument" should "execute the api key removal interface" in {
    AuthenticationTest.runAuthenticationMain(Array[String]("any arg"), "")

    //Get the first printed line, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight API key removal interface ****"
  }

  "Authentication.main with a api key as any argument, if user has not confirm the operation (type \"n\")" should
    "abort the execution the api key removal interface" in {
    AuthenticationTest.runAuthenticationMain(Array[String]("any arg"), "n")

    //Get the last printed line, which shall be the warning that no api key registration.
    val output: String = AuthenticationTest.getOutputContent.mkString
    //The last printed line of the executed interface should be the warning that no api key registration.
    val warnMsg = "Api key removal canceled!"
    output.substring(output.length - warnMsg.length) should be === warnMsg
  }

  it should "remove no api key" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    val newKey = AuthenticationTest.getApiKeysFileContent.mkString("")

    AuthenticationTest.runAuthenticationMain(Array[String](newKey), "n")
    newKey should be === AuthenticationTest.getApiKeysFileContent.mkString("")
  }

  "Authentication.main with a api key as argument, if user has confirm the operation (type \"y\")" should
    "remove the informed api key only" in {
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    for(i<-1 to 30){
      AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    }
    var registeredKeys: List[String] = AuthenticationTest.getApiKeysFileContent.toList

    val keyToRemove: String = registeredKeys(registeredKeys.length/2)
    registeredKeys = registeredKeys diff List(keyToRemove) //remove keyToRemove from registeredKeys

    AuthenticationTest.runAuthenticationMain(Array[String](keyToRemove), "y")
    val keysAfterRemoval = AuthenticationTest.getApiKeysFileContent.toList
    keysAfterRemoval.contains(keyToRemove) should be === false
    keysAfterRemoval.mkString("\n") should be === registeredKeys.mkString("\n")
  }

  "This test class" should "recover the original api keys file" in {
    AuthenticationTest.finalization() should be === true
  }
}

object AuthenticationTest {

  private var apiKeysFile: File = null
  private var bkpApiKeysFile: File = null

  private val testOutputStream: File = new File("AuthenticationTest.out.tmp")

  private def getApiKeysFileContent = Source.fromFile(apiKeysFile).getLines().filterNot(_.equals(""))
  private def getOutputContent = Source.fromFile(testOutputStream).getLines()

  private def runAuthenticationMain(mainArgs: Array[String], userInput: String) {
    val inputStream = new ByteArrayInputStream(userInput.getBytes())
    val outputStream = new FileOutputStream(AuthenticationTest.testOutputStream)
    Console.withIn(inputStream){
      Console.withOut(outputStream){
        Authentication.main(mainArgs)
      }
    }
  }

  def cleanFile(file: File) {
    val writer = new FileWriter(file, false)
    writer.write("")
    writer.close()
  }

  private def initialization(){
    apiKeysFile = new File(Authentication.getApiKeysFilePath)

    def createBkpApiKeysFile(): File = {
      val bkp = new File(apiKeysFile.getCanonicalPath + ".bkp.authentication.test.tmp")
      //Make a back up of the the original spotlight api keys file
      val writer = new FileWriter(bkp, false)
      writer.write(Source.fromFile(apiKeysFile).getLines().mkString("\n"))
      writer.close()
      //Return the bkp
      bkp
    }

    bkpApiKeysFile = createBkpApiKeysFile()
  }

  private def finalization(): Boolean = {
    if(!testOutputStream.delete)
      SpotlightLog.warn(this.getClass, "Could not delete the temporary output stream file: %s ", testOutputStream.getCanonicalPath)

    def restoreApiKeysFile(): Boolean = {
      //Copy back the bkp to the api keys file (It is slower but safer then delete the current apiKeysFile and rename the bkp file)
      val writer = new FileWriter(apiKeysFile, false)
      writer.write(Source.fromFile(bkpApiKeysFile).getLines().mkString("\n"))
      writer.close()

      //Delete the temp bkp
      if(!bkpApiKeysFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary back up of the api keys file: %s " +
                                          "But the original api keys file was successfully restored.", bkpApiKeysFile.getCanonicalPath)

      true
    }

    restoreApiKeysFile()
  }

}