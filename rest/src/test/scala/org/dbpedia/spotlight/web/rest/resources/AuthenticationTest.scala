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
 * This ScalaTest test the Authentication class interface for generation and registration as the removal interface.
 * And the authentication of a informed key method.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class AuthenticationTest extends FlatSpec with ShouldMatchers {
  //Get the api keys file used by Authentication class and back it up
  AuthenticationTest.initialization()

  /* Generation and Registration Interface Tests */

  "Authentication.main without any argument" should "execute the new key generation and registration interface" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface
    AuthenticationTest.runAuthenticationMain(Array[String](), "")

    //Get the first printed line, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  "Authentication.main without any argument, if user has not confirm the operation (type \"n\")" should
    "abort the execution the new key generation and registration interface" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface but cancel the execution, typing n from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](), "n")

    //Get the last printed line, which shall be the warning that no api key registration.
    val output: String = AuthenticationTest.getOutputContent.mkString
    //The last printed line of the executed interface should be the warning that no api key registration.
    val warnMsg = "No api key was registered."
    output.substring(output.length - warnMsg.length) should be === warnMsg
  }

  it should "not generate neither register any key" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface but cancel the execution, typing n from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](), "n")
    //The api keys should be empty as it was cleaned and no key was registered
    AuthenticationTest.getApiKeysFileContent.mkString("") should be === ""
  }

  "Authentication.main without any argument, if user has confirm the operation (type \"y\")" should
    "execute the new key generation and registration interface" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution, typing y from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")

    //Get the first line printed, which shall be the header of the executed interface
    val interfaceHeader: String = AuthenticationTest.getOutputContent.toList.head.mkString
    //The header of the executed interface must be the header of the generation and registration interface
    interfaceHeader should be === "**** DBpedia-Spotlight new API key generation and registration interface ****"
  }

  it should "generate and register a valid (25 characters) api key" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution, typing y from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    //The api keys file content, as it was cleaned and the main called just 1 time, shall have only 1 key.
    //And this key should be valid (has 25 characters of length)
    AuthenticationTest.getApiKeysFileContent.mkString("").length should be === 25
  }

  it should "register and inform the user the same new api key" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution, typing y from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    //Get the api keys registered in the file (as it was cleaned and the main called just 1 time)
    val newKeyAtApiKeysFile = AuthenticationTest.getApiKeysFileContent.mkString("")
    //Get the key which was informed to the user (assume that the key is the last thing to be printed.)
    var newKeyAtOutputStream: String = AuthenticationTest.getOutputContent.mkString
    newKeyAtOutputStream = newKeyAtOutputStream.substring(newKeyAtOutputStream.length - newKeyAtApiKeysFile.length)

    //Both the stored new key and the key informed to the user should be the same
    newKeyAtApiKeysFile should be === newKeyAtOutputStream
  }

  /* Api key authentication Tests */

  "Authentication.authenticate" should "accept a registered key" in{
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution to register the key that the test shall authenticate
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    //authorized represents if the authenticate method validate the key as a registered one or not
    var authorized = false
    try{
      authorized = Authentication.authenticate("127.0.0.1",AuthenticationTest.getApiKeysFileContent.mkString(""))
    } catch {
      //If the key is not registered the authenticate method trow a WebApplicationException
      case e: WebApplicationException => //authorized is false already
    }
    authorized should be === true
  }

  it  should "not accept a valid unregistered key" in{
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution to register a key
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    val newKey = AuthenticationTest.getApiKeysFileContent.mkString("")
    //authorized represents if the authenticate method validate the key as a registered one or not
    var authorized = true
    try{
      //Generate a valid unregistered key modifing the unique registered key
      var unregisteredKey: String = "1" + newKey.substring(2)
      if(unregisteredKey == newKey)
        unregisteredKey = "2" + newKey.substring(2)
      //Run the authenticate method for the unregistered valid generated key
      authorized = Authentication.authenticate("127.0.0.1", unregisteredKey)
    } catch {
      //If the key is not registered the authenticate method trow a WebApplicationException
      //And the server response must be UNAUTHORIZED (401)
      case e: WebApplicationException => {
        authorized = false
        e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
      }
    }
    //The authenticate method should not accept an unregistered key
    authorized should be === false
  }

  it  should "not accept \"\\n\" as key" in{
    var authorized = true
    try{
      authorized = Authentication.authenticate("127.0.0.1","\n")
    } catch {
      //If the key is not registered the authenticate method trow a WebApplicationException
      //And the server response must be UNAUTHORIZED (401)
      case e: WebApplicationException => {
        authorized = false
        e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
      }
    }
    authorized should be === false
  }

  it  should "not accept \"\" as key" in{
    var authorized = true
    try{
      authorized = Authentication.authenticate("127.0.0.1","")
    } catch {
      //If the key is not registered the authenticate method trow a WebApplicationException
      //And the server response must be UNAUTHORIZED (401)
      case e: WebApplicationException => {
        authorized = false
        e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
      }
    }
    authorized should be === false
  }

  it  should "not accept \" \" as key" in{
    var authorized = true
    try{
      authorized = Authentication.authenticate("127.0.0.1"," ")
    } catch {
      //If the key is not registered the authenticate method trow a WebApplicationException
      //And the server response must be UNAUTHORIZED (401)
      case e: WebApplicationException => {
        authorized = false
        e.getResponse.getStatus should be === Response.Status.UNAUTHORIZED.getStatusCode
      }
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
    //Run removal interface
    AuthenticationTest.runAuthenticationMain(Array[String]("any arg"), "n")

    //Get the last printed line, which shall be the warning that no api key registration.
    val output: String = AuthenticationTest.getOutputContent.mkString
    //The last printed line of the executed interface should be the warning that no api key registration.
    val warnMsg = "Api key removal canceled!"
    output.substring(output.length - warnMsg.length) should be === warnMsg
  }

  it should "remove no api key" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run generation and registration interface and confirm the execution to register the key, to ensure that api key is not empty
    AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    //Get the registered key by the above run of generation and registration interface
    //(as the api keys file was cleaned an the main executed just 1 time the content of the api keys file is 1 key only)
    val newKey = AuthenticationTest.getApiKeysFileContent.mkString("")

    //Run removal interface and cancel the execution, typing n from the not standard input
    //(The registered key is informed to ensure the the removal abortion was done by typing n from the input and not by informing an unregistered key)
    AuthenticationTest.runAuthenticationMain(Array[String](newKey), "n")

    //The registered key should still listed at api keys file
    newKey should be === AuthenticationTest.getApiKeysFileContent.mkString("")
  }

  "Authentication.main with a api key as argument, if user has confirm the operation (type \"y\")" should
    "remove the informed api key only" in {
    //Ensure the api keys file is empty
    AuthenticationTest.cleanFile(AuthenticationTest.apiKeysFile)
    //Run 30 (statistical number of samples) times generation and registration interface and confirm the execution,
    //to register a set of new api keys
    for(i<-1 to 30){
      AuthenticationTest.runAuthenticationMain(Array[String](), "y")
    }
    //Get all registered keys
    var registeredKeys: List[String] = AuthenticationTest.getApiKeysFileContent.toList

    //Remove 1 key (choose the middle is cheaper than a random)
    val keyToRemove: String = registeredKeys(registeredKeys.length/2)
    //The expected registered keys after the removal are the same excepted by the one removal
    registeredKeys = registeredKeys diff List(keyToRemove) //remove keyToRemove from registeredKeys

    //Run removal interface and confirm the execution, typing y from the not standard input
    AuthenticationTest.runAuthenticationMain(Array[String](keyToRemove), "y")
    //Get the registered keys from api keys file after the removal
    val keysAfterRemoval = AuthenticationTest.getApiKeysFileContent.toList

    //The api keys file should not contain the removed key
    keysAfterRemoval.contains(keyToRemove) should be === false
    //The api keys file should still contain all other keys that was not removed (so be equal to the expected registeredKeys defined above)
    keysAfterRemoval.mkString("\n") should be === registeredKeys.mkString("\n")
  }

  /* This test is only to ensure that the original api keys file will be recovered
  (If method finalization was called outside a test the ScalaTest would ignore it) */
  "This test class" should "recover the original api keys file" in {
    //Restore the original api keys file and delete the temp files used by the tests
    AuthenticationTest.finalization() should be === true
  }
}

object AuthenticationTest {

  /* The api keys file used by Authentication class */
  private var apiKeysFile: File = null
  /* A bakc up of the original api keys file, to recover it after the test modifications */
  private var bkpApiKeysFile: File = null

  /* Where the temporary output will be printed in the place of stdout */
  private val testOutputStream: File = new File("AuthenticationTest.out.tmp")

  /* Get the current content of api keys file */
  private def getApiKeysFileContent = Source.fromFile(apiKeysFile).getLines().filterNot(_.equals(""))
  /* Get the content of the not standard output used during the Authentication.main last execution */
  private def getOutputContent = Source.fromFile(testOutputStream).getLines()

  /* Run Authentication.main with the informed argument array and using a not standard input an output */
  private def runAuthenticationMain(mainArgs: Array[String], userInput: String) {
    //Define an input stream with the userInput
    val inputStream = new ByteArrayInputStream(userInput.getBytes())
    //Define an output stream that will print to the testOutputStream file
    val outputStream = new FileOutputStream(testOutputStream)
    //In this block the stdin is replaced by inputStream
    Console.withIn(inputStream){
      //In this block the stdout is replaced by outputStream
      Console.withOut(outputStream){
        //Run the Authentication main with the informed array of arguments and using the above defined stream in the place of the standard ones
        Authentication.main(mainArgs)
      }
    }
  }

  /* Make the informed file a empty file, with nothing wrote */
  def cleanFile(file: File) {
    //Clean up a file is the same of overwrite the existing file with an empty one
    val writer = new FileWriter(file, false)
    writer.write("")
    writer.close()
  }

  /* Get the api keys file used by Authentication and back it up */
  private def initialization(){
    //Set apiKeysFile to the api keys file used by Authentication
    apiKeysFile = new File(Authentication.getApiKeysFilePath)

    /* Back up the api keys file into a new bkp file */
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

  /* Remove temp files and Restore the original api keys file */
  private def finalization(): Boolean = {
    //Delete the temporary output stream file
    if(!testOutputStream.delete)
      SpotlightLog.warn(this.getClass, "Could not delete the temporary output stream file: %s ", testOutputStream.getCanonicalPath)

    /* Restore the api keys files from its back up file */
    def restoreApiKeysFile(): Boolean = {
      //Copy back the bkp to the api keys file (It is slower but safer then delete the current apiKeysFile and rename the bkp file)
      val writer = new FileWriter(apiKeysFile, false)
      writer.write(Source.fromFile(bkpApiKeysFile).getLines().mkString("\n"))
      writer.close()

      //Delete the temporary api keys back up file
      if(!bkpApiKeysFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary back up of the api keys file: %s " +
                                          "But the original api keys file was successfully restored.", bkpApiKeysFile.getCanonicalPath)

      true
    }

    /* Restore the original api keys file and return true for success */
    restoreApiKeysFile()
  }

}