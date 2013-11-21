package org.dbpedia.spotlight.web.rest.resources

import javax.ws.rs.core.Response
import javax.ws.rs.WebApplicationException
import scala.io.Source
import java.io.{File, FileWriter}
import org.dbpedia.spotlight.log.SpotlightLog
import java.security.MessageDigest
import java.math.BigInteger
import util.Random

/**
 * Authentication class for Spotlight server.
 * Verifies if the informed api key is a valid one. If not a valid one, throws an exception informing that it was unauthorized.
 * Generates and registers new api key. And remove a informed key.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object Authentication {

  val configFileName: String = "./conf/server.properties"

  /* The path for the file containing the registered api keys */
  val apiKeysFilePath: String = getApiKeysFile(configFileName)

  def getApiKeysFile(configFilePath: String): String = {
    AuthenticationConfig.setConfigFile(configFilePath)
    AuthenticationConfig.get("org.dbpedia.spotlight.authentication.apikeys", "rest/src/main/scala/org/dbpedia/spotlight/web/rest/resources/.api-keys")
  }

  /* Public method to authenticate if it is or not a valid key */
  def authenticate(clientIp: String, key: String): Boolean = {
    if(!isRegistered(key))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)

    true
  }

  /* Verifies if the key is registered or not */
  private def isRegistered(key: String): Boolean = {
    Source.fromFile(apiKeysFilePath).getLines.foreach{
      line =>
        if(line == key && line != "")
            return true
    }

    //If not returned yet then it not a valid key
    false
  }

  /* Register the new key, if it is available */
  private def registerKey(newKey: String): Boolean = {
    //Verify if this key is available
    if(isRegistered(newKey)){
      SpotlightLog.info(this.getClass, "This key is not available.")
      false
    }

    //If it is available, append the new key to the end of apiKeyFile
    val writer = new FileWriter(new File(apiKeysFilePath), true)
    writer.write("\n") //Separate from the last registered key, for sure
    writer.write(newKey)
    writer.close()

    true
  }

  /* Generate a 27 characters api-key */
  private def generateKey(): String = {
    //Generate the md5 hash for a random number
    val md5 = MessageDigest.getInstance("MD5").digest(Random.nextLong().toString().getBytes())
    val bigInt = new BigInteger(1, md5)
    //Represents bigInt using base 26 (all numbers and all alphabet letters) and convert it to String
    bigInt.toString(26)
  }

  /* Remove the informed key from the api keys file */
  private def removeKey(key: String): Boolean = {
    //Rename the original api keys file (it is safer than rename the new file after the copy and deletion of the original file)
    val oldFile = new File(apiKeysFilePath + ".old.tmp")
    val apiKeyFile = new File(apiKeysFilePath) 
    if(!apiKeyFile.renameTo(oldFile))
      throw new IllegalAccessException("Could not remove the key, because it is unable to modify the api key file at %s".format(apiKeysFilePath))

    //Copy all keys except the one to be deleted from the old api key file to the new one (already with the api keys file name/path)
    val reader = Source.fromFile(oldFile)
    val writer = new FileWriter(apiKeyFile, true)
    reader.getLines.foreach{
      line =>
        if(line != key && line != ""){
          writer.append("\n"+line)
          writer.flush()
        }
    }
    reader.close()
    writer.close()

    //Delete the old api keys file
    if(!oldFile.delete())
      SpotlightLog.warn(this.getClass(), "The key was removed correctly. But could not delete the temporary file at: %s", oldFile.getCanonicalPath)

  true
  }

  /* Public method to run the new api key generation and registration interface */
  def newKeyInterface(){
    SpotlightLog.info(this.getClass, "**** DBpedia-Spotlight new API key generation and registration interface ****" )
    print("Do you want to generate and register a new api key? [y|n]" )
    val line = Console.readLine

    if (line == "y"){
      var newKey: String = ""
      val tryTimes: Int = 100
      //Try to generate a new api key and register it (registration do not accept existing keys)
      for(i <- 1 to tryTimes){
        newKey = generateKey()
        if(registerKey(newKey)){
          //Inform the new key and return
          SpotlightLog.info(this.getClass, "The new Spotlight api key: %s", newKey)
          return
        }
      }
      //Give up to generate a new key
      SpotlightLog.info(this.getClass, "The authentication api key generator try %d times, but any new key was generated." +
        " Try again or contact the authentication module developer for more information.", tryTimes)
    }

    SpotlightLog.info(this.getClass, "No api key was registered.")
  }

  /* Public method to run the safe routine to remove api key */
  def removeKeyInterface(key: String){
    SpotlightLog.info(this.getClass, "**** DBpedia-Spotlight API key removal interface ****" )
    print("Do you want to remove this api key: %s ? [y|n]".format(key))
    val line = Console.readLine
    if (line == "y") {
      if(removeKey(key))
        SpotlightLog.info(this.getClass, "Successfully remove the api key: %s", key)
      else
        SpotlightLog.error(this.getClass, "Could not remove the api key: %s", key)
    }else
      SpotlightLog.info(this.getClass, "Api key removal canceled!")
  }


  /* Main method runs the interface for generate and register a new api key or for api key removal (if the key was informed as argument) */
  def main(args: Array[String]){
    if(args.length == 0)
      newKeyInterface()
    else //Assume that arg(0) is the key to be removed
      removeKeyInterface(args(0))
  }

}
