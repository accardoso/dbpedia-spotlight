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
 * Generates and registers new api key.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object Authentication {
  /* The path for the file containing the registered api keys */
  val apiKeysFilePath = "rest/src/main/scala/org/dbpedia/spotlight/web/rest/resources/.api-keys" //If Spotlight is running from .../dbpedia-spotlight/ directory.

  /* Public method to authenticate if it is or not a valid key */
  def authenticate(clientIp: String, key: String): Boolean = {
    if(!isRegistered(key))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)

    true
  }

  /* Public method to run the new api key generation and registration interface */
  def newKeyInterface(){
    SpotlightLog.info(this.getClass, "**** DBpedia-Spotlight new API key generation and registration interface ****" )
    print("Do you want to generate and register a new api key? [y|n]" )
    val line = Console.readLine

    if (line == "y"){
      var newKey: String = ""
      val tryTimes: Int = 10

      for(i <- 1 to tryTimes){
        newKey = generateKey()
        if(registerKey(newKey)){
          SpotlightLog.info(this.getClass, "The new Spotlight api key: %s", newKey)
          return
        }
      }
      SpotlightLog.info(this.getClass, "The authentication api key generator try %d times, but any new key was generated." +
        " Try again or contact the authentication module developer for more information.", tryTimes)
    }
    SpotlightLog.info(this.getClass, "No api key was registered.")
  }


  /* Verifies if the key is registered or not */
  private def isRegistered(key: String): Boolean = {
    Source.fromFile(apiKeysFilePath).getLines.foreach{
      line =>
        if(line == key)
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


  /* Main method runs the interface for generate and register a new api key */
  def main(args: Array[String]){
    newKeyInterface()
  }

}
