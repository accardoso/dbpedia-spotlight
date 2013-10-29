package org.dbpedia.spotlight.web.rest.resources

import javax.ws.rs.core.Response
import javax.ws.rs.WebApplicationException
import scala.io.Source
import java.io.{File, FileWriter}
import org.dbpedia.spotlight.log.SpotlightLog
import java.security.MessageDigest
import java.math.BigInteger

/**
 * Authentication class for Spotlight server.
 * Verifies if the informed api key is a valid one. If not a valid one, throws an exception informing that it was unauthorized.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object Authentication {

  val apiKeysFile = "/home/alexandre/Projects/dbpedia-spotlight/rest/src/main/scala/org/dbpedia/spotlight/web/rest/resources/.api-keys"

  def authenticate(clientIp: String, key: String): Boolean = {
    if(!isRegistered(key))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)

    true
  }

  private def isRegistered(key: String): Boolean = {
    Source.fromFile(apiKeysFile).getLines.foreach{
      line =>
        if(line == key)
          return true
    }

    //If not returned yet then it not a valid key
    false
  }

  private def registerKey(newKey: String): Boolean = {
    //Verify if this key is available
    if(isRegistered(newKey)){
      SpotlightLog.info(this.getClass, "This key is not available.")
      false
    }

    //If it is available, append the new key to the end of apiKeyFile
    val writer = new FileWriter(new File(apiKeysFile ), true)
    writer.write("\n") //to separate from the last registered key
    writer.write(newKey)
    writer.close()

    true
  }

  private def generateRandomKey(): String = {
    val md5sum = MessageDigest.getInstance("MD5").digest()
    val bigInt = new BigInteger(1, md5sum)
    bigInt.toString(26)
  }


  def main(args: Array[String]){
    println(generateRandomKey())
    println(generateRandomKey())
    println(generateRandomKey())
    println(generateRandomKey())
    println(generateRandomKey())
  }

}
