package org.dbpedia.spotlight.web.rest.resources

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.{FileWriter, File}
import org.dbpedia.spotlight.exceptions.ConfigurationException

/**
 * This ScalaTest test verify if the behavior of the Authentication Config object is correct.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

@RunWith(classOf[JUnitRunner])
class AuthenticationConfigTest extends FlatSpec with ShouldMatchers {

  "AuthenticationConfig" should "set a file" in {
    AuthenticationConfig.setConfigFile(AuthenticationConfigTest.testConfigFile)
    AuthenticationConfig.getConfigFilePath should be === AuthenticationConfigTest.testConfigFile.getCanonicalPath
  }

  it should "get the right value of the informed config key" in {
    AuthenticationConfig.get(AuthenticationConfigTest.key) should be === AuthenticationConfigTest.value
  }

  it should "throw a ConfigurationException if the informed config key does not exist" in {
    var succeed: Boolean = false
    try{
      AuthenticationConfig.get(AuthenticationConfigTest.key + ".wrong")
    } catch {
      case e: ConfigurationException => succeed = true
    }
    succeed should be === true
  }

  it should "get the right value of the informed config key even if a default value was informed" in {
    AuthenticationConfig.get(AuthenticationConfigTest.key, AuthenticationConfigTest.value+"/default") should be === AuthenticationConfigTest.value
  }

  it should "get the default value if the informed config key does not exist and a default value was informed" in {
    AuthenticationConfig.get("authentication.apikeys", AuthenticationConfigTest.value+"/default") should be === AuthenticationConfigTest.value+"/default"
  }

  it should "throw a NoSuchElementException if the config file does not exist when setting the configuration file" in {
    AuthenticationConfigTest.deleteTestConfigFile()

    var succeed: Boolean = false
    try{
      AuthenticationConfig.setConfigFile(AuthenticationConfigTest.testConfigFile)
    } catch {
      case e: NoSuchElementException => succeed = true
    }
    succeed should be === true
  }

  it should "throw a NoSuchElementException if the config file does not exist when setting the configuration file from its path" in {
    AuthenticationConfigTest.deleteTestConfigFile()

    var succeed: Boolean = false
    try{
      AuthenticationConfig.setConfigFile(AuthenticationConfigTest.testConfigFile.getCanonicalPath)
    } catch {
      case e: NoSuchElementException => succeed = true
    }
    succeed should be === true
  }

  it should "throw a NoSuchElementException if the config file does not exist when it try to get the a value of a key even informing a default value" in{
    AuthenticationConfigTest.deleteTestConfigFile()

    var succeed: Boolean = false
    try{
      AuthenticationConfig.get(AuthenticationConfigTest.key, AuthenticationConfigTest.value+"/default")
    } catch {
      case e: NoSuchElementException => succeed = true
    }
    succeed should be === true
  }

  it should "throw a NoSuchElementException if the config file does not exist when it try to get the value of a key" in{
    AuthenticationConfigTest.deleteTestConfigFile()

    var succeed: Boolean = false
    try{
      AuthenticationConfig.get(AuthenticationConfigTest.key)
    } catch {
      case e: NoSuchElementException => succeed = true
    }
    succeed should be === true
  }

  it should "set a file from its path" in {
    AuthenticationConfig.setConfigFile(AuthenticationConfig.defaultConfigFilePath)
    new File(AuthenticationConfig.getConfigFilePath()).getCanonicalPath should be === new File(AuthenticationConfig.defaultConfigFilePath).getCanonicalPath
  }
}

object AuthenticationConfigTest {

  val key: String = "org.dbpedia.spotlight.authentication.config.test.key"
  val value: String = "authentication/config/test/value"
  val testConfigFileContent: String = key + "=" + value

  val testConfigFile: File = AuthenticationConfigTest.createTestConfigFile() //Define a config file to the test

  private def createTestConfigFile(): File = {
    val configFile = new File("AuthenticationConfigTest.configFile.tmp")
    val writer = new FileWriter(configFile, false)
    writer.write(testConfigFileContent)
    writer.close()

    configFile
  }

  private def deleteTestConfigFile() {
    if(testConfigFile.exists())
      if(!testConfigFile.delete()) //Remove temp file used during the test
        throw new IllegalAccessException("Could not delete the temp file %s used during the test.".format(testConfigFile.getCanonicalPath))
  }

}