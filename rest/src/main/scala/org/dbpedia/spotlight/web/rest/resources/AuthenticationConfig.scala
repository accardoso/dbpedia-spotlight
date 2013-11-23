package org.dbpedia.spotlight.web.rest.resources

import java.io.{FileInputStream, File}
import java.util.Properties
import org.dbpedia.spotlight.exceptions.ConfigurationException

/**
 * This object implements a generic access for the configuration file.
 * Allow to set a unique configuration file and get the value of any key.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */
object AuthenticationConfig {

  val defaultConfigFilePath: String = "conf/server.properties"

  private var configFile: File = new File(defaultConfigFilePath)
  private var properties : Properties = resetProperties()

  private def resetProperties(): Properties = {
    properties = new Properties()
    properties.load(new FileInputStream(configFile))
    properties
  }

  /* Set the configuration file */
  def setConfigFile(configFile: File) {
    //If not a valid file, validate(..) shall throw an exception
    validate(configFile)
    //Set configFile with the new one
    this.configFile = configFile
    //Reset properties to use the new configFile
    resetProperties()
  }
  /* Set the configuration file from its path */
  def setConfigFile(configFilePath: String) { setConfigFile(new File(configFilePath)) }

  /* If not valid throw an exception */
  private def validate(file: File) {
    if(!file.exists() || !file.isFile)
      throw new NoSuchElementException("Could not find the configuration file: %s".format(configFile.getCanonicalPath))
  }

  /* Get the value of a key in the config file, or use de default value */
  def get(key : String, defaultValue : String) : String = {
    //Verify if the configFile still valid
    validate(configFile)
    //Get the value of the key or use the default value
    properties.getProperty(key, defaultValue)
  }

  /* Get the value of a key in the config file */
  def get(key : String) : String = {
    //Get the value setting the default as null
    val value = get(key, null)
    //If the key not found or has no value, then throw an exception
    if(value == null)
      throw new ConfigurationException(key + " not specified in " + configFile.getCanonicalPath)
    //If exists return it
    value
  }

  def getConfigFilePath():String = configFile.getCanonicalPath
}