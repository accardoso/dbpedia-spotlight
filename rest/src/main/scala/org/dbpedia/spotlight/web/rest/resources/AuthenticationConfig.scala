package org.dbpedia.spotlight.web.rest.resources

import java.io.{FileInputStream, File}
import java.util.Properties
import org.dbpedia.spotlight.exceptions.ConfigurationException

object AuthenticationConfig {

  private var configFile: File = null
  private var properties : Properties = null

  def setConfigFile(configFile: File) {
    if(validate(configFile))
      this.configFile = configFile

    properties = new Properties()
    properties.load(new FileInputStream(configFile))
  }
  def setConfigFile(configFilePath: String) { setConfigFile(new File(configFilePath)) }

  private def validate(file: File):Boolean = {
    if(!file.exists() || !file.isFile)
      throw new NoSuchElementException("Could not find the configuration file: %s".format(configFile.getCanonicalPath))

    true
  }

  def get(key : String, defaultValue : String) : String = {
    properties.getProperty(key, defaultValue)
  }

  def get(key : String) : String = {
    val value = get(key, null)
    if(value == null) {
      throw new ConfigurationException(key+" not specified in "+configFile.getCanonicalPath)
    }
    value
  }
}