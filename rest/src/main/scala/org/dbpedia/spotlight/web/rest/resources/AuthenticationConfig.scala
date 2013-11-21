import java.io.{FileInputStream, File}
import java.util.Properties
import org.dbpedia.spotlight.exceptions.ConfigurationException

class AuthenticationConfig(configFile: File) {
  //Constructor receiving the path of the configuration file
  def this(configFilePath: String) = this(new File(configFilePath))
  //Constructor using the default path and default name for the configuration file (if this file do not exists it shall not work)
  def this() = this("conf" + File.separator + "server.properties")

  if(!configFile.exists() || !configFile.isFile)
    throw new NoSuchElementException("Could not find the configuration file: %s".format(configFile.getCanonicalPath))

  private val properties : Properties = new Properties()
  //SpotlightLog.info(this.getClass, "Loading configuration file %s", configFile)
  properties.load(new FileInputStream(configFile))

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