package org.dbpedia.spotlight.web.rest.resources

import javax.ws.rs.core.Response
import javax.ws.rs.WebApplicationException

/**
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object Authentication {

  //TODO definitive professional authentication
  def authenticate(key: String): Boolean = {
    /*if(registeredClients.contains(clientIp))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED) */
    if (!(key == "2013_Helping_Spotlight_improvement_with_feedback_for_that_I_have_the_key"))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)

    true
  }

}