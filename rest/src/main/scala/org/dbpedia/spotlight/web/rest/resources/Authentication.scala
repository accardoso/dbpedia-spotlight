package org.dbpedia.spotlight.web.rest.resources

import javax.ws.rs.core.Response
import javax.ws.rs.WebApplicationException

/**
 * Authentication class for Spotlight server.
 * Verifies if the informed api key is a valid one. If not a valid one, throws an exception informing that it was unauthorized.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */

object Authentication {

  //TODO implement a not hardcoded key validation/generation
  def authenticate(key: String): Boolean = {
    if (!(key == "2013_Helping_Spotlight_improvement_with_feedback_for_that_I_have_the_key"))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)

    true
  }

  def authenticate(clientIp: String, key: String): Boolean = {
    /*if(registeredClients.contains(clientIp))
      throw new WebApplicationException(Response.Status.UNAUTHORIZED)*/
    authenticate(key)
  }

}
