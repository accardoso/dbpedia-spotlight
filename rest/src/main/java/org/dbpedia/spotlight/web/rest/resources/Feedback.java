/*
 * Copyright 2011 DBpedia Spotlight Development Team
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *  Check our project website for information on how to acknowledge the authors and how to contribute to the project: http://spotlight.dbpedia.org
 */

package org.dbpedia.spotlight.web.rest.resources;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.dbpedia.spotlight.exceptions.InputException;
import org.dbpedia.spotlight.io.CSVFeedbackStore;
import org.dbpedia.spotlight.io.FeedbackStore;
import org.dbpedia.spotlight.model.*;
import org.dbpedia.spotlight.web.rest.Server;
import org.dbpedia.spotlight.web.rest.ServerUtils;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * REST Web Service for feedback at http://<rest_url_setted_in_Server.java>/feedback //Default: http://localhost:2222/rest/feedback
 * Send the feed back by a GET using file request is obligatory, e.g.: curl -X POST -d/home/alexandre/Projects/feedbackCorrect.xml http://localhost:2222/rest/feedback
 *
 *
 * TODO bulk feedback: users can post a gzip with json or xml encoded feedback
 *
 * @author pablomendes (original implementation)
 * @author Alexandre Cançado Cardoso - accardoso
 */

@ApplicationPath(Server.APPLICATION_PATH)
@Path("/feedback")
@Consumes("text/plain")
public class Feedback {

    Log LOG = LogFactory.getLog(this.getClass());

    @Context
    private UriInfo context;

    // Feedback interface
    //private static SpotlightInterface feedbackInterface = new SpotlightInterface("/feedback");

    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces({MediaType.TEXT_XML,MediaType.APPLICATION_XML})
    public Response postXML(@DefaultValue("") @FormParam("key") String key,
                           @DefaultValue("") @FormParam("text") String text,
                           @DefaultValue("") @FormParam("url") String docUrlString,
                           @DefaultValue("") @FormParam("entity_uri") String entityUri,
                           @DefaultValue("") @FormParam("surface_form") String surfaceForm,
                           @DefaultValue("0") @FormParam("offset") int offset,
                           @DefaultValue("") @FormParam("feedback") String feedback,
                           @DefaultValue("") @FormParam("systems") String systemIds,
                           @Context HttpServletRequest request) throws Exception {

        String clientIp = request.getRemoteAddr();

        try {
            if(!key.equals("2013_Helping_Spotlight_improvement_with_feedback"))
                throw new WebApplicationException(Response.Status.UNAUTHORIZED);

            if (text.equals(""))
                throw new InputException("&text must be filled!");

            String response = "";
            String[] systems = systemIds.split(" ");

            URL docUrl = null;
            try {
                docUrl = new URL(docUrlString);
            } catch (MalformedURLException e) {
                if(!docUrlString.equals("")){
                    throw new InputException("The informed &url is not a valid one.");
                }
            }

            if (docUrl==null)
                docUrl = new URL("http://spotlight.dbpedia.org/id/"+text.hashCode());

            FeedbackStore output = new CSVFeedbackStore(System.out);
            output.add(docUrl,new Text(text),new DBpediaResource(entityUri),new SurfaceForm(surfaceForm),offset,feedback,systems);

            response = "ok";
            return ServerUtils.ok(response);

        } catch (Exception e) {
            e.printStackTrace();
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST). entity(ServerUtils.print(e)).type(MediaType.TEXT_HTML).build());
        }
    }

}
