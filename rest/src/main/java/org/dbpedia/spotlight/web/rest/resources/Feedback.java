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
import org.dbpedia.spotlight.io.feedback.LuceneFeedbackStore;
import org.dbpedia.spotlight.web.rest.Server;
import org.dbpedia.spotlight.web.rest.ServerUtils;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;
import org.dbpedia.spotlight.io.feedback.FeedbackMultiStore;
import org.dbpedia.spotlight.model.SpotlightFeedback;
import org.dbpedia.spotlight.io.feedback.TSVFeedbackStore;
import java.io.File;

/**
 * REST Web Service for feedback at http://<rest_url_setted_in_Server.java>/feedback //Default: http://localhost:2222/rest/feedback/
 * Send the feed back by a GET using file request is obligatory, e.g.: curl -X POST -d @/home/alexandre/Projects/feedbackIncorrect http://localhost:2222/rest/feedback/
 *
 * @author pablomendes
 * @author Alexandre Cançado Cardoso - accardoso
 */
//TODO bulk feedback: users can post a gzip with json or xml encoded feedback
//TODO accept HTTP GET

@ApplicationPath(Server.APPLICATION_PATH)
@Path("/feedback")
@Consumes("text/plain")
public class Feedback {

    Log LOG = LogFactory.getLog(this.getClass());

    @Context
    private UriInfo context;

    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces({MediaType.TEXT_XML,MediaType.APPLICATION_XML})
    public Response postXML(@DefaultValue("") @FormParam("key") String key,
                           @DefaultValue("") @FormParam("text") String text,
                           @DefaultValue("") @FormParam("url") String docUrlString,                          //Optional, auto-generated if not informed.
                           @DefaultValue("") @FormParam("discourse_type") String discourseType,              //Optional
                           @DefaultValue("") @FormParam("entity_uri") String entityUri,
                           @DefaultValue("") @FormParam("surface_form") String surfaceForm,
                           @DefaultValue("0") @FormParam("offset") int offset,
                           @DefaultValue("") @FormParam("feedback") String feedback,
                           @DefaultValue("") @FormParam("systems") String systemIds,
                           @DefaultValue("") @FormParam("is_manual_feedback") boolean isManualFeedback,
                           @DefaultValue("") @FormParam("language") String language,                         //Optional
                           @Context HttpServletRequest request) throws Exception {

        try {
            String clientIp = request.getRemoteAddr();

            //Request authentication (throws an exception if not a valid one)
            Authentication.authenticate(clientIp, key);

            //Validate and Standardize the feedback, creating a standard feedback
            SpotlightFeedback spotlightFeedback =  new SpotlightFeedback(text, docUrlString, discourseType, entityUri, surfaceForm, offset, feedback, systemIds, isManualFeedback, language);

            //Create a folder to keep all storage files
            String storageFolderPath = FeedbackMultiStore.createStorageFolder("feedback-warehouse");
            //Create a manager for multiples stores
            FeedbackMultiStore multiStore = new FeedbackMultiStore(); //Create the empty Multi-Store
            //Register the stores to the manager
            multiStore.registerStore(new TSVFeedbackStore(storageFolderPath)); //Create and register a store that output to a auto-created and default named .tsv file
            multiStore.registerStore(new LuceneFeedbackStore(storageFolderPath + File.separator + "feedbackStore.luceneIndex"));

            //Store the feedback into all stores registered at multiStore
            multiStore.storeFeedback(spotlightFeedback);

            //Close all stores that need to be closed
            multiStore.close();

            //Answer that the feedback was stored right. (If not: an exception was threw before this point)
            return ServerUtils.ok("ok");
        } catch (Exception e) {
            e.printStackTrace();
            throw new WebApplicationException(Response.status(Response.Status.BAD_REQUEST). entity(ServerUtils.print(e)).type(MediaType.TEXT_HTML).build());
       }
    }

}
