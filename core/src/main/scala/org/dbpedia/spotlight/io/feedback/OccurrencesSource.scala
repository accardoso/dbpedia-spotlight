package org.dbpedia.spotlight.io.feedback

import org.dbpedia.spotlight.model.{SpotlightFeedback, DBpediaResourceOccurrence}

object OccurrencesSource {

  def extractOccs(src: FeedbackLoader):List[Tuple3[String, Boolean, List[DBpediaResourceOccurrence]]] = {
    var annotated: List[FeedbackOccurrences] = List()
    var suggested: List[FeedbackOccurrences] = List()
    SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
      annotated = annotated :+ new FeedbackOccurrences(possibility, false, List())
    }
    SpotlightFeedback.getFeedbackPossibilitiesThatAcceptSuggestion().foreach{ possibility =>
      suggested = suggested :+ new FeedbackOccurrences(possibility, true, List())
    }

    val feedbackList: List[SpotlightFeedback] =  src.load()
    feedbackList.foreach{ feedback =>
      var index: Int = annotated.indexWhere(_.getFeedback() == feedback.getFeedback())
      annotated(index).addOcc(feedback.toDBpediaResourceOccurrence())
      if(SpotlightFeedback.getFeedbackPossibilitiesThatAcceptSuggestion().contains(feedback.getFeedback())){
        if(feedback.getEntitySuggestion() != null){
          index = suggested.indexWhere(_.getFeedback() == feedback.getFeedback())
          suggested(index).addOcc(feedback.toSuggestedDBpediaResourceOccurrence())
        }
      }
    }

    var output:List[Tuple3[String, Boolean, List[DBpediaResourceOccurrence]]] = List()
    annotated.foreach{ occ =>
      output = output :+ occ.toTuple()
    }
    suggested.foreach{ occ =>
      output = output :+ occ.toTuple()
    }

    output
  }

  private class FeedbackOccurrences(feedback: String, isSuggestion: Boolean, var occs: List[DBpediaResourceOccurrence]){
    def addOcc(occ: DBpediaResourceOccurrence) = {
      occs = occs :+ occ
    }

    def toTuple() = (feedback, isSuggestion ,occs)

    def getFeedback() = feedback
  }

}