package org.dbpedia.spotlight.io.feedback

import org.dbpedia.spotlight.model.{SpotlightFeedback, DBpediaResourceOccurrence}

object OccurrencesSource {

  def extractOccs(src: FeedbackLoader):List[(String, List[DBpediaResourceOccurrence])] = {
    var occs: List[FeedbackOccurrences] = List()
    SpotlightFeedback.getAllFeedbackPossibilities().foreach{ possibility =>
      occs = occs :+ new FeedbackOccurrences(possibility, List())
    }

    val feedbackList: List[SpotlightFeedback] =  src.load()
    var index: Int = -1
    feedbackList.foreach{ feedback =>
      index = occs.indexWhere(_.getFeedback() == feedback.getFeedback())
      occs(index).addOcc(feedback.toDBpediaResourceOccurrence())
    }

    var output:List[(String, List[DBpediaResourceOccurrence])] = List()
    occs.foreach{ occ =>
      output = output :+ occ.toTuple()
    }

    output
  }

  private class FeedbackOccurrences(feedback: String, var occs: List[DBpediaResourceOccurrence]){
    def addOcc(occ: DBpediaResourceOccurrence) = occs = occs :+ occ
    def toTuple() = (feedback, occs)
    def getFeedback() = feedback
  }

}