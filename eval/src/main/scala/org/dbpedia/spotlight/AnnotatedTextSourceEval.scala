import java.io.{FileNotFoundException, File}
import org.dbpedia.spotlight.corpus.MilneWittenCorpus
import org.dbpedia.spotlight.io.AnnotatedTextSource
import org.dbpedia.spotlight.log.SpotlightLog
import org.dbpedia.spotlight.model._
import org.dbpedia.spotlight.spot.SpotXmlParser
import org.xml.sax.SAXParseException
import scala.io.Source
import sys.process._
import scala.collection.JavaConverters._

object AnnotatedTextSourceEval{

  val spotlightServer: String = "http://spotlight.dbpedia.org/rest/"

  val spotInterface: String = spotlightServer + (if(spotlightServer.endsWith("/")) "spot?text=" else "/spot?text=")

  def evaluate(src: AnnotatedTextSource) = {
    var totRetrieved = 0
    var totRelevant = 0
    var totTP = 0

    var countParagraphs = 0
    var avgPrecision = 0
    var avgRecall = 0

    val parser: SpotXmlParser = new SpotXmlParser()
    val tmpFile: File = new File("AnnotatedTextSourceEval.tmp")

    src.foreach{ paragraph =>
      ("curl \"" + spotInterface + paragraph.text.text + "\" > " + tmpFile.getAbsolutePath).!


      var ans:List[SpotOccurrence] = List()
      try{
        ans = convertFrom(parser.extract(new Text(Source.fromFile(tmpFile).getLines().mkString(""))).asScala.toList).sorted

        val expected:List[SpotOccurrence] = convertFrom(paragraph.occurrences).sorted
        var tp: Int = 0

        var i: Int = 0
        var a: SpotOccurrence = ans(i)
        expected.foreach{ e=>
          while(e.getOffset() < a.getOffset()){
            a = ans(i)
            i += 1
          }
          if(e.getOffset() == a.getOffset())
            tp += 1
        }

        val precision = if(ans.length != 0) tp / ans.length else -1
        val recall = if(expected.length != 0) tp / expected.length else -1
        var f1 = precision+recall
        f1 = if(f1 > 0 && precision >= 0 && recall >= 0) 2*precision*recall / (precision+recall) else -1

        SpotlightLog.info(this.getClass, "Paragraph %d Measures:\nPrecision = %f\nRecall = %f\nF1-score = %f",
          precision, recall, f1)

        totRetrieved += ans.length
        totRelevant += expected.length
        totTP += tp

        countParagraphs += 1
        avgPrecision += precision
        avgRecall += recall

      }catch{
        case e: FileNotFoundException => {
          SpotlightLog.warn(this.getClass, "Invalid answer to the cURL request: %s" , spotInterface+paragraph.text.text)
        }
        case e: SAXParseException => {
          SpotlightLog.warn(this.getClass, "Could not parse the Spoter result: %s" , Source.fromFile(tmpFile).getLines().mkString("\n"))
        }
      }

    }

    if(countParagraphs == 0)
      SpotlightLog.info(this.getClass, "The informed source has no paragraphs to be evaluated.")
    else{
      val totPrecision = if(totRetrieved != 0) totTP / totRetrieved else -1
      val totRecall = if(totRelevant != 0) totTP / totRelevant else -1
      var f1OfTotMeasures = totPrecision+totRecall
      f1OfTotMeasures = if(f1OfTotMeasures > 0 && totPrecision >= 0 && totRecall >= 0)
        2*totPrecision*totRecall / (totPrecision+totRecall) else -1

      SpotlightLog.info(this.getClass, "Whole Corpus Measures:\nPrecision = %f\nRecall = %f\nF1-score = %f",
        totPrecision, totRecall, f1OfTotMeasures)

      avgPrecision /= countParagraphs
      avgRecall /= countParagraphs
      var f1OfAvgMeasures = avgPrecision+avgRecall
      f1OfAvgMeasures = if(f1OfAvgMeasures > 0) 2*avgPrecision*avgRecall / (avgPrecision+avgRecall) else -1

      SpotlightLog.info(this.getClass, "Average of Paragraphs Measures:\nPrecision = %f\nRecall = %f\nF1-score = %f",
        avgPrecision, avgRecall, f1OfAvgMeasures)
    }

    if(tmpFile.exists())
      if(!tmpFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary file: %s", tmpFile.getAbsolutePath)
  }

  def convertFrom(list: List[Any]): List[SpotOccurrence] = {
    var out: List[SpotOccurrence] = List()

    list match {
      case l: List[DBpediaResourceOccurrence] => {
        l.foreach { e =>
          out = out :+ new SpotOccurrence(e)
        }
      }
      case l: List[SurfaceFormOccurrence] => {
        l.foreach { e =>
          out = out :+ new SpotOccurrence(e)
        }
      }
      case _ => throw new IllegalArgumentException("Invalid type of the elements of argument: list. It must be a list of DBpediaResourceOccurrence OR SurfaceFormOccurrence.")
    }

    out
  }


  def main(args: Array[String]){
    evaluate(MilneWittenCorpus.fromDirectory(new File("/home/alexandre/Desktop/mock-MilneWitten")))
  }

}

class SpotOccurrence(offset : Int, surfaceForm : SurfaceForm) extends Comparable[SpotOccurrence]{
  def this(element: DBpediaResourceOccurrence) = this(element.textOffset, element.surfaceForm)
  def this(element: SurfaceFormOccurrence) = this(element.textOffset, element.surfaceForm)

  def compareTo(o: SpotOccurrence): Int = this.getOffset().compareTo(o.getOffset())

  def getOffset(): Int = offset
  def getSurfaceForm(): SurfaceForm = surfaceForm
}