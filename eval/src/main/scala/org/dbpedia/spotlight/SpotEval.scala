import java.io.{PrintStream, FileNotFoundException, File}
import java.net.URLEncoder
import org.dbpedia.spotlight.corpus.MilneWittenCorpus
import org.dbpedia.spotlight.io.AnnotatedTextSource
import org.dbpedia.spotlight.log.SpotlightLog
import org.dbpedia.spotlight.model._
import org.dbpedia.spotlight.spot.SpotXmlParser
import org.xml.sax.SAXParseException
import scala.io.Source
import sys.process._
import scala.collection.JavaConverters._

object SpotEval{

  val justOffset: Boolean = false

  val spotlightServer: String = "http://spotlight.dbpedia.org/rest/"

  val spotInterface: String = (spotlightServer + (if(spotlightServer.endsWith("/")) "spot?text=" else "/spot?text=")).trim

  def evaluate(src: AnnotatedTextSource) = {
    SpotlightLog

    var totRetrieved = 0
    var totRelevant = 0
    var totTP: Float = 0

    var countParagraphs = 0
    var avgPrecision: Float = 0
    var avgRecall: Float = 0

    val parser: SpotXmlParser = new SpotXmlParser()
    val tmpFile: File = new File("AnnotatedTextSourceEval.tmp")

    src.foreach{ paragraph =>
      val curlcmd: String = "curl -o "+tmpFile.getCanonicalPath+" "+spotInterface+URLEncoder.encode(paragraph.text.text, "UTF-8")
      curlcmd.!

      var ans:List[SpotOccurrence] = List()
      try{
        ans = convertFromSurfaceFormOccurrence(parser.extract(new Text(Source.fromFile(tmpFile).getLines().mkString(""))).asScala.toList).sorted

        val expected:List[SpotOccurrence] = convertFromDBpediaResourceOccurrence(paragraph.occurrences).sorted

        /* //debug
        var debug = new File("ans.debug")
        var stream = new PrintStream(debug)
        ans.foreach(stream.println(_))
        stream.close()
        if(debug.exists())
          if(!debug.delete())
            SpotlightLog.warn(this.getClass, "Could not delete the debug file: %s", debug.getAbsolutePath)

        debug = new File("expected.debug")
        stream = new PrintStream(debug)
        expected.foreach(stream.println(_))
        stream.close()
        if(debug.exists())
          if(!debug.delete())
            SpotlightLog.warn(this.getClass, "Could not delete the debug file: %s", debug.getAbsolutePath)
        */

        var tp: Float = 0

        var i: Int = 0
        var a: SpotOccurrence = ans(i)
        expected.foreach{ e=>
          while(e.getOffset() > a.getOffset()){
            i += 1
            a = ans(i)
          }
          if(e.getOffset() == a.getOffset())
            if(!justOffset && e.getSurfaceForm().equals(a.getSurfaceForm())){
              tp += 1
              /*println(a) //debug*/
            }
        }

        val precision = if(ans.length != 0) tp / ans.length else -1
        val recall = if(expected.length != 0) tp / expected.length else -1
        var f1 = precision+recall
        f1 = if(f1 > 0 && precision >= 0 && recall >= 0) 2*precision*recall / (precision+recall) else -1

        totRetrieved += ans.length
        totRelevant += expected.length
        totTP += tp

        countParagraphs += 1
        avgPrecision += precision
        avgRecall += recall

        SpotlightLog.info(this.getClass, "Paragraph "+countParagraphs+" Measures:\nTP = "+tp+"\nPrecision = "+precision+
          "\nRecall = "+recall+"\nF1-score = "+f1)

      }catch{
        case e: FileNotFoundException => {
          SpotlightLog.warn(this.getClass, "Discard paragraph %d. Invalid answer to the cURL request: %s" , countParagraphs, spotInterface+paragraph.text.text)
        }
        case e: SAXParseException => {
          SpotlightLog.warn(this.getClass, "Could not parse the Spotter result:\n%s" , Source.fromFile(tmpFile).getLines().mkString("\n"))
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

      SpotlightLog.info(this.getClass, "Whole Corpus Measures:\nTP = "+totTP+"\nTot Precision = "+totPrecision+
        "\nTot Recall = "+totRecall+"\nF1-score from tot metrics= "+f1OfTotMeasures)

      avgPrecision /= countParagraphs
      avgRecall /= countParagraphs
      var f1OfAvgMeasures = avgPrecision+avgRecall
      f1OfAvgMeasures = if(f1OfAvgMeasures > 0) 2*avgPrecision*avgRecall / (avgPrecision+avgRecall) else -1

      SpotlightLog.info(this.getClass, "Average of Paragraphs Measures:\nNum of paragraphs = "+countParagraphs+
        "\nAvg Precision = "+avgPrecision+"\nAvg Recall = "+avgRecall+"\nF1-score from avg metrics = "+f1OfAvgMeasures)
    }

    if(tmpFile.exists())
      if(!tmpFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary file: %s", tmpFile.getAbsolutePath)
  }

  def convertFromDBpediaResourceOccurrence(list: List[DBpediaResourceOccurrence]): List[SpotOccurrence] = {
    var out: List[SpotOccurrence] = List()

    list.foreach { e =>
      out = out :+ new SpotOccurrence(e)
    }

    out
  }
  def convertFromSurfaceFormOccurrence(list: List[SurfaceFormOccurrence]): List[SpotOccurrence] = {
    var out: List[SpotOccurrence] = List()

    list.foreach { e =>
      out = out :+ new SpotOccurrence(e)
    }

    out
  }

  def main(args: Array[String]){
    //val mewDir: String = "/home/alexandre/Desktop/mock-MilneWitten"
    val mewDir: String = "/home/alexandre/intrinsic/corpus/MilneWitten-wikifiedStories"

    evaluate(MilneWittenCorpus.fromDirectory(new File(mewDir)))
  }

}

class SpotOccurrence(offset : Int, surfaceForm : SurfaceForm) extends Comparable[SpotOccurrence]{
  def this(element: DBpediaResourceOccurrence) = this(element.textOffset, element.surfaceForm)
  def this(element: SurfaceFormOccurrence) = this(element.textOffset, element.surfaceForm)

  def compareTo(o: SpotOccurrence): Int = this.getOffset().compareTo(o.getOffset())

  def getOffset(): Int = offset
  def getSurfaceForm(): SurfaceForm = surfaceForm

  override def toString: String = "SpotOccurrence[%d | %s]".format(offset, surfaceForm)
}