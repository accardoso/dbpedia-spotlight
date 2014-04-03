import java.io.File
import org.dbpedia.spotlight.io.AnnotatedTextSource
import org.dbpedia.spotlight.log.SpotlightLog
import org.dbpedia.spotlight.model.{SurfaceForm, DBpediaResourceOccurrence, SurfaceFormOccurrence, Text}
import org.dbpedia.spotlight.spot.SpotXmlParser
import scala.io.Source
import sys.process._
import scala.collection.JavaConverters._

object AnnotatedTextSourceEval{

  val spotlightServer: String = "http://spotlight.dbpedia.org/rest/"

  val spotInterface: String = spotlightServer + (if(spotlightServer.endsWith("/")) "spot?text=" else "/spot?text=")

  def evaluate(src: AnnotatedTextSource) = {

    var countParagraphs = 0
    var avgPrecision = 0; var avgRecall = 0;
    val parser: SpotXmlParser = new SpotXmlParser()
    val tmpFile: File = new File("AnnotatedTextSourceEval.tmp")

    src.foreach{ paragraph =>
      ("curl " + spotInterface +  paragraph.text.toString + " > " + tmpFile.getAbsolutePath).!
      val ans:List[SpotOccurrence] = convertFrom(parser.extract(new Text(Source.fromFile(tmpFile).getLines().mkString(""))).asScala.toList).sorted
      val expected:List[SpotOccurrence] = convertFrom(paragraph.occurrences).sorted

      var tp: Int = 0
      var fp: Int = 0
      var fn: Int = 0
      
      expected.foreach{ e=>
        while(e.textOffset)
        
      }


      val precison = tp / (tp+fp)
      val recall = tp / (tp+fn)

      countParagraphs += 1
      avgPrecision += precison
      avgRecall += recall

      //Output de cada PR

    }

    if(countParagraphs == 0)
      SpotlightLog.info(this.getClass, "The informed source has no paragraphs to be evaluated.")
    else{
      avgPrecision /= countParagraphs
      avgRecall /= countParagraphs



    }

    if(tmpFile.exists())
      if(!tmpFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temprary file: %s", tmpFile.getAbsolutePath)
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

  }

}

class SpotOccurrence(offset : Int, surfaceForm : SurfaceForm) extends Comparable[SpotOccurrence]{
  def this(element: DBpediaResourceOccurrence) = this(element.textOffset, element.surfaceForm)
  def this(element: SurfaceFormOccurrence) = this(element.textOffset, element.surfaceForm)

  def compareTo(o: SpotOccurrence): Int = this.offset.compareTo(o.offset)
  
  def getOffset
}