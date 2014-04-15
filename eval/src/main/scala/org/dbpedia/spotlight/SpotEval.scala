import java.io.{PrintStream, FileNotFoundException, File}
import java.net.URLEncoder
import org.dbpedia.spotlight.corpus.{AidaCorpus, CSAWCorpus, MilneWittenCorpus}
import org.dbpedia.spotlight.io.AnnotatedTextSource
import org.dbpedia.spotlight.log.SpotlightLog
import org.dbpedia.spotlight.model._
import org.dbpedia.spotlight.spot.SpotXmlParser
import org.xml.sax.SAXParseException
import scala.io.Source
import sys.process._
import scala.collection.JavaConverters._

/**
 * Evaluation class for any Spotter at the /spot interface. Perform a evaluation which call the /spot for the text of a
 * AnnotatedTextSource using the informed spotter and save the evaluation result to a tsv output file with n lines. Or
 * a batch evaluation of the same source for a list of spotters, saving the outputFile into the informed directory.
 *
 * Where the first (n-2) lines are the evaluation for each paragraph of the AnnotatedTextSource alone; the (n-1)th line is
 * the evaluation considering all paragraphs by the same time; and the line n (the last line) is the average metrics for
 * the (n-2) first lines.
 *
 * And the fields are:
 * Evaluated_text(paragraph)_ID \t #True_Positives \t #Retrieved \t #Relevant \t Precision \t Recall \t F1
 *
 * Eg.:
 * P#1	7	59	16	0.118644066	0.4375	0.18666667
 * (...)
 * P#(n-2)	8	54	15	0.14814815	0.53333336	0.23188406
 * All	15	113	31	0.13274336	0.48387095	0.20833333
 * Avg	7	56	15	0.1333961	0.48541668	0.20927536
 *
 * Note: When the server's Spotlight could not spot the paragraph or it's answer could not be parsed the paragraph is
 * discard (and an warning is show). And the whole source metrics will not considerate this too. If all paragraph are
 * invalid the output file will be empty.
 *
 * @param spotlightServer: String - The Spotlight service URL. Default: "http://spotlight.dbpedia.org/rest/"
 * @param justOffset: Boolean - If True considerate true positive when the offset is the same, even with the distinct
 *                              surface forms.
 *                                Eg.: SpotEvalOccurence[10 | "brain"] SpotEvalOccurence[10 | "brain cancer"] will be
 *                              considerate as a true positive.
 *                                     Note: At the eg. the SpotEvalOccurence[16 | "cancer"] continue to be NOT considerate
 *                              as a true positive.
 *                                Important: The usual is use both offset and surface form equality. So, justOffset = false.
 *                              Default: false
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */
class SpotEval(var spotlightServer: String, val justOffset: Boolean){
  def this() = this("http://spotlight.dbpedia.org/rest/", false) //Constructor with default parameters
  def this(spotlightServer: String) = this(spotlightServer, false) //Constructor for usual evaluation, i.e. using both offset and surface form equality.

  /* Treat the informed parameters */
  if(!spotlightServer.endsWith("/")){
    spotlightServer = spotlightServer + "/"
  }

  /* Evaluate the /spot interface result using the spotter for the Corpus on src and save it into the output tsv file*/
  def evaluate(src: AnnotatedTextSource, spotter: String, outputFileName: String) = {
    SpotlightLog.info(this.getClass, "Evaluation begin with configuration:" +
      "\nCorpus: %s".format(src.name)+
      "\nServer: %s".format(spotlightServer)+
      "\nSpotter: %s".format(spotter)+
      "\nEquality Policy: %s".format(if(justOffset) "Just Offset" else "Usual (Offset & Surface Form)"))

    /* Define the spotlight server /spot interface url for the informed spotter */
    var spotterParam: String = spotter
    if(spotter.equals("")){
        spotterParam = "Default"
    }
    //val spotInterface: String = spotlightServer + "spot?"+"spotter="+spotterParam+"&"+"text="
    val spotInterface: String = spotlightServer + "spot/"

    /* Metrics var/val */
    var totRetrieved = 0
    var totRelevant = 0
    var totTP: Float = 0
    var countParagraphs = 0
    var avgTP: Float = 0
    var avgPrecision: Float = 0
    var avgRecall: Float = 0
    var avgF1: Float = 0

    /* Discarded paragraphs counters */
    var invalidAnsToHttpRequest = 0
    var couldNotParseAns = 0

    /* Requests and comparisons */
    val parser: SpotXmlParser = new SpotXmlParser()    
    val tmpStdOutFile: File = new File("SpotEval-curl-"+System.currentTimeMillis()+".stdout.tmp")
    val tmpStdErrFile: File = new File("SpotEval-curl-"+System.currentTimeMillis()+".stderr.tmp")
    val tmpPostDataFile: File = new File("SpotEval-curl-"+System.currentTimeMillis()+".post-data.tmp")
    val outputStream: PrintStream = new PrintStream(outputFileName)
    src.foreach{ paragraph =>
      /* Request the server's Spotlight */
      val postDataStream: PrintStream = new PrintStream(tmpPostDataFile)
      postDataStream.print("?spotter="+spotterParam+"&text=")
      postDataStream.print(URLEncoder.encode(paragraph.text.text, "UTF-8"))
      val curlcmd: String = "curl -d @"+tmpPostDataFile.getCanonicalPath+" -o "+tmpStdOutFile.getCanonicalPath+" --stderr "+tmpStdErrFile+" "+spotInterface
      //val curlcmd: String = "curl -o "+tmpStdOutFile.getCanonicalPath+" --stderr "+tmpStdErrFile+" "+spotInterface+URLEncoder.encode(paragraph.text.text, "UTF-8")
      curlcmd.!

      var ans:List[SpotEvalOccurrence] = List()
      try{
        /* Read the answer as List of SpotEvalOccurrence and sort it*/
        try{
          ans = SpotEvalOccurrence.convertFromSurfaceFormOccurrence(parser.extract(new Text(Source.fromFile(tmpStdOutFile).getLines().mkString(""))).asScala.toList).sorted
        } catch {
          case e: SAXParseException => {
            val escaped = "<annotation text=\"Escaped by SpotEval.evaluate\">"+Source.fromFile(tmpStdOutFile).getLines().drop(1).mkString("")
            ans = SpotEvalOccurrence.convertFromSurfaceFormOccurrence(parser.extract(new Text(escaped)).asScala.toList).sorted
          }
        }

        /* Read the paragraph expected annotations as List of SpotEvalOccurrence and sort it*/
        val expected:List[SpotEvalOccurrence] = SpotEvalOccurrence.convertFromDBpediaResourceOccurrence(paragraph.occurrences).sorted

        /* Compare the ans and expected list */
        var tp: Float = 0 // True positives of the current paragraph
        var i: Int = 0 //Index of the ans list current element
        try{
          expected.foreach{ e=>
            while(e.getOffset() > ans(i).getOffset()){
              i += 1
            }
            if(e.getOffset() == ans(i).getOffset())
              if(justOffset || e.getSurfaceForm().equals(ans(i).getSurfaceForm())){
                tp += 1
                i += 1
              }
          }
        }catch {
          /* If the IndexOutOfBoundsException it means that the i is bigger than the positions of ans list, so it has
          ended and no more tp incrementation is posible, i.e. the comparations for this paragraph has terminated. */
          case e: IndexOutOfBoundsException => if(e.getMessage.toInt != ans.length) throw e //else The List ans is terminate, so also the true positives (tp)
        }

        /* Calculate the paragraph's metrics */
        val precision = if(ans.length != 0) tp / ans.length else -1
        val recall = if(expected.length != 0) tp / expected.length else -1
        var f1 = precision+recall
        f1 = if(f1 > 0 && precision >= 0 && recall >= 0) 2*precision*recall / (f1) else -1

        /* Increment the metric of the whole text (All valid paragraphs) and the average metrics*/
        totRetrieved += ans.length
        totRelevant += expected.length
        totTP += tp
        countParagraphs += 1
        avgTP += tp
        var precisionStr: String = "NaN"
        if(precision >= 0){
          avgPrecision += precision
          precisionStr = precision.toString
        }
        var recallStr: String = "NaN"
        if(recall >= 0){
          avgRecall += recall
          recallStr = recall.toString
        }
        var f1Str: String = "NaN"
        if(f1 >= 0){
          avgF1 += f1
          f1Str = f1.toString
        }

        /* Save the Paragraph's metrics  */
        outputStream.println("P#"+countParagraphs+"\t"+tp.toInt+"\t"+ans.length+"\t"+expected.length+"\t"+precisionStr+"\t"+recallStr+"\t"+f1Str)

      }catch{/* Invalid paragraphs are discard */
        case e: FileNotFoundException => {
          //SpotlightLog.error(this.getClass, "Invalid answer to the cURL request: %s" , spotInterface+paragraph.text.text)
          SpotlightLog.error(this.getClass, "No answer to the request with the Text:\n%s" , paragraph.text.text)
          invalidAnsToHttpRequest += 1
        }
        case e: SAXParseException => {
          SpotlightLog.error(this.getClass, "Could not parse the Spotlight answer:\n%s" , Source.fromFile(tmpStdOutFile).getLines().mkString("\n"))
          SpotlightLog.error(this.getClass, "Parser error: %s", e.getMessage)
          couldNotParseAns += 1
        }
      }

    }
    /* If no paragraph is valid inform it and do not make sense calculate the metrics */
    if(countParagraphs == 0)
      SpotlightLog.warn(this.getClass, "The informed source has no paragraphs to be evaluated.")
    else{ //If at least one paragraph is valid calculate the metrics for the whole text (all valid paragraphs) and the average ones
      /* Calculate the Whole Text (All valid paragraph)'s metrics */
      val totPrecision = if(totRetrieved != 0) totTP / totRetrieved else -1
      val totRecall = if(totRelevant != 0) totTP / totRelevant else -1
      var f1OfTotMeasures = totPrecision+totRecall
      f1OfTotMeasures = if(f1OfTotMeasures > 0 && totPrecision >= 0 && totRecall >= 0)
        2*totPrecision*totRecall / (totPrecision+totRecall) else -1

      /* Save the Whole Text (All valid paragraph)'s metrics  */
      var totPrecisionStr: String = "NaN"
      if(totPrecision >= 0)
        totPrecisionStr = totPrecision.toString
      var totRecallStr: String = "NaN"
      if(totRecall >= 0)
        totRecallStr = totRecall.toString
      var f1OfTotMeasuresStr: String = "NaN"
      if(f1OfTotMeasures >= 0)
        f1OfTotMeasuresStr = f1OfTotMeasures.toString

      outputStream.println("All\t"+totTP.toInt+"\t"+totRetrieved+"\t"+totRelevant+"\t"+totPrecisionStr+"\t"+totRecallStr+"\t"+f1OfTotMeasuresStr)

      /* Calculate the average of the paragraph's metrics  */
      avgTP /= countParagraphs
      avgPrecision /= countParagraphs
      avgRecall /= countParagraphs
      avgF1 /= countParagraphs
      val avgRetrieved = totRetrieved/countParagraphs
      val avgRelevant = totRelevant/countParagraphs

      /* Save the average of the paragraph's metrics  */
      outputStream.println("Avg\t"+avgTP.toInt+"\t"+avgRetrieved+"\t"+avgRelevant+"\t"+avgPrecision+"\t"+avgRecall+"\t"+avgF1)
    }

    outputStream.close()

    /* Delete the temporary file */
    if(tmpStdOutFile.exists())
      if(!tmpStdOutFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary file: %s", tmpStdOutFile.getAbsolutePath)
    if(tmpStdErrFile.exists())
      if(!tmpStdErrFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary file: %s", tmpStdErrFile.getAbsolutePath)
    if(tmpPostDataFile.exists())
      if(!tmpPostDataFile.delete())
        SpotlightLog.warn(this.getClass, "Could not delete the temporary file: %s", tmpPostDataFile.getAbsolutePath)


    /* Evaluations Stats */
    SpotlightLog.info(this.getClass, ("Evaluation is finished." +
      "\nThe result is saved at: %s").format(outputFileName) +
      "\nNumber of no answer to the HTTP request: %d".format(invalidAnsToHttpRequest)+
      "\nNumber of paragraphs that the Spotlight answer could not be parsed: %d".format(couldNotParseAns))

  }

  /* Call evaluate method for the same src n times one for each spotter of the list save its output tsv file into the informed directory */
  def batchEvaluation(src: AnnotatedTextSource, spotterList: List[String], outputFolder: File) {
    if(!outputFolder.exists() || !outputFolder.isDirectory)
      throw new NoSuchElementException("Could not find output directory: "+outputFolder.getCanonicalPath)

    spotterList.foreach{ spotter =>
      val outputFileName: String = outputFolder.getCanonicalPath+File.separator+"SpotEvalResults-"+src.name+"-"+spotter+".tsv"
      try{
        evaluate(src, spotter, outputFileName)
      }catch {
        case e: Exception => {
          SpotlightLog.error(this.getClass, "The evaluate method for spotter \"%s\" threw the exception bellow: %s", spotter, e.getMessage+"\n"+e.getStackTrace.mkString("\n"))
          SpotlightLog.info(this.getClass, "Continue to the next spotter..")
        }
      }
    }
  }
}

object SpotEval{

  /* Default Spotter List with All Spotters */
  val allSpotters: List[String] = List("LingPipeSpotter", "AtLeastOneNounSelector", "CoOccurrenceBasedSelector", "NESpotter",
    "KeyphraseSpotter", "OpenNLPChunkerSpotter") //The default spotter: "Default" == "default" == "" is the first spotter listed at server's Spotlight server.properties config file, so (we assume that) it is one of the listed by allServers List.


  def main(args: Array[String]){

    val luceneEvaluator = new SpotEval()

    val mwMockDir: String = "/home/alexandre/intrinsic/corpus/mock-MilneWitten"
    luceneEvaluator.batchEvaluation(MilneWittenCorpus.fromDirectory(new File(mwMockDir)), allSpotters, new File("/home/alexandre/projects/spot-eval/spot-eval-m&w-mock"))

//    val mwDir: String = "/home/alexandre/intrinsic/corpus/MilneWitten-wikifiedStories"
//    luceneEvaluator.batchEvaluation(MilneWittenCorpus.fromDirectory(new File(mwDir)), allSpotters, new File("/home/alexandre/projects/spot-eval/spot-eval-m&w"))
//
//    val csawPreDir = "/home/alexandre/intrinsic/corpus/CSAW_crawledDocs"
//    luceneEvaluator.batchEvaluation(CSAWCorpus.fromDirectory(new File(csawPreDir)), allSpotters, new File("/home/alexandre/projects/spot-eval/spot-eval-csaw"))
//
    val aidaFile = "/home/alexandre/intrinsic/corpus/aida-yago2-dataset/aida-yago2-dataset/AIDA-YAGO2-annotations.tsv"
    luceneEvaluator.batchEvaluation(AidaCorpus.fromFile(new File(aidaFile)), allSpotters, new File("/home/alexandre/projects/spot-eval/spot-eval-aida"))

  }

}


/* The Abstract Data Type for the spot results and corpus compatibility */
class SpotEvalOccurrence(offset : Int, surfaceForm : SurfaceForm) extends Comparable[SpotEvalOccurrence]{
  def this(element: DBpediaResourceOccurrence) = this(element.textOffset, element.surfaceForm)
  def this(element: SurfaceFormOccurrence) = this(element.textOffset, element.surfaceForm)

  def compareTo(o: SpotEvalOccurrence): Int = this.getOffset().compareTo(o.getOffset())

  def getOffset(): Int = offset
  def getSurfaceForm(): SurfaceForm = surfaceForm

  override def toString: String = "SpotOccurrence[%d | %s]".format(offset, surfaceForm)
}
object SpotEvalOccurrence{
  def convertFromDBpediaResourceOccurrence(list: List[DBpediaResourceOccurrence]): List[SpotEvalOccurrence] = {
    var out: List[SpotEvalOccurrence] = List()
    list.foreach { e =>
      out = out :+ new SpotEvalOccurrence(e)
    }
    out
  }
  def convertFromSurfaceFormOccurrence(list: List[SurfaceFormOccurrence]): List[SpotEvalOccurrence] = {
    var out: List[SpotEvalOccurrence] = List()
    list.foreach { e =>
      out = out :+ new SpotEvalOccurrence(e)
    }
    out
  }
}