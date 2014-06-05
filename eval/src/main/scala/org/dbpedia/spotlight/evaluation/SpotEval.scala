package org.dbpedia.spotlight.evaluation

import java.io._
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
import scala.util.control.Breaks._
import scala.collection.mutable
import org.dbpedia.spotlight.db.memory._
import org.dbpedia.spotlight.exceptions.DBpediaResourceNotFoundException
import org.dbpedia.spotlight.db.{WikipediaToDBpediaClosure, DBCandidateSearcher}

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
class SpotEval(var spotlightServer: String, var spotter: String, val justOffset: Boolean){
  def this(spotlightServer: String, spotter: String) = this(spotlightServer, spotter, false) //Constructor for usual evaluation, i.e. using both offset and surface form equality.

  /* Treat the informed parameters */
  if(!spotlightServer.endsWith("/"))
    spotlightServer = spotlightServer + "/"
  spotlightServer += "spot/"

  if(spotter.equals(""))
    spotter = "Default"

  def spotParagraph(paragraph: AnnotatedParagraph, outputFileName: String){
    val postDataTmpFile: File = new File(outputFileName+".text-param.tmp")
    val writer: FileWriter = new FileWriter(postDataTmpFile, false)
    writer.write("text=")
    writer.write(URLEncoder.encode(paragraph.text.text, "UTF-8"))
    writer.close()

    //TODO remove Unix cURL dependency. It was almost at: org.dbpedia.spotlight.web.rest.ServerTextSizeLimitTest.scala
    val curlcmd: String = "curl -s -o "+outputFileName+" --data spotter="+spotter+" --data @"+postDataTmpFile+" "+spotlightServer
    curlcmd.!

    if(postDataTmpFile.exists())
      if(!postDataTmpFile.delete())
        SpotlightLog.warn(this.getClass,"Could not delete the temporary file: %s", postDataTmpFile.getCanonicalPath)
  }

  def spotCorpus(corpus: AnnotatedTextSource, outputDirName: String){
    SpotlightLog.info(this.getClass, "Spot requesting parameters:" +
      "\n\tCorpus = "+corpus.name+
      "\n\tEvaluator = "+this.toString+
      "\n\tSpotting output directory = "+outputDirName)

    val outputDir: File = new File(outputDirName)
    if(!outputDir.exists() || !outputDir.isDirectory)
      throw new IllegalArgumentException("Invalid output directory: "+outputDir.getCanonicalPath)

    var countParagraphs: Int = 0
    corpus.foreach{ paragraph =>
      countParagraphs += 1
      spotParagraph(paragraph, outputDir.getCanonicalPath+File.separator+corpus.name+"-"+spotter+"-P"+countParagraphs)
    }
  }

  private def extractSpottingOccsFromFile(spottedParagraphFileName: String):List[SurfaceFormOccurrence] = {
    val parser: SpotXmlParser = new SpotXmlParser()    
    try{
      return parser.extract(new Text(Source.fromFile(spottedParagraphFileName).getLines().mkString(""))).asScala.toList
    }catch {
      case e: FileNotFoundException => {
        SpotlightLog.fatal(this.getClass, "Could not find the spotted paragraphs file: %s" , spottedParagraphFileName)
        throw e
      }
      case e: SAXParseException => {
        val escaped = "<annotation text=\"Escaped by SpotEval.evaluate\">"+Source.fromFile(spottedParagraphFileName).getLines().drop(1).mkString("")
        try{
          return parser.extract(new Text(escaped)).asScala.toList
        }catch{
          case e: SAXParseException => {
            SpotlightLog.fatal(this.getClass, "Could not parse the Spotlight answer at: %s", spottedParagraphFileName)
            throw e            
          }
        }
      }
    }
  }
  
  private def compareParagraphSpotting(expectedSpottingOccs:List[SpotEvalOccurrence], spottedParagraphSpottingOccs:List[SpotEvalOccurrence]): Int = {
    /* Sort the Spotting Occs Lists */    
    val expected:List[SpotEvalOccurrence] = expectedSpottingOccs.sorted
    val ans:List[SpotEvalOccurrence] = spottedParagraphSpottingOccs.sorted

    /* Compare the ans and expected list */
    var tp: Int = 0 //True positives of the current paragraph
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
      ended and no more tp incrementation is possible, i.e. the comparison for this paragraph has terminated. */
      case e: IndexOutOfBoundsException => if(e.getMessage.toInt != ans.length) throw e //else The List ans is terminate, so also the true positives (tp)
    }

    tp
  }

  /* Evaluate the /spot interface result at spottedParagraphsDirName using the Corpus spots and save it into the output tsv file */ 
  def evaluate(corpus: AnnotatedTextSource, spottedParagraphsDirName: String, outputFileName: String){
    evaluate(corpus,spottedParagraphsDirName, outputFileName, null)
  }

  def evaluate(corpus: AnnotatedTextSource, spottedParagraphsDirName: String, outputFileName: String, acceptableTypes: List[String]){
    SpotlightLog.info(this.getClass, "Evaluation parameters:" +
      "\n\tCorpus = "+corpus.name+
      "\n\tEvaluator = "+this.toString+
      "\n\tAcceptable Types = "+ (if(acceptableTypes != null) acceptableTypes.mkString(" , ") else "All") +
      "\n\tSpotted paragraphs directory = "+spottedParagraphsDirName+
      "\n\tEvaluation results file = "+outputFileName)

    val outputStream = new PrintStream(outputFileName)

    /* Metrics var */
    var totRetrieved: Int = 0
    var totRelevant: Int = 0
    var totTP: Int = 0
    var countParagraphs: Int = 0
    var avgTP: Float = 0
    var avgPrecision: Float = 0
    var avgRecall: Float = 0
    var avgF1: Float = 0
    
    corpus.foreach{ paragraph =>
      countParagraphs += 1

      /* Read the answer as List of SpotEvalOccurrence from the file with the current paragraph spotted */
      val spottedParagraphFileName: String = spottedParagraphsDirName+File.separator+corpus.name+"-"+spotter+"-P"+countParagraphs
      var ans:List[SpotEvalOccurrence] = SpotEvalOccurrence.convertFromSurfaceFormOccurrence(extractSpottingOccsFromFile(spottedParagraphFileName))

      /* Read the paragraph expected annotations as List of SpotEvalOccurrence from the occurrences of the current paragraph (AnnotatedParagraph) */
      var expected:List[SpotEvalOccurrence] = SpotEvalOccurrence.convertFromDBpediaResourceOccurrence(paragraph.occurrences)

      /* Filter types */
      if(acceptableTypes != null){
        expected.foreach(_.definePossibleTypes(true))
        ans.foreach(_.definePossibleTypes(false))

        ans = ans.filter(_.isOfAcceptableTypes(acceptableTypes))
        expected = expected.filter(_.isOfAcceptableTypes(acceptableTypes))
      }

      /* Compare the expected and the Spotlight answer and define the true positives for the current paragraph */
      val tp: Int = compareParagraphSpotting(expected , ans)

      /* Calculate the paragraph's metrics */
      val retrieved: Int = ans.length
      val relevant: Int = expected.length
      val precision: Float = if(retrieved != 0) tp / retrieved.toFloat else -1
      val recall = if(relevant != 0) tp / relevant.toFloat else -1
      var f1: Float = precision+recall
      f1 = if(f1 > 0 && precision >= 0 && recall >= 0) 2*precision*recall / (f1) else -1

      /* Increment the metric of the whole text (All valid paragraphs) and the average metrics*/
      totRetrieved += retrieved
      totRelevant += relevant
      totTP += tp
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
      
      /* Save the metrics of the current paragraph */
      outputStream.println(List("P#"+countParagraphs, tp, retrieved, relevant, precisionStr, recallStr, f1Str).mkString("\t"))      
    }
    /* If no paragraph is valid inform it and do not make sense calculate the metrics */
    if(countParagraphs == 0)
      SpotlightLog.warn(this.getClass, "The informed source has no paragraphs to be evaluated.")
    else{ //If at least one paragraph is valid calculate the metrics for the whole text (all valid paragraphs) and the average ones
      /* Calculate the Whole Text (All valid paragraph)'s metrics */
      val totPrecision: Float = if(totRetrieved != 0) totTP / totRetrieved.toFloat else -1
      val totRecall: Float = if(totRelevant != 0) totTP / totRelevant.toFloat else -1
      var f1OfTotMeasures: Float = totPrecision+totRecall
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

      outputStream.println(List("All", totTP, totRetrieved, totRelevant, totPrecisionStr, totRecallStr, f1OfTotMeasuresStr).mkString("\t"))

      /* Calculate the average of the paragraph's metrics  */
      avgTP /= countParagraphs
      avgPrecision /= countParagraphs
      avgRecall /= countParagraphs
      avgF1 /= countParagraphs
      val avgRetrieved: Float = totRetrieved/countParagraphs
      val avgRelevant: Float = totRelevant/countParagraphs

      /* Save the average of the paragraph's metrics  */
      outputStream.println(List("Avg", avgTP, avgRetrieved, avgRelevant, avgPrecision, avgRecall, avgF1).mkString("\t"))
    }
    
    outputStream.close()
  }

  def complementOfTP(corpus: AnnotatedTextSource, spottedParagraphsDirName: String, outputFileName: String) {
    SpotlightLog.info(this.getClass, "TP Complement definition parameters:" +
      "\n\tCorpus = "+corpus.name+
      "\n\tEvaluator = "+this.toString+
      "\n\tSpotted paragraphs directory = "+spottedParagraphsDirName+
      "\n\tTP Complement results file = "+outputFileName)

    val outputStream = new PrintStream(outputFileName)

    var countParagraphs: Int = 0

    corpus.foreach{ paragraph =>
      countParagraphs += 1

      /* Read the answer as List of SpotEvalOccurrence from the file with the current paragraph spotted */
      val spottedParagraphFileName: String = spottedParagraphsDirName+File.separator+corpus.name+"-"+spotter+"-P"+countParagraphs
      val ans:List[SpotEvalOccurrence] = SpotEvalOccurrence.convertFromSurfaceFormOccurrence(extractSpottingOccsFromFile(spottedParagraphFileName))

      /* Read the paragraph expected annotations as List of SpotEvalOccurrence from the occurrences of the current paragraph (AnnotatedParagraph) */
      val expected:List[SpotEvalOccurrence] = SpotEvalOccurrence.convertFromDBpediaResourceOccurrence(paragraph.occurrences)

      /* Compare the expected and the Spotlight answer and define the true positives for the current paragraph */
      val paragraphComplement = paragraphComplementOfTP(expected , ans)

      /* Save the average of the paragraph's metrics  */
      paragraphComplement.foreach{ c =>
        outputStream.println(List("P#"+countParagraphs, c._1.getOffset(), c._1.getSurfaceForm().name, c._2).mkString("\t"))
      }
    }
    /* If no paragraph is valid inform it and do not make sense calculate the metrics */
    if(countParagraphs == 0)
      SpotlightLog.warn(this.getClass, "The informed source has no paragraphs to be evaluated.")

    outputStream.close()
  }

  private def paragraphComplementOfTP(expectedSpottingOccs:List[SpotEvalOccurrence], spottedParagraphSpottingOccs:List[SpotEvalOccurrence]): List[(SpotEvalOccurrence, String)] = {
    /* Sort the Spotting Occs Lists */
    val expected:List[SpotEvalOccurrence] = expectedSpottingOccs.sorted
    val ans:List[SpotEvalOccurrence] = spottedParagraphSpottingOccs.sorted

    var tpComplement: List[(SpotEvalOccurrence, String)] = List()
    
    /* Compare the ans and expected list */
    var list: List[String] = List()
    var i: Int = 0 //Index of the ans list current element
    var j: Int = 0 //Index of the expected list current element
    breakable{
      expected.foreach{ e=>
        if(i >= ans.length)
          break()
        while(e.getOffset() > ans(i).getOffset()){
          tpComplement = tpComplement :+ (ans(i), "Retrieved")
          i += 1
          if(i >= ans.length)
            break()
        }

        if(e.getOffset() == ans(i).getOffset()){
          if(!(justOffset || e.getSurfaceForm().equals(ans(i).getSurfaceForm())) ){
            tpComplement = tpComplement :+ (expected(j), "Relevant")
          }
          i += 1
          if(i >= ans.length)
            break()
        }else{
          tpComplement = tpComplement :+ (expected(j), "Relevant")
        }

        j += 1
      }
    }
    if(i >= ans.length){
      expected.slice(j, expected.length).foreach{ e =>
        tpComplement = tpComplement :+ (e, "Relevant")
      }
    }

    tpComplement
  }

  override def toString:String = "SpotEval["+spotlightServer+" | "+spotter+" | "+justOffset+"]"  
}

object SpotEval{

  var qcStore : MemoryQuantizedCountStore = _
  var resStore : MemoryResourceStore = _
  var sfStore : MemorySurfaceFormStore = _
  var candMapStore : MemoryCandidateMapStore = _
  var searcher : DBCandidateSearcher = _

  val namespace = "http://dbpedia.org/resource/"
  val rawDataFolder = new File("D:/Spotlight/data/dbpedia/en")

  val wikipediaToDBpediaClosure = new WikipediaToDBpediaClosure(
    namespace,
    new FileInputStream(new File(rawDataFolder, "redirects_en.nt")),
    new FileInputStream(new File(rawDataFolder, "disambiguations_en.nt"))
  )

  def batchSpotEval(evaluatorsList: List[SpotEval], corpus: AnnotatedTextSource, outputBaseDirName: String): File = {       
    val outputBaseDir: File = new File(outputBaseDirName)
    if(!outputBaseDir.exists() && !outputBaseDir.mkdir())
      throw new NoSuchElementException("Could not reach the output directory: "+outputBaseDir.getCanonicalPath)
    val spottedCorpusBaseDir = new File(outputBaseDir.getCanonicalPath+File.separator+"spotted")
    if(!spottedCorpusBaseDir.exists() && !spottedCorpusBaseDir.mkdir())
      throw new NoSuchElementException("Could not reach the output directory: "+spottedCorpusBaseDir.getCanonicalPath)
    val resultsDir = new File(outputBaseDir.getCanonicalPath+File.separator+"results")
    if(!resultsDir.exists() && !resultsDir.mkdir())
      throw new NoSuchElementException("Could not reach the output directory: "+resultsDir.getCanonicalPath)

      evaluatorsList.foreach{ evaluator =>
        //Execute the Evaluator's spot interface for each paragraphs of the corpus
        val spottingOutputDir: File = new File(spottedCorpusBaseDir+File.separator+evaluator.spotter)
        if(!spottingOutputDir.exists() && !spottingOutputDir.mkdir())
          throw new NoSuchElementException("Could not reach the output directory: "+spottingOutputDir.getCanonicalPath)        
        var spotted = true    
         //Run the spotting and inform exceptions
        try{
          evaluator.spotCorpus(corpus, spottingOutputDir.getCanonicalPath)
        }catch {
          case e: Exception => {
            spotted = false //To prevent useless execution of the evaluation
            SpotlightLog.error(this.getClass, "When spotting with %s threw the exception bellow: %s\n", evaluator.toString, e.getMessage+"\n"+e.getStackTrace.mkString("\n"))
          }
        }
        //Evaluate the spotted paragraphs, only if no exception has occurred when spotting the corpus, otherwise continue to the next evaluator
        if(spotted){                 
          //The output file with the evaluation results
          val resultsOutputFileName: String = resultsDir.getCanonicalPath+File.separator+"SpotEvalResults-"+corpus.name+"-"+evaluator.spotter+".tsv"                    
          //Run the evaluation and inform exceptions
          try{
            evaluator.evaluate(corpus, spottingOutputDir.getCanonicalPath, resultsOutputFileName)
          }catch {
            case e: Exception => {
              SpotlightLog.error(this.getClass, "The evaluation of spotted corpus at %s\n with %s\n threw the exception bellow:\n%s",
                spottingOutputDir.getCanonicalPath, evaluator.toString, e.getMessage+"\n"+e.getStackTrace.mkString("\n"))
              SpotlightLog.info(this.getClass, "Continue to the next spotter..")
            }
          }        
        }else //If an exception happens during the spotting continue to the next evaluator
          SpotlightLog.info(this.getClass, "An error occurred when spotting the paragraphs. Continue to the next spotter..")        
      }
    
    resultsDir  
  }

  def gather(textId: String, evaluatorsList: List[SpotEval], corpus: AnnotatedTextSource, resultsDir: File): String = {
    val textIdStd = textId.toLowerCase().trim
    var textLineId: String = ""
    if(textIdStd(0).equals("p"))
      if(textIdStd(1).equals("#"))
        textLineId = "P#"+textIdStd.substring(2)
      else
        textLineId = "P#"+textIdStd.substring(1)
    else if(textIdStd.equals("all") || textIdStd.equals("tot") || textIdStd.equals("whole") || textIdStd.equals("entire") || textIdStd.equals("total"))
      textLineId = "All"
    else if(textIdStd.equals("avg") || textIdStd.equals("average"))
      textLineId = "Avg"
    else
      throw new IllegalArgumentException("Invalid textId = "+textId)

    val outputFileName = resultsDir.getCanonicalPath+File.separator+corpus.name+"-"+textLineId
    val stream = new PrintStream(outputFileName)
    evaluatorsList.foreach{ evaluator =>
      val evaluatorResultsFileName: String = resultsDir.getCanonicalPath+File.separator+"SpotEvalResults-"+corpus.name+"-"+evaluator.spotter+".tsv"
      breakable(
        Source.fromFile(evaluatorResultsFileName).getLines().foreach{ line =>
          val lineArray = line.split("\t")
          if(lineArray(0).equals(textLineId)){
            stream.println((evaluator.spotter +: lineArray.tail).mkString("\t"))
            break()
          }
        }
      )
    }
    stream.close()

    outputFileName
  }

  def defaultPipeLine(evaluatorsList: List[SpotEval], goldStandardList: List[(AnnotatedTextSource, String)]){ //goldStandardList: List[(corpus, resultsDirNameOfTheCorpus)]
    goldStandardList.foreach{ gs =>     
      val resultsDir: File = batchSpotEval(evaluatorsList, gs._1, gs._2)
      List("All", "Avg").foreach{ id =>
        SpotlightLog.info(this.getClass, "The metrics line \"%s\" of each evaluator were gathered in: %s",
                                          id, gather(id, evaluatorsList, gs._1, resultsDir))
      }
    }
  }

  def getSpotType(spot: String): String = {
    breakable(
      for (candidate <- searcher.getCandidates(new SurfaceForm(spot)).toList) {
        println(candidate.surfaceForm.name)
        System.exit(1)

        try {
          val typesList = resStore.getResourceByName(candidate.resource.uri).getTypes.asScala.toList
          //println(candidate.resource.uri + " has: " + typesList.size + " types.")

          for (singleType <- typesList) {
            singleType.toString match {
              case "DBpedia:Person" => {
                //println("The candidate " + candidate.surfaceForm + " is a person.")
                return "DBpedia:Person"
              }
              case "DBpedia:Place" => {
                //println("The candidate " + candidate.surfaceForm + " is a place.")
                return "DBpedia:Place"
              }
              case "DBpedia:Organisation" => {
                //println("The candidate " + candidate.surfaceForm + " is an organization.")
                return "DBpedia:Organisation"
              }
              case _ =>
            }
          }
        } catch {
          case e: DBpediaResourceNotFoundException => println("DBpedia resource " + candidate.resource.uri + " not found.")
        }
      }
    )
    "Other"
  }

  def main(args: Array[String]){

    val isQS: InputStream = new FileInputStream(new File(args(0)))
    val isRS: InputStream = new FileInputStream(new File(args(1)))
    val isSFS: InputStream = new FileInputStream(new File(args(2)))
    val isCMS: InputStream = new FileInputStream(new File(args(3)))
    qcStore = MemoryStore.loadQuantizedCountStore(isQS)
    resStore = MemoryStore.loadResourceStore(isRS, qcStore)
    sfStore = MemoryStore.loadSurfaceFormStore(isSFS, qcStore)
    candMapStore = MemoryStore.loadCandidateMapStore(isCMS, resStore, qcStore)
    searcher = new DBCandidateSearcher(resStore, sfStore, candMapStore)

    //val cands = searcher.getCandidates(new SurfaceForm("PPRT"))
    //for (cand <- cands) {
      //println(cand.surfaceForm.name)
    //}
    //println(cands.size)
    //System.exit(1)

    var evaluatorsList = List[SpotEval]()
    val luceneSpoters = List("LingPipeSpotter", "AtLeastOneNounSelector", "CoOccurrenceBasedSelector",
      "NESpotter", "KeyphraseSpotter", "OpenNLPChunkerSpotter"
    )
    luceneSpoters.foreach{ spotter =>
      evaluatorsList = evaluatorsList :+ new SpotEval("http://spotlight.dbpedia.org/rest/", spotter)
    }
    evaluatorsList = evaluatorsList :+ new SpotEval("http://spotlight.sztaki.hu:2222/rest/", "Default")

    val outputBaseDirName: String = "D:/DB_output/spotted_corpus"

    var gsList: List[(AnnotatedTextSource, String)] = List()

    //M&W mock
    gsList = gsList :+ ( MilneWittenCorpus.fromDirectory(new File(
      "D:/DB_output/spotted_corpus/corpus/2u-mock-MilneWitten")), (outputBaseDirName+"/mw-mock") )
    //M&W
    gsList = gsList :+ ( MilneWittenCorpus.fromDirectory(new File(
      "D:/DB_output/spotted_corpus/corpus/MilneWitten-wikifiedStories")), (outputBaseDirName+"/mw") )
    //CSAW mock
    gsList = gsList :+ ( CSAWCorpus.fromDirectory(new File(
      "D:/DB_output/spotted_corpus/corpus/CSAW_crawledDocs")), (outputBaseDirName+"/csaw") )
    //CoNLL
    gsList = gsList :+ ( AidaCorpus.fromFile(new File(
      "D:/DB_output/spotted_corpus/corpus/conll-yago/CoNLL-YAGO.tsv")), (outputBaseDirName+"/conll") )

//    defaultPipeLine(evaluatorsList, gsList)

    gsList.foreach{ gs =>
      val resultsDirName: String = gs._2+"/results/"

      evaluatorsList.foreach{ e =>
        e.evaluate(gs._1, gs._2+"/spotted/"+e.spotter, resultsDirName+"SpotEvalResults-"+gs._1.name+"-"+e.spotter+".tsv", List[String]("DBpedia:Person", "DBpedia:Place", "DBpedia:Organisation"))
      }

      List("All", "Avg").foreach{ id =>
        SpotlightLog.info(this.getClass, "The metrics line \"%s\" of each evaluator were gathered in: %s",
          id, gather(id, evaluatorsList, gs._1, new File(resultsDirName)) )
      }
    }



  }
}

/* The Abstract Data Type for the spot results and corpus compatibility */
class SpotEvalOccurrence(offset : Int, surfaceForm : SurfaceForm, uri : String) extends Comparable[SpotEvalOccurrence]{
  def this(element: DBpediaResourceOccurrence) = this(element.textOffset, element.surfaceForm, element.resource.uri)
  def this(element: SurfaceFormOccurrence) = this(element.textOffset, element.surfaceForm, "")

  var possibleTypes = List[String]()
  def definePossibleTypes(isKnown : Boolean) {
    if (isKnown) {
      possibleTypes = List(SpotEval.wikipediaToDBpediaClosure.wikipediaToDBpediaURI("http://en.wikipedia.org/wiki/" + uri.replaceAll(" ","""_""")))
      println("http://en.wikipedia.org/wiki/" + uri.replaceAll(" ","""_"""))
      println(possibleTypes(0))
      System.exit(1)
    }
    else possibleTypes = List(SpotEval.getSpotType(surfaceForm.name)) //todo
  }

  def compareTo(o: SpotEvalOccurrence): Int = this.getOffset().compareTo(o.getOffset())

  def getOffset(): Int = offset
  def getSurfaceForm(): SurfaceForm = surfaceForm

  def isOfAcceptableTypes(acceptableTypes: List[String]): Boolean = {
    acceptableTypes.foreach { acceptableType =>
      if(possibleTypes.contains(acceptableType))
        return true
    }

    return false
  }

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