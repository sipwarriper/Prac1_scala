import java.io.File
import java.util.concurrent.TimeUnit

import scala.io.Source
import scala.math.sqrt
import scala.xml.XML
import scala.util.matching.Regex
import akka.actor._
import akka.routing._
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.Await

object SimilitutEntreDocs extends App {

  final val StopWordsFileName = "english-stop.txt"
  final val DirectoriFitxers = "wikidocs"

  //classes de missatges entre els actors
  case class LlegirDirectori(nom:String)
  case class ProcessarFitxer(nom:String)
  case class FitxerLlegit(contingut:(String, String, List[String]))
  case class TractarContingutFitxer(dades:(String, String, List[String]), stopWords:List[String])
  case class FitxerTractat(dades:(String, List[(String, Double)], List[String]))

  //classe del mapWorker
  class MapWorkerProcessatFitxers() extends Actor{
    //donat un fitxer XML, el llegeix i converteix a una tupla amb el titol, el contingut i les referencies
    def mapFreq(file:String):(String, String, List[String]) =
      tractaXMLdoc(file)

    override def receive: Receive = {
      case ProcessarFitxer(nom) => val dades = mapFreq(nom)
        sender ! FitxerLlegit(dades)
    }
  }

  //classe del reduceWorker
  class ReduceWorkerProcessatFitxers() extends Actor{
    //rep un fitxer en format tupla de strings (Títol, Contingut, Referències) i
    //retorna una tupla equivalent, canviant el contingut per una llista amb les parelles de valors (paraula, frequencia)
    def reduce(fileContent:(String, String, List[String]), stopWords:List[String]):(String, List[(String, Double)], List[String]) =
      (fileContent._1, freqAtf(nonStopFreq(fileContent._2,stopWords,1)), fileContent._3)

    def receive: Receive = {
      case TractarContingutFitxer (dades, stopWords) => val res = reduce(dades, stopWords)
        sender ! FitxerTractat(res)
    }
  }

  //classe del master del tractament de fitxers
  class MapReduceTractamentFitxers() extends Actor{
    val nombreActors = 100
    val inici = System.nanoTime()
    var pendent = 0
    val MapRouter: ActorRef = context.system.actorOf(RoundRobinPool(nombreActors).props(Props[MapWorkerProcessatFitxers]))
    val stopWords: List[String] = Source.fromFile(StopWordsFileName).getLines.toList
    var LlistaContingutFitxers:List[(String, String, List[String])] = Nil
    var diccionariFitxers: mutable.Map[String, (List[(String, Double)], List[String])] = mutable.Map[String, (List[(String, Double)], List[String])]()

    //donat un directori dir, retorna una llista amb els noms dels fitxers en aquest directori
    def llistaFitxers(dir: String):List[String] = new File(dir).listFiles.filter(_.isFile).toList.map{a => dir + "/" + a.getName}

    def receive: Receive = {
      case LlegirDirectori(nom) =>
        val fitxers = llistaFitxers(nom)
        fitxers.foreach{
          f=>MapRouter ! ProcessarFitxer(f)
          pendent+=1}

      case FitxerLlegit(dades) =>
        pendent-=1
        LlistaContingutFitxers = List(dades):::LlistaContingutFitxers
        if(pendent == 0){
          val ReduceRouter = context.system.actorOf(RoundRobinPool(nombreActors).props(Props[ReduceWorkerProcessatFitxers]))
          LlistaContingutFitxers.foreach{f=>
            ReduceRouter!TractarContingutFitxer(f,stopWords)
            pendent+=1}
        }

      case FitxerTractat(dades) =>
        pendent-=1
        diccionariFitxers+=(dades._1->(dades._2,dades._3))
        if(pendent==0){
          sender ! diccionariFitxers
          context.system.terminate()
          val fi = System.nanoTime()
          println("Duració: ", (fi-inici).toDouble/1000000000.0)
        }


    }
  }

  //rep una String i retorn una llista amb tuples (paraula, freqüència)
  def freq(text:String, n:Int):List[(String, Int)] =
    normalitza(text).split(" +").sliding(n).toList.map(_.mkString(" ")).groupBy(identity).mapValues(_.length).toList

  //rep una String i la normalitza (canvia tot el que no és lletra per espais i passa la string a mínuscules)
  def normalitza(text:String):String =
    text.map(c=> if(c.isLetter) c else ' ').toLowerCase().trim

  //rep una String i una llista de Strings amb stop words, i fa el vector de frequencies filtran les stop words.
  def nonStopFreq(text:String, stop:List[String], n:Int):List[(String, Int)] =
    freq(text,n).filter{a => !stop.contains(a._1)}


  //rep dos vectors de frequencies absolutes i les converteix a tf.
  def freqAtf(llistaFreq:List[(String, Int)]):List[(String, Double)] = {
    val mesfrequent = llistaFreq.maxBy(_._2)._2
    llistaFreq.map{a=> (a._1, a._2.toDouble/mesfrequent)}
  }

  //rep dos vectors amb les paraules i les seves freqüencies tf, i retorna la semblança entre aquests dos fitxers.
  def cosinesim (txt1:List[(String,Double)], txt2:List[(String,Double)]):Double =
    (for ((a, b) <- alinearVector(txt1,txt2) zip alinearVector(txt2,txt1)) yield a._2 * b._2).foldLeft(0.0)(_ + _)/(sqrt(txt1.foldLeft(0.0){(a,b)=> a+(b._2*b._2)}) * sqrt(txt2.foldLeft(0.0){(a,b)=> a+(b._2*b._2)}) )


  def mesFrequent(text:String, stop:List[String], n:Int) = nonStopFreq(text, stop, n).maxBy(_._2)

  //busquem les paraules q no tenim a txt1 de txt2 i les afegim amb frequencia = 0 a txt1 Al final ordenem alfabeticament, per tenir el mateix ordre en els dos vectors!
  def alinearVector(aAlinear:List[(String, Double)], suport:List[(String, Double)]):List[(String, Double)] ={
    val aAlinearMap = aAlinear.toMap
    (aAlinear ::: (for (b<-suport if !aAlinearMap.contains(b._1)) yield (b _1, 0.0))) sortBy(_._1)
  }

  //obtenim les 10 frequències més frequents, i les 5 menys frequents
  def paraulafreqfreq(text:String): Unit = {
    val llistaFrequencies = freq(text,1)
    val stringFrequencies:String = llistaFrequencies.map(_._2.toString.concat(" ")).mkString
    val freqfreqList = stringFrequencies.split(" +").groupBy(identity).mapValues(_.length).toList.sortBy(-_._2)
    Console.out.println("Les 10 frequencies mes frequents:")
    val freqAltes = freqfreqList.slice(0,10)
    for(frequencia <- freqAltes)println(frequencia._2 + " paraules apareixen " + frequencia._1 +" vegades")
    Console.out.println("Les 5 frequencies menys frequents:")
    val freqBaixes = freqfreqList.slice(freqfreqList.length-5,freqfreqList.length).sortBy(-_._2)
    for(frequencia <- freqBaixes) println(frequencia._2 + " paraules apareixen " + frequencia._1 +" vegades")
  }

  //rep el nom de un docoment de la wiki i torna el nom, el contingut i una llista de referències
  def tractaXMLdoc(docName:String): (String, String, List[String]) = {
    val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(docName), "UTF-8")
    val xmllegg = XML.load(xmlleg)
    // obtinc el titol
    val titol=(xmllegg \\ "title").text
    // obtinc el contingut de la pàgina
    val contingut = (xmllegg \\ "text").text

    // identifico referències
    val refs=(new Regex("\\[\\[[^\\]]*\\]\\]") findAllIn contingut).toList
    // elimino les que tenen :
    val kk = refs.filterNot(x=> x.contains(':'))
    (titol, contingut, kk)
  }




  override def main(args: Array[String]): Unit = {
    val filename = "pg11.txt"
    val fileContents = Source.fromFile(filename).mkString

    val llistaFreq = freq(fileContents,1).sortBy(-_._2)
    //val llistaFreqFreq = paraulafreqfreq(fileContents)
    val nParules = llistaFreq.foldLeft(0){(a,b) => b._2+a}
    val diff = llistaFreq.size
    println(String.format("%-20s %-10s %-20s %-20s", "Num de Parules:", nParules.toString, "Diferents", diff.toString))
    println(String.format("%-20s %-20s %-20s ", "Paraules", "ocurrencies", "frequencia"))
    println("------------------------------------------------------")
    val sFormat = "%-20s %-20s %-1.3f "
    for (p <- llistaFreq.slice(0,10)) println(String.format(sFormat, p._1, p._2.toString, (p._2*100.0/nParules).toFloat:java.lang.Float))

    println("\n\nNONSTOP EXAMPLE\n")

    val filename2 = "english-stop.txt"
    val stopWords = Source.fromFile(filename2).getLines.toList
    val llistaNonStop = nonStopFreq(fileContents, stopWords, 1).sortBy(-_._2)
    println(String.format("%-20s %-20s %-20s ", "Paraules", "ocurrencies", "frequencia"))
    println("------------------------------------------------------")
    for (p <- llistaNonStop.slice(0,10)) println(String.format(sFormat, p._1, p._2.toString, (p._2*100.0/nParules).toFloat:java.lang.Float))

    println("\n\nn-grames\n")
    val llistaNgrames = freq(fileContents, 3).sortBy(-_._2)
    for (p <- llistaNgrames slice (0, 10)) println(String.format("%-30s %-5s", p._1, p._2.toString))

    val filename3 = "pg74.txt"
    println("\n\ncosinesim\n")

    val start = System.nanoTime()
    val newFileContents = Source.fromFile(filename).mkString
    val fileContents2 = Source.fromFile(filename3).mkString

    val freq1 = nonStopFreq(newFileContents, stopWords, 1).sortBy(-_._2)
    val freq2 = nonStopFreq(fileContents2, stopWords, 1).sortBy(-_._2)

    val txt1 = freqAtf(freq1)
    val txt2 = freqAtf(freq2)

    val hi = cosinesim(txt1, txt2)

    val end = System.nanoTime()
    println(hi)
    println((end-start).toDouble/1000000000.0)

    println("\n\n\n\n\nINICI DEL CAMP MINAT: \n")

    val system = ActorSystem("Aggregator")
    val act = system.actorOf(Props[MapReduceTractamentFitxers])
    implicit val timeout = Timeout(120,TimeUnit.SECONDS)
    val futur = act ? LlegirDirectori(DirectoriFitxers)
    val result = Await.result(futur,timeout.duration).asInstanceOf[mutable.Map[String, (List[(String, Double)], List[String])]]


  }
}

