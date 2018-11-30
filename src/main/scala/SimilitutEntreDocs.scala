import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.collection.mutable
import scala.concurrent.Await
import scala.io.Source
import scala.math._
import scala.util.matching.Regex
import scala.xml.XML


object SimilitutEntreDocs extends App {

  final val StopWordsFileName = "stopwordscat.txt"
  final val DirectoriFitxers = "wikidocs-test"
  final val LlindarNoReferenciats = 0.05
  final val LlindarReferenciats = 0.01
  lazy final val nombreFitxers: Int =
    new File(DirectoriFitxers).list.length
  lazy final val stopWords = getStopWords(StopWordsFileName)

  //retorna una llista de paraules, que seran les stopwords obtingudes a través del arxiu de la ruta que hem entrat
  def getStopWords(file:String):List[String] = {
    val stopWordsSaltLinia = Source.fromFile(file).getLines.toList
    if (stopWordsSaltLinia.length>1) stopWordsSaltLinia
    else normalitza(stopWordsSaltLinia.head).split(" +").toList
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

  //retorna la paraula no-stop més frequent del text introduït, junt amb la seva freqüència
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
    println("Les 10 frequencies mes frequents:")
    val freqAltes = freqfreqList.slice(0,10)
    for(frequencia <- freqAltes)println(frequencia._2 + " paraules apareixen " + frequencia._1 +" vegades")
    println("Les 5 frequencies menys frequents:")
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
    // elimino les que tenen : (fitxers) i # (referencies internes)
    val kk = refs.filterNot(x=> x.contains(':') || x.contains('#')).map(_.takeWhile(_!='|'))
    (normalitza(titol), contingut, kk.map(normalitza).distinct)
  }
  //donada una string de directori, retorna la llista de fitxers que conté
  def llistaFitxers(dir: String):List[String] = new File(dir).listFiles.filter(_.isFile).toList.map{a => dir + "/" + a.getName}






  override def main(args: Array[String]): Unit = {
    val filename = "pg11.txt"
    val fileContents = Source.fromFile(filename).mkString

    val filename2 = "english-stop.txt"
    val englishStopWords = getStopWords(filename2)

    val llistaFreq = freq(fileContents,1).sortBy(-_._2)
    val nParules = llistaFreq.foldLeft(0){(a,b) => b._2+a}
    val diff = llistaFreq.size
    println(String.format("%-20s %-10s %-20s %-20s", "Num de Parules:", nParules.toString, "Diferents", diff.toString))
    println(String.format("%-20s %-20s %-20s ", "Paraules", "ocurrencies", "frequencia"))
    println("------------------------------------------------------")
    val sFormat = "%-20s %-20s %-1.3f "
    for (p <- llistaFreq.slice(0,10)) println(String.format(sFormat, p._1, p._2.toString, (p._2*100.0/nParules).toFloat:java.lang.Float))

    println("\n\nCàlcul primer fitxer sense stopWords\n")
    println("debug")
    val llistaNonStop = nonStopFreq(fileContents, englishStopWords, 1).sortBy(-_._2)
    println(String.format("%-20s %-20s %-20s ", "Paraules", "ocurrencies", "frequencia"))
    println("------------------------------------------------------")
    for (p <- llistaNonStop.slice(0,10)) println(String.format(sFormat, p._1, p._2.toString, (p._2*100.0/nParules).toFloat:java.lang.Float))

    println("\n\nn-grames del primer fitxer amb n = 3\n")
    val llistaNgrames = freq(fileContents, 3).sortBy(-_._2)
    for (p <- llistaNgrames slice (0, 10)) println(String.format("%-30s %-5s", p._1, p._2.toString))

    val filename3 = "pg74.txt"
    println("\n\nComparació de pg11.txt amb pg74.txt utilitzant el cosinesim\n")

    val start = System.nanoTime()
    val newFileContents = Source.fromFile(filename).mkString
    val fileContents2 = Source.fromFile(filename3).mkString

    val freq1 = nonStopFreq(newFileContents, englishStopWords, 1).sortBy(-_._2)
    val freq2 = nonStopFreq(fileContents2, englishStopWords, 1).sortBy(-_._2)

    val txt1 = freqAtf(freq1)
    val txt2 = freqAtf(freq2)

    val hi = cosinesim(txt1, txt2)

    val end = System.nanoTime()
    println(hi)
    println("El cosinesim ha tardat: " + (end-start).toDouble/1000000000.0)

    println("\n\n\n\nINICI DEL CAMP MINAT: \n")

    val system = ActorSystem("SystemActor")
    type Fitxer = (String,(List[(String,Double)],List[String]))

    val fitxers = llistaFitxers(DirectoriFitxers)//input

    //funció que donada la ruta d'un fitxer, retorna
    def mapFunctionReadFiles(file:String):(String, (String, List[String])) = {
      val valors = tractaXMLdoc(file)
      (valors._1,(valors._2,valors._3))
    }

    def reduceFunctionReadFiles(fileContent:(String, (String, List[String]))):Fitxer =
    (fileContent._1, (freqAtf(nonStopFreq(fileContent._2._1,stopWords,1)), fileContent._2._2))


    val act = system.actorOf(Props(
      new MapReduceFramework[String, String, (String,List[String]),String, (String,List[String]),String, (List[(String, Double)], List[String])](
      {f:String => List(mapFunctionReadFiles(f))},
      {f:List[(String, (String, List[String]))]=>f},
      {(f:String, s:(String, List[String])) => List(reduceFunctionReadFiles((f,s)))},
      10,10,fitxers)
    ))

    implicit val timeout = Timeout(12000,TimeUnit.SECONDS)
    val futur = act ? Iniciar()
    val diccionariFitxers = Await.result(futur,timeout.duration).asInstanceOf[mutable.Map[String, (List[(String, Double)], List[String])]]

    act ! PoisonPill




    def mapFunctionIDF(fitxer:Fitxer):List[(String,Double)] =
      (fitxer._2)._1.map{x=>(x._1,1.0)}

    def reduceFunctionIDF(dades:(String,List[(String,Double)])):(String,Double) =
      (dades._1, log10(nombreFitxers/ dades._2.foldLeft(0){ (a, _)=>a+1}))
    //      fitxers.mapValues(_.size)

    def funcioIntermitjaIDF(dades:List[(String,Double)]):List[(String,List[(String,Double)])] =
      dades.sortBy(_._1).groupBy(_._1).toList



    val act2 = system.actorOf(Props (new MapReduceFramework[Fitxer,
      String,Double,String,List[(String,Double)],String,Double](
      {f=>mapFunctionIDF(f)},
      {f=>funcioIntermitjaIDF(f)},
      {(f:String,s:List[(String,Double)])=>List(reduceFunctionIDF((f,s)))},
      10,10,diccionariFitxers.toList
    )))


    val futur2 = act2 ? Iniciar()
    val diccionariIDF = Await.result(futur2,timeout.duration).asInstanceOf[mutable.Map[String, Double]]

    act2 ! PoisonPill




    def mapComparacio(fitxer:Fitxer):Fitxer =
      (fitxer._1, (fitxer._2._1.map{f=> (f._1,f._2 * diccionariIDF(f._1))}, fitxer._2._2))

    def generarComparacions(fitxers:List[Fitxer]):List[(Fitxer,List[Fitxer])] = {
      if (fitxers.isEmpty) Nil:List[(Fitxer,List[Fitxer])]
      else {
        val fitxersSenseCap = fitxers.tail
        List((fitxers.head, fitxersSenseCap)) ::: generarComparacions(fitxersSenseCap)
      }
    }

    //def generarComparacions(fitxers:List[Fitxer]):List[(Fitxer,List[Fitxer])] =
    //  fitxers.map{f=>(f,fitxers)}

    def reduceComparacio(comparacions:(Fitxer,List[Fitxer])):(String, List[(String, Double)]) =
      (comparacions._1._1, for(f <- comparacions._2) yield (f._1, cosinesim(comparacions._1._2._1,f._2._1)))



    val act3 = system.actorOf(Props (new MapReduceFramework[Fitxer,
      String,(List[(String,Double)],List[String]),
      Fitxer, List[Fitxer],
      String, List[(String, Double)]](
      {f=>List(mapComparacio(f))},
      {f=>generarComparacions(f)},
      {(f:Fitxer,s:List[Fitxer])=>List(reduceComparacio((f,s)))},
      100,100,diccionariFitxers.toList
    )))
    val futur3 = act3 ? Iniciar()
    val resultatComparacions = Await.result(futur3,timeout.duration).asInstanceOf[mutable.Map[String, List[(String, Double)]]].toList.sortBy(-_._2.length)

    act3 ! PoisonPill



    //donada llista referencies
  //map: eliminar els que no superin cert llindar, despres eliminar els no referenciats
    def mapObtenirNoRefs(fitxer:(String, List[(String, Double)])):(String, List[(String, Double)]) ={
      (fitxer._1,fitxer._2.filter(_._2>LlindarNoReferenciats).filter{
        f => !diccionariFitxers(fitxer._1)._2.contains(f._1) && !diccionariFitxers(f._1)._2.contains(fitxer._1)
      })
    }
    val act4 = system.actorOf(Props (new MapReduceFramework[
        (String, List[(String, Double)]),
        String,List[(String, Double)],
        String, List[(String, Double)],
        String, List[(String, Double)]]
    (
      {f=>List(mapObtenirNoRefs(f))},
      {f=>f},
      {(f:String, s:List[(String, Double)])=>scala.List((f,s))},
      100,100,resultatComparacions
    )))

    val futur4 = act4 ? Iniciar()
    val resultatObtenirNoRefs = Await.result(futur4,timeout.duration).asInstanceOf[mutable.Map[String, List[(String, Double)]]].toList.sortBy(-_._2.length)

    act4 ! PoisonPill





    def mapObtenirRefsDiferents(fitxer:(String, List[(String, Double)])):(String, List[(String, Double)]) = (
      fitxer._1,fitxer._2.filter(_._2>LlindarReferenciats).filter{
        f => diccionariFitxers(fitxer._1)._2.contains(f._1) || diccionariFitxers(f._1)._2.contains(fitxer._1)
    })

    val act5 = system.actorOf(Props (new MapReduceFramework[
      (String, List[(String, Double)]),
      String,List[(String, Double)],
      String, List[(String, Double)],
      String, List[(String, Double)]]
    (
      {f=>List(mapObtenirRefsDiferents(f))},
      {f=>f},
      {(f:String, s:List[(String, Double)])=>scala.List((f,s))},
      100,100,resultatComparacions
    )))

    val futur5 = act5 ? Iniciar()
    val resultatObtenirRefsDiferents = Await.result(futur5,timeout.duration).asInstanceOf[mutable.Map[String, List[(String, Double)]]].toList.sortBy(-_._2.length)

    act5 ! PoisonPill

    system.terminate()





    println("debugpoint")





    /*
      TODO-1 DONE: fer el vector de idf

      -todo-2: fer un mapreduce que faci la comparació tots amb tots
      todo-3: llistar els parells de pagines similars q no es referenciin
      map: eliminar els que no superin cert llindar, despres eliminar els no referenciats
      intermig:--
      reduce:
      todo-4: llistar parell de pagines q es referenciin pero q no siguin prou similars
      todo-5: relacionat amb els dos anteriors, decidir llindar
     */
  }
}

