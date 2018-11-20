import scala.io.Source
import scala.math.sqrt

object SimilitutEntreDocs extends App {

  def freq(text:String, n:Int):List[(String, Int)] =
    normalitza(text).split(" +").sliding(n).toList.map(_.mkString(" ")).groupBy(identity).mapValues(_.length).toList

  def normalitza(text:String):String =
    text.map(c=> if(c.isLetter) c else ' ').toLowerCase().trim

  def nonStopFreq(text:String, stop:List[String], n:Int):List[(String, Int)] =
    freq(text,n).filter{a => !stop.contains(a._1)}

  def cosinesim (text1:String, text2:String, stop:List[String]):Double = {
    val mesfrequent1 = mesFrequent(text1,stop, 1)._2
    val mesfrequent2 = mesFrequent(text2,stop, 1)._2
    val txt1 = nonStopFreq(text1, stop, 1).map{a=> (a._1, a._2.toDouble/mesfrequent1)}
    val txt2 = nonStopFreq(text2, stop, 1).map{a=> (a._1, a._2.toDouble/mesfrequent2)}
    (for ((a, b) <- alinearVector(txt1,txt2) zip alinearVector(txt2,txt1)) yield a._2 * b._2).foldLeft(0.0)(_ + _) /( sqrt(txt1.foldLeft(0.0){(a,b)=> a+(b._2*b._2)}) * sqrt(txt2.foldLeft(0.0){(a,b)=> a+(b._2*b._2)}) )
  }

  def mesFrequent(text:String, stop:List[String], n:Int) = nonStopFreq(text, stop, n).maxBy(_._2)

  //busquem les paraules q no tenim a txt1 de txt2 i les afegim amb frequencia = 0 a txt1 Al final ordenem alfabeticament, per tenir el mateix ordre en els dos vectors!
  def alinearVector(aAlinear:List[(String, Double)], suport:List[(String, Double)]):List[(String, Double)] =
    (aAlinear ::: (for (b<-suport if !aAlinear.toMap.contains(b._1)) yield (b _1, 0.0))) sortBy(_._1) //aixo es lent!




  override def main(args: Array[String]): Unit = {
    val filename = "pg11.txt"
    val fileContents = Source.fromFile(filename).mkString

    val llistaFreq = freq(fileContents,1).sortBy(-_._2)
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

    val filename3 = "pg11-net.txt"
    val fileContents2 = Source.fromFile(filename3).mkString
    println("\n\ncosinesim\n")
    val start = System.nanoTime()

    val hi = cosinesim(fileContents,fileContents2,stopWords)

    val end = System.nanoTime()
    println(hi)
    println((end-start).toDouble/1000000000.0)


  }
}

