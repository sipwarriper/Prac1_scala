import scala.io.Source

object SimilitutEntreDocs extends App {

  def freq(text:String, n:Int):List[(String, Int)] = normalitza(text).split(" +").sliding(n).toList.map(_.mkString(" ")).groupBy(identity).mapValues(_.length).toList

  def normalitza(text:String):String = text.map(c=> if(c.isLetter) c else ' ').toLowerCase().trim

  def nonStopFreq(text:String, stop:List[String], n:Int):List[(String, Int)] =
    freq(text,n).filter{a => !stop.contains(a._1)}

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


    }
}

