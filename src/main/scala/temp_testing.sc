import java.io.File
import scala.math._

object testing {
  val inputlist: List [String] = List("abc", "abc", "bcd")
  val a = 'a'
  ';'.isLetter

  for (p <- "a b c".toList if p.isLetter) yield p

  val hi = "hi, im a person so cool that im Cool".split(" ").map(_.toLowerCase())
  ",".toList.exists{x => x.isLetter}

  val genius = for (p<-hi) yield p.filter(_.isLetter)

  genius.groupBy(identity)
    //.mapValues(_.length).toList



  // new try
  val hi2 = "hi, im a person so cool that im Cool".map(c=> if(c.isLetter) c else ' ').toLowerCase()
  val text = "hi. hola, que fas? que faig! -hihi\r\nasdaas"
  val separat = SimilitutEntreDocs.normalitza(text).split(" +").toList
  separat.sliding(1).toList.map(_.mkString(" "))

  val text2 = "hi, im a person so cool that im Cool"

  val v1 = SimilitutEntreDocs.normalitza(text).split(" +").sliding(1).toList.map(_.mkString(" ")).groupBy(identity).mapValues(_.length).toList
  val v2 = SimilitutEntreDocs.normalitza(text2).split(" +").sliding(1).toList.map(_.mkString(" ")).groupBy(identity).mapValues(_.length).toList

  val vv1 = (v1 ::: (for (p <-v1; b<-v2 if p._1 != b._1) yield (b _1, 0))).distinct sortBy(_._1)
  val vv2 = (v2 ::: (for (p <-v1; b<-v2 if p._1 != b._1) yield (p _1, 0))).distinct sortBy(_._1)

  v1 ::: (for (b<-v2 if !v1.toMap.contains(b._1)) yield (b _1, 0))

  vv1 zip vv2

  vv1.map(_._2)
  vv2.map(_._2)


  val llista1 = List(1,2,3,4,5)
  val llista2 = List(1,3,5,6,7).tail

  val nombreFitxers = new File(".").list.length

  val llistaFreqs = List(("hola",1),("adeu",1),("hola",1),("adeu",1),("mad",1),("hola",1),("yay",1))
  llistaFreqs.groupBy(_._1).mapValues{a => log10(nombreFitxers/a.size) }.toList

  //llista1.updated()

  ('a' to 'z').toList


//  (for ((a, b) <- vv1 zip vv2)yield a._2 * b._2).foldLeft(0.0)(_ + _)






}