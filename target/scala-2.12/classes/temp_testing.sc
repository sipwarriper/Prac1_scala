object testing {
  val inputlist: List [String] = List("abc", "abc", "bcd")
  val a = 'a'
  ';'.isLetter

  for (p <- "a b c".toList if p.isLetter) yield p

  val hi = "hi, im a person so cool that im Cool".split(" ").map(_.toLowerCase())
  ",".toList.exists{x => x.isLetter}

  val genius = for (p<-hi) yield p.filter(_.isLetter)

  genius.groupBy(identity).mapValues(_.size).toList



  // new try
  val hi2 = "hi, im a person so cool that im Cool".map(c=> if(c.isLetter) c else ' ').toLowerCase()
  val text = "hi. hola, que fas? que faig! -hihi\r\nasdaas"
  val separat = SimilitutEntreDocs.normalitza(text).split(" +").toList
  separat.sliding(1).toList.map(_.mkString(" "))


}