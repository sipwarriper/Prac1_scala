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

  val povador = new SimilitutEntreDocs
  povador.freq("hi i'm the bitch of the village, YOU'RE the ugly woman. AND I HATE YOU muhahahahah muhahahahah, let's see if you're able to do something without your man" )


}