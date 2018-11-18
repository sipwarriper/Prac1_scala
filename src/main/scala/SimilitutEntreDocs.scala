class SimilitutEntreDocs {

  def freq(text:String):List[(String, Int)] = {
    nomalitza(text).split(" +").groupBy(identity).mapValues(_.length).toList
  }

  def nomalitza(text:String):String = text.map(c=> if(c.isLetter) c else ' ').toLowerCase()





}
