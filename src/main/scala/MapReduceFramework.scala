
import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.routing.RoundRobinPool

import scala.collection.mutable


//crida de un mapreduce generic
case class Iniciar()


class MapReduceFramework[Input, ClauMap, ValorMap, ClauIntermitja, ValorIntermig, ClauSortida, ValorSortida]
(
  mapFunction:Input=>List[(ClauMap, ValorMap)],
  funcioIntermitja:List[(ClauMap, ValorMap)] => List[(ClauIntermitja, ValorIntermig)] ,
  reduceFunction:(ClauIntermitja, ValorIntermig)=>List[(ClauSortida, ValorSortida)],
  nombreActorsMap: Int,
  nombreActorsReduce: Int,
  input: List[Input]
) extends Actor{

  case class MissatgeMapeig(dades:Input)
  case class RespostaMapeig(dades:List[(ClauMap,ValorMap)])
  case class MissatgeReduccio(dades:(ClauIntermitja, ValorIntermig))
  case class RespostaReduccio(dades:List[(ClauSortida,ValorSortida)])

  val inici: Long = System.nanoTime()
  var pendent = 0
  var pare:ActorRef = _
  var llistaIntermedia:List[(ClauMap, ValorMap)] = Nil
  var resultat:mutable.Map[ClauSortida,ValorSortida] =mutable.Map[ClauSortida,ValorSortida]()
  val MapRouter: ActorRef = context.actorOf(RoundRobinPool(nombreActorsMap).props(Props(new Actor{
    override def receive: Receive = {
      case MissatgeMapeig(dades) => sender ! RespostaMapeig(mapFunction(dades))
    }})))
  val ReduceRouter: ActorRef = context.actorOf(RoundRobinPool(nombreActorsReduce).props(Props( new Actor{
    override def receive: Receive = {
      case MissatgeReduccio(fitxer) => sender ! RespostaReduccio(reduceFunction(fitxer._1,fitxer._2))
    }
  })))

  override def receive: Receive = {
    case Iniciar() =>
      pare = sender
      println("Iniciem el MapReduceFramework")
      input.foreach{
        f=>MapRouter ! MissatgeMapeig(f)
          pendent+=1}
    case RespostaMapeig(dades) =>
      pendent-=1
      llistaIntermedia = dades ::: llistaIntermedia
      if (pendent == 0){
        context.stop(MapRouter)
        println("--------Duració mapeig: " + (System.nanoTime()-inici).toDouble/1000000000.0 + " segons")
        funcioIntermitja(llistaIntermedia).foreach {
          f => ReduceRouter ! MissatgeReduccio(f)
            pendent+=1
        }
        println("--------Duració Fins Intermig: " + (System.nanoTime()-inici).toDouble/1000000000.0 + " segons")
      }
    case RespostaReduccio(dades) =>
      pendent -=1
      dades.foreach{a => resultat+=(a._1->a._2)}
      if (pendent == 0){
        context.stop(ReduceRouter)
        val fi = System.nanoTime()
        println("Duració: " + (fi-inici).toDouble/1000000000.0 + " segons")
        pare ! resultat
      }
  }
  override def postStop { println("Finalitzant el MapReduceFramework") }


}