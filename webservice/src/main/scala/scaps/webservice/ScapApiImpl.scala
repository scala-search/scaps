package scaps.webservice

import akka.actor.ActorSystem
import scaps.webapi.ScapsApi
import akka.actor.Props

class ScapsApiImpl(system: ActorSystem) extends ScapsApi {
  val indexActor = system.actorOf(Props[IndexActor])

  def index(sourceFile: String, classpath: Seq[String]): Unit = {
    indexActor ! Index(sourceFile, classpath)
  }
}
