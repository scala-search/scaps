package scaps.webservice.actors

import scala.concurrent.duration.DurationInt
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Finders
import org.scalatest.FlatSpecLike
import org.scalatest.Matchers
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import scaps.webapi.Module
import scaps.webapi.IndexStatus
import org.scalatest.BeforeAndAfterEach
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.Future
import scalaz._
import scaps.webapi.TermEntity

class SearchEngineActorSpecs(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FlatSpecLike with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  import SearchEngineProtocol._

  def this() = this(ActorSystem("searchEngineActorSpecs"))

  override def beforeEach() = {
    searchEngine ! Reset
  }

  override def afterAll() = {
    TestKit.shutdownActorSystem(system)
  }

  val searchEngine = system.actorOf(Props[SearchEngineActor], "searchEngine")

  implicit val timeout = Timeout(5.seconds)
  implicit val ec = system.dispatcher

  val indexModule1 = {
    val path = getClass.getResource("/testModule1.jar").getPath
    Index(Module("myOrg", "module1", "0.1.0"), path, Nil, false)
  }

  val indexModule2 = {
    val path = getClass.getResource("/testModule2.jar").getPath
    Index(Module("myOrg", "module2", "0.1.0"), path, Nil, false)
  }

  "A search engine actor" should "report an error on a broken job" in {
    val job = Index(Module("", "test", ""), "/nonexistant", Nil, false)

    searchEngine ! job

    expectMsgPF(10.seconds) {
      case Indexed(job, Some(e)) => ()
    }

    searchEngine ! GetStatus

    expectMsgPF(1.second) {
      case IndexStatus(Nil, Nil) => ()
    }
  }

  it should "enqueue valid jobs and report success" in {
    searchEngine ! indexModule1

    expectMsgPF(10.seconds) {
      case Indexed(indexModule1, None) => ()
    }

    searchEngine ! GetStatus

    expectMsgPF(1.second) {
      case IndexStatus(Nil, Seq(indexModule1.module)) => ()
    }
  }

  it should "ensure that Index is an idempotent operation" in {
    for (_ <- 1 to 10) {
      await(searchEngine ? indexModule1)

      searchEngine ! GetStatus
      expectMsgPF(1.second) { case s: IndexStatus if s.allModules == Seq(indexModule1.module) => }
    }
  }

  it should "not overwrite modules when forceReindex is false" in {
    for {
      _ <- searchEngine ? indexModule1
      reindexModule1 = indexModule1.copy(sourceFile = indexModule2.sourceFile)
      _ <- searchEngine ? reindexModule1
      r1 <- search("testModule1.C")
      r2 <- search("testModule2.C")
    } {
      r1 should be('right)
      r2 should be('left)
    }
  }

  it should "overwrite modules when forceReindex is true" in {
    for {
      _ <- searchEngine ? indexModule1
      reindexModule1 = indexModule1.copy(sourceFile = indexModule2.sourceFile, forceReindex = true)
      _ <- searchEngine ? reindexModule1
      r1 <- search("testModule1.C")
      r2 <- search("testModule2.C")
    } {
      r1 should be('left)
      r2 should be('right)
    }
  }

  def await[T](f: Future[T]): T = {
    Await.result(f, 10.seconds)
  }

  def search(q: String): Future[String \/ Seq[TermEntity]] =
    (searchEngine ? Search(q, Set(), 10, 0)).mapTo[String \/ Seq[TermEntity]]
}
