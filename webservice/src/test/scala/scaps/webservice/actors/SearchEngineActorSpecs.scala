package scaps.webservice.actors

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import org.scalatest.BeforeAndAfterAll
import org.scalatest.BeforeAndAfterEach
import org.scalatest.Finders
import org.scalatest.FlatSpecLike
import org.scalatest.Matchers
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSelection.toScala
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.testkit.ImplicitSender
import akka.testkit.TestKit
import akka.util.Timeout
import scalaz.{ Index => _, _ }
import scaps.searchEngine.SearchEngine
import scaps.settings.Settings
import scaps.webapi._
import scaps.webservice.actors.SearchEngineProtocol._
import java.io.FileNotFoundException

class SearchEngineActorSpecs(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with FlatSpecLike with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  def this() = this(ActorSystem("searchEngineActorSpecs"))

  override def beforeEach() = {
    awaitIndexReady(searchEngine)
    searchEngine ! Reset
  }

  override def afterAll() = {
    TestKit.shutdownActorSystem(system)
  }

  val searchEngineImpl = SearchEngine.inMemory(Settings.fromApplicationConf)
  val searchEngine = {
    system.actorOf(SearchEngineActor.props(searchEngineImpl), "searcher")
  }

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

    awaitIndexReady()

    searchEngine ! GetStatus

    expectMsgPF(1.second) {
      case IndexStatus(Nil, Nil, Seq(_)) => ()
    }
  }

  it should "enqueue valid jobs and report success" in {
    searchEngine ! indexModule1

    awaitIndexReady()

    searchEngine ! GetStatus

    expectMsgPF(1.second) {
      case IndexStatus(Nil, Seq(indexModule1.module), Nil) => ()
    }
  }

  it should "ensure that Index is an idempotent operation" in {
    for (_ <- 1 to 10) {
      searchEngine ! indexModule1
    }

    val status = await(searchEngine ? GetStatus).asInstanceOf[IndexStatus]
    status.allModules should be(Seq(indexModule1.module))
  }

  it should "ensure that Index is an idempotent operation when forcing reindexing" in {
    for (_ <- 1 to 10) {
      searchEngine ! indexModule1.copy(forceReindex = true)
    }

    awaitIndexReady()

    val status = await(searchEngine ? GetStatus).asInstanceOf[IndexStatus]
    status.allModules should be(Seq(indexModule1.module))
  }

  it should "not overwrite modules when forceReindex is false" in {
    searchEngine ! indexModule1

    val reindexModule1 = indexModule1.copy(sourceFile = indexModule2.sourceFile)
    searchEngine ! reindexModule1

    awaitIndexReady()

    val r1 = await(search("testModule1.C"))
    val r2 = await(search("testModule2.C"))

    r1 should be('right)
    r2 should be('left)
  }

  it should "overwrite modules when forceReindex is true" in {
    searchEngine ! indexModule1

    val reindexModule1 = indexModule1.copy(sourceFile = indexModule2.sourceFile, forceReindex = true)
    searchEngine ! reindexModule1

    awaitIndexReady()

    val r1 = await(search("testModule1.C"))
    val r2 = await(search("testModule2.C"))

    r1 should be('left)
    r2 should be('right)
  }

  it should "reject search queries while indexing" in {
    val (searchEngine, indexWorker) = searchEngineWithMockedIndexWorker()

    searchEngine ! indexModule1

    await(search("Int", searchEngine)) should matchPattern {
      case -\/(msg: String) if msg.contains("index is being built") =>
    }

    indexWorker ! "continue"

    awaitIndexReady(searchEngine)

    await(search("Int", searchEngine)) should matchPattern {
      case -\/(msg: String) if msg.contains("not found") =>
    }
  }

  it should "enqueue reset operations while indexing" in {
    val (searchEngine, indexWorker) = searchEngineWithMockedIndexWorker()

    searchEngine ! indexModule1
    searchEngine ! indexModule2

    searchEngine ! Reset

    awaitStatus(searchEngine).workQueue should be(
      Seq(indexModule1.module, indexModule2.module))

    indexWorker ! "continue"

    awaitIndexReady(searchEngine)

    awaitStatus(searchEngine).allModules should be('empty)
  }

  def await[T](f: Future[T]): T = {
    Await.result(f, 10.seconds)
  }

  def awaitIndexReady(se: ActorRef = searchEngine) =
    awaitAssert({
      val status = await(se ? GetStatus).asInstanceOf[IndexStatus]
      status.workQueue should be('empty)
    }, 5.second, 10.millis)

  def awaitStatus(se: ActorRef = searchEngine) =
    await(se ? GetStatus).asInstanceOf[IndexStatus]

  def search(q: String, se: ActorRef = searchEngine): Future[String \/ Seq[TermEntity]] =
    (se ? Search(q, Set(), 10, 0)).mapTo[String \/ Seq[TermEntity]]

  def searchEngineWithMockedIndexWorker() = {
    val searchEngine = system.actorOf(
      SearchEngineActor.props(searchEngineImpl, Some(Props[MockedIndexWorker])))

    searchEngine ! Reset

    (searchEngine, system.actorSelection(searchEngine.path / "indexWorker"))
  }
}

class MockedIndexWorker() extends Actor {
  var job: (ActorRef, Index) = null

  def receive = {
    case i: Index   => job = (sender, i)
    case "continue" => job._1 ! Indexed(job._2, None)
  }
}
