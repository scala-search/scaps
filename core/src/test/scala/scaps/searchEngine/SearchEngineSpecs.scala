package scaps.searchEngine

import scaps.webapi.Module
import scaps.settings.Settings
import scaps.searchEngine.index.IndexUtils
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scaps.webapi.Entity
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scalaz._

class SearchEngineSpecs extends FlatSpec with Matchers with IndexUtils {

  val module1 = (Module("test", "m1", "0.1.0"), extractAll("""
      package p

      object O {
        def m = 1
        def c = new C
      }

      class C
      """))

  val module2 = (Module("test", "m2", "0.1.0"), extractAll("""
      package q

      object O {
        def m = 1
        def c = new C
      }

      class C
      """))

  "the search engine" should "index terms from various modules" in
    withSearchEngine { searchEngine =>
      val results = searchEngine.search("Int").get.fold(qe => fail(qe.toString), identity)

      results.map(_.name) should (
        contain("p.O.m") and
        contain("q.O.m"))
    }

  it should "index modules" in
    withSearchEngine { searchEngine =>
      searchEngine.indexedModules().get should (
        contain(module1._1) and
        contain(module2._1))
    }

  it should "support queries with module filters" in
    withSearchEngine { searchEngine =>
      val results = searchEngine.search("Int", Set(module1._1.moduleId))
        .get.fold(qe => fail(qe.toString), identity)

      results.map(_.name) should (
        contain("p.O.m") and
        not contain ("q.O.m"))
    }

  it should "yield an ambiguity error on ambiguities between selected modules" in
    withSearchEngine { searchEngine =>
      val result = searchEngine.search("C").get

      result should matchPattern {
        case -\/(NameAmbiguous("C", _)) =>
      }
    }

  it should "not yield an ambiguity error on ambiguities between not selected modules" in
    withSearchEngine { searchEngine =>
      val result = searchEngine.search("C", Set(module1._1.moduleId)).get

      result should matchPattern {
        case \/-(Seq(_)) =>
      }
    }

  it should "overwritte modules when reindexed with same id" in
    withSearchEngine { searchEngine =>
      val f = searchEngine.indexEntities(module1._1, extractAll("""
        package p

        object O {
          def neu = 1
        }
        """))

      Await.ready(f, 5.seconds)

      val results = searchEngine.search("Int")
        .get.fold(qe => fail(qe.toString), identity)

      results.map(_.name) should (
        not contain ("p.O.m")
        and contain("p.O.neu"))
    }

  def withSearchEngine(block: SearchEngine => Unit): Unit =
    withSearchEngine(module1, module2)(block)

  def withSearchEngine(modulesWithEntities: (Module, Seq[Entity])*)(block: SearchEngine => Unit) =
    withTermIndex { termIndex =>
      withClassIndex { classIndex =>
        withModuleIndex { moduleIndex =>
          val settings = Settings.fromApplicationConf

          val se = new SearchEngine(
            settings, termIndex, classIndex, moduleIndex)

          val fs = for ((module, entities) <- modulesWithEntities) yield {
            se.indexEntities(module, entities)
          }

          Await.ready(Future.sequence(fs), 5.seconds)

          block(se)
        }
      }
    }
}
