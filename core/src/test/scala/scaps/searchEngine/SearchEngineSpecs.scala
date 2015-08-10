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
import scaps.webapi._
import scalaz.{ Contravariant => _, _ }

class SearchEngineSpecs extends FlatSpec with Matchers with IndexUtils {

  val module1 = (Module("test", "m1", "0.1.0"), extractAll("""
      package p

      object O {
        def m = 1
        def c = new C
        def f = 1f
      }

      class C
      """))

  val module2 = (Module("test", "m2", "0.1.0"), extractAll("""
      package q

      object O {
        def m = 1
        def c = new C
        def f = 1f
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

  it should "accumulate referencedFrom fields from class entities" in
    withSearchEngine { searchEngine =>
      val intClasses = searchEngine.classesIndex.findClassBySuffix("Int").get

      intClasses.size should be(1)
      intClasses.head.referencedFrom should be(
        Set(module1._1, module2._1))
    }

  it should "remove module ids in referencedFrom when class is no longer referenced from a module" in
    withSearchEngine { searchEngine =>
      val f = searchEngine.indexEntities(module1._1, extractAll("""
        package p

        // empty, float is no longer referenced from module1
        """)).get

      val floatClasses = searchEngine.classesIndex.findClassBySuffix("Float").get

      floatClasses.size should be(1)
      floatClasses.head.referencedFrom should be(
        Set(module2._1))
    }

  it should "yield an ambiguity error on ambiguities between selected modules" in
    withSearchEngine { searchEngine =>
      val result = searchEngine.search("C").get

      result should matchPattern {
        case -\/(NameAmbiguous("C", _)) =>
      }
    }

  it should "not yield ambiguity errors when ambiguities are caused by excluded modules" in
    withSearchEngine { searchEngine =>
      val result = searchEngine.search("C", Set(module1._1.moduleId)).get

      result should matchPattern {
        case \/-(_) =>
      }
    }

  it should "overwritte terms when reindexed with the same module id" in
    withSearchEngine { searchEngine =>
      val f = searchEngine.indexEntities(module1._1, extractAll("""
        package p

        object O {
          def neu = 1
        }
        """)).get

      val results = searchEngine.search("Int")
        .get.fold(qe => fail(qe.toString), identity)

      results.map(_.name) should (
        not contain ("p.O.m")
        and contain("p.O.neu"))
    }

  it should "overwritte classes when reindexed with the same module id" in
    withSearchEngine { searchEngine =>
      searchEngine.indexEntities(module1._1, extractAll("""
        package p

        // empty, p.C does no longer exist in module1
        """)).get

      val c = searchEngine.classesIndex.findClassBySuffix("p.C").get

      c should be(Seq())
    }

  def withSearchEngine(block: SearchEngine => Unit): Unit =
    withSearchEngine(module1, module2)(block)

  def withSearchEngine(modulesWithEntities: (Module, Seq[Entity])*)(block: SearchEngine => Unit) =
    withTermIndex { termIndex =>
      withClassIndex { classIndex =>
        withModuleIndex { moduleIndex =>
          withViewIndex { viewIndex =>
            val settings = Settings.fromApplicationConf

            val se = new SearchEngine(
              settings, termIndex, classIndex, moduleIndex, viewIndex)

            for ((module, entities) <- modulesWithEntities) {
              se.indexEntities(module, entities).get
            }

            block(se)
          }
        }
      }
    }
}
