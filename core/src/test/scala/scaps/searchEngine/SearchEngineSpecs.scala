/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.searchEngine

import scaps.api.Module
import scaps.settings.Settings
import scaps.searchEngine.index.IndexUtils
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scaps.api._
import scalaz.{ Contravariant => _, _ }

class SearchEngineSpecs extends FlatSpec with Matchers with IndexUtils {

  val m1 = Module("test", "m1", "0.1.0")

  val module1 = extractAll("""
      package p

      object O {
        def m = 1
        def c = new C
        /** Creates a float */
        def f = 1f
        def foo(i: Int) = i
      }

      class C
      """).map(_.withModule(m1))

  val m2 = Module("test", "m2", "0.1.0")

  val module2 = extractAll("""
      package q

      object O {
        def m = 1
        def c = new C
        def f = 1f
      }

      class C
      """).map(_.withModule(m2))

  "the search engine" should "index values from various modules" in
    withSearchEngine { searchEngine =>
      val results = searchEngine.search("Int").get.fold(qe => fail(qe.toString), _.map(_.entity))

      results.map(_.name) should (
        contain("p.O.m") and
        contain("q.O.m"))
    }

  it should "index modules" in
    withSearchEngine { searchEngine =>
      searchEngine.indexedModules().get should (
        contain(m1) and
        contain(m2))
    }

  it should "support queries with module filters" in
    withSearchEngine { searchEngine =>
      val results = searchEngine.search("Int", Set(m1.moduleId))
        .get.fold(qe => fail(qe.toString), _.map(_.entity))

      results.map(_.name) should (
        contain("p.O.m") and
        not contain ("q.O.m"))
    }

  it should "accumulate referencedFrom fields from class entities" in
    withSearchEngine { searchEngine =>
      val intTypeDefs1 = searchEngine.typeIndex.findTypeDefsBySuffix("Int", Set(m1.moduleId)).get
      intTypeDefs1.size should be(1)

      val intTypeDefs2 = searchEngine.typeIndex.findTypeDefsBySuffix("Int", Set(m2.moduleId)).get
      intTypeDefs2.size should be(1)
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
      val result = searchEngine.search("C", Set(m1.moduleId)).get

      result should matchPattern {
        case \/-(_) =>
      }
    }

  it should "reinterpret queries with a single unknown type as keyword queries" in {
    withSearchEngine { se =>
      se.search("create").get.getOrElse(Nil).map(_.entity.name) should (
        contain("p.O.f"))
    }
  }

  it should "fail on composed type queries containing an unknown type" in {
    withSearchEngine { se =>
      val results = List(
        se.search("List => Xyz"),
        se.search("List[Xyz]"),
        se.search("(List, Xyz)"))

      results.foreach { r =>
        r.get should be('left)
      }
    }
  }

  def withSearchEngine(block: SearchEngine => Unit): Unit =
    withSearchEngine(module1, module2)(block)

  def withSearchEngine(modules: Seq[Definition]*)(block: SearchEngine => Unit) =
    withValueIndex { valueIndex =>
      withTypeIndex { typeIndex =>
        withModuleIndex { moduleIndex =>
          withViewIndex { viewIndex =>
            val settings = Settings.fromApplicationConf

            val se = new SearchEngine(
              settings, valueIndex, typeIndex, moduleIndex, viewIndex)

            modules.foreach(se.index)

            block(se)
          }
        }
      }
    }
}
