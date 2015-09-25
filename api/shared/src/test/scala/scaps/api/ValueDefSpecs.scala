package scaps.api

import utest._
import utest.framework.TestSuite

/**
 * Test name encoding on all platforms (JVM & JS).
 */
object ValueDefSpecs extends TestSuite {
  val tests = TestSuite {
    'valueDef{
      'shortName{
        'simple{
          val name = ValueDef("p.C.m", Nil, TypeRef.Any(Covariant), DocComment.empty).shortName
          assert(name == "m")
        }

        'beginningWithE{
          val name = ValueDef("p.C.Executor", Nil, TypeRef.Any(Covariant), DocComment.empty).shortName
          assert(name == "Executor")
        }
      }
    }
  }
}
