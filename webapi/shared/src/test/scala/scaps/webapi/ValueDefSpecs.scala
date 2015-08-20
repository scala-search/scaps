package scaps.webapi

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
          val name = ValueDef("p.C.m", Nil, TypeRef.Any(Covariant), "").shortName
          assert(name == "m")
        }

        'beginningWithE{
          val name = ValueDef("p.C.Executor", Nil, TypeRef.Any(Covariant), "").shortName
          assert(name == "Executor")
        }
      }
    }
  }
}
