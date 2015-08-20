package scaps.webapi

import utest._
import utest.framework.TestSuite

object ValueDefSpecs extends TestSuite {
  val tests = TestSuite {
    'valueDef{
      'shortName{
        'simple{
          val name = ValueDef("p.C.m", Nil, TypeEntity.Any(Covariant), "").shortName
          assert(name == "m")
        }

        'beginningWithE{
          val name = ValueDef("p.C.Executor", Nil, TypeEntity.Any(Covariant), "").shortName
          assert(name == "Executor")
        }
      }
    }
  }
}
