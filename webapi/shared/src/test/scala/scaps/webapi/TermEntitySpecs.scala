package scaps.webapi

import utest._
import utest.framework.TestSuite

object TermEntitySpecs extends TestSuite {
  val tests = TestSuite {
    'termEntity{
      'shortName{
        'simple{
          val name = TermEntity("p.C#m", Nil, TypeEntity.Any(Covariant), "").shortName
          assert(name == "m")
        }

        'beginningWithE{
          val name = TermEntity("p.C#Executor", Nil, TypeEntity.Any(Covariant), "").shortName
          assert(name == "Executor")
        }
      }
    }
  }
}
