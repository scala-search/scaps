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

      'noExplicitParams{
        'method - {
          val tpe = TypeEntity.MethodInvocation(
            TypeEntity.Int(Contravariant) :: TypeEntity.Int(Contravariant) :: Nil,
            TypeEntity.String())
          val t = TermEntity("a", Nil, tpe, "")

          assert(t.noExplicitParams == 2)
        }

        'member - {
          val tpe = TypeEntity.MemberAccess(
            TypeEntity.Int(),
            TypeEntity.MethodInvocation(
              Nil,
              TypeEntity.String()))
          val t = TermEntity("a", Nil, tpe, "")

          assert(t.noExplicitParams == 1)
        }

        'implicitMemberParam - {
          val tpe = TypeEntity.MemberAccess(
            TypeEntity.Int(Contravariant),
            TypeEntity.MethodInvocation(
              TypeEntity.Int(Contravariant) :: Nil,
              TypeEntity.MethodInvocation(
                TypeEntity.Implicit(TypeEntity.String()) :: Nil,
                TypeEntity.String())))
          // Int#a(_: Int)(implicit _: String): String
          val t = TermEntity("a", Nil, tpe, "")

          assert(t.noExplicitParams == 2)
        }
      }
    }
  }
}
