package scaps.api

import utest._
import utest.framework.TestSuite

object ViewDefSpecs extends TestSuite {
  val tests = TestSuite {
    'view{
      val A = TypeRef("A", Covariant, Nil, isTypeParam = true)
      val B = TypeRef("B", Covariant, Nil, isTypeParam = true)
      'apply{
        'genericViewToConstantType{
          val v = ViewDef(A, TypeRef.Unknown(Invariant), 1, "")

          val res = v(TypeRef.Char(Covariant))
          assert(res == Some(TypeRef.Unknown(Invariant)))
        }

        'genericViewToModifiedVariance{
          val v = ViewDef(A, A.copy(variance = Invariant), 1, "")

          val res = v(TypeRef.Char(Covariant))
          assert(res == Some(TypeRef.Char(Invariant)))
        }

        'viewWithParam{
          val v = ViewDef(TypeRef.Option(A), TypeRef.Seq(A), 1, "")

          val res = v(TypeRef.Option(TypeRef.Int()))
          assert(res == Some(TypeRef.Seq(TypeRef.Int())))
        }

        'viewWithModifiedParamVariance{
          val v = ViewDef(TypeRef.Option(A), TypeRef.Seq(A.copy(variance = Invariant)), 1, "")

          val res = v(TypeRef.Option(TypeRef.Int()))
          assert(res == Some(TypeRef.Seq(TypeRef.Int(Invariant))))
        }

        'viewWithMultipleParams{
          val v = ViewDef(TypeRef.Function(A :: Nil, B), TypeRef.MemberAccess(A, B), 1, "")

          val res = v(TypeRef.Function(TypeRef.Int(Contravariant) :: Nil, TypeRef.Char()))
          assert(res == Some(TypeRef.MemberAccess(TypeRef.Int(Contravariant), TypeRef.Char())))
        }
      }
    }
  }
}
