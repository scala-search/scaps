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
          val v = ViewDef(A, TypeRef.Unknown(Invariant))

          val res = v(TypeRef.Char(Covariant))
          assert(res == Some(TypeRef.Unknown(Invariant)))
        }

        'genericViewToModifiedVariance{
          val v = ViewDef(A, A.copy(variance = Invariant))

          val res = v(TypeRef.Char(Covariant))
          assert(res == Some(TypeRef.Char(Invariant)))
        }

        'viewWithParam{
          val v = ViewDef(TypeRef.Option(A), TypeRef.Seq(A))

          val res = v(TypeRef.Option(TypeRef.Int()))
          assert(res == Some(TypeRef.Seq(TypeRef.Int())))
        }

        'viewWithModifiedParamVariance{
          val v = ViewDef(TypeRef.Option(A), TypeRef.Seq(A.copy(variance = Invariant)))

          val res = v(TypeRef.Option(TypeRef.Int()))
          assert(res == Some(TypeRef.Seq(TypeRef.Int(Invariant))))
        }

        'viewWithMultipleParams{
          val v = ViewDef(TypeRef.Function(A :: Nil, B), TypeRef.MemberAccess(A, B))

          val res = v(TypeRef.Function(TypeRef.Int(Contravariant) :: Nil, TypeRef.Char()))
          assert(res == Some(TypeRef.MemberAccess(TypeRef.Int(Contravariant), TypeRef.Char())))
        }

        'viewWithNonMatchingArgs{
          val v = ViewDef(TypeRef.Option(TypeRef.Int()), TypeRef.Seq(TypeRef.Int()))

          val res = v(TypeRef.Option(A))
          assert(res == None)
        }

        'viewWithNonMatchingArgs2{
          val v = ViewDef(TypeRef.Option(TypeRef.Int()), TypeRef.Seq(TypeRef.Int()))

          val res = v(TypeRef.Option(TypeRef.Long()))
          assert(res == None)
        }
      }
      'retainedInformation{
        def assertRI(v: ViewDef, expected: Double) = {
          val res = v.retainedInformation
          assert(res == expected)
        }

        'fromGenericToProper{
          val v = ViewDef(A, TypeRef.Unknown(Invariant))
          assertRI(v, 1)
        }
        'fromWithParamToProper{
          val v = ViewDef(TypeRef.SList(A, Covariant), TypeRef.Unknown(Invariant))
          assertRI(v, 1d / 2)
        }
        'fromProperToProper{
          val v = ViewDef(TypeRef.SList(TypeRef.Int(Covariant), Covariant), TypeRef.Unknown(Invariant))
          assertRI(v, 1)
        }
        'addedParam{
          val v = ViewDef(TypeRef.SList(A), TypeRef.Function(TypeRef.Int(Contravariant) :: Nil, A))
          assertRI(v, 1)
        }
        'droppedParam{
          val v = ViewDef(TypeRef.Function(A :: B :: Nil, A), TypeRef.SList(A))
          assertRI(v, 3d / 4)
        }
      }

      'compose{
        val coFloat2coInt = ViewDef(TypeRef.Float(), TypeRef.Int())
        val coInt2coChar = ViewDef(TypeRef.Int(), TypeRef.Char())

        'simple{
          val res = coFloat2coInt.compose(coInt2coChar)
          assert(res == Some(ViewDef(TypeRef.Float(), TypeRef.Char())))
        }

        'invalid{
          val res = coInt2coChar.compose(coFloat2coInt)
          assert(res == None)
        }

        'withParam{
          val res = ViewDef(TypeRef.Option(A), TypeRef.Seq(A)).compose(ViewDef(TypeRef.Seq(A), TypeRef.SList(A)))
          assert(res == Some(ViewDef(TypeRef.Option(A), TypeRef.SList(A))))
        }

        'withRenamedParam{
          val res = ViewDef(TypeRef.Option(A), TypeRef.Seq(A)).compose(ViewDef(TypeRef.Seq(B), TypeRef.SList(B)))
          assert(res == Some(ViewDef(TypeRef.Option(A), TypeRef.SList(A))))
        }
      }
    }
  }
}
