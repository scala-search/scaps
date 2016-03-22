/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus.indexing

import org.scalatest.FlatSpec

import scaps.nucleus.TestLanguage._
import scaps.nucleus.TypeRef

import org.scalatest.Matchers

import scaps.nucleus.Covariant

class TypeNormalizationSpecs extends FlatSpec with Matchers {
  import scaps.nucleus.indexing.{ InternalTypes => I }

  def normalize(t: TypeRef) = TypeNormalization.normalize(I.toInternal(t, testModel))

  def assertNorm(pairs: (TypeRef, TypeRef)*) = {
    pairs.foreach {
      case (tpe, expected) =>
        normalize(tpe) should be(expected)
    }
  }

  def assertAllEqualNorm(first: TypeRef, rest: TypeRef*) = {
    val expected = normalize(first)
    assertNorm(
      rest.map(_ -> expected): _*)
  }

  it should "use internal function types" in {
    assertNorm(
      +T.Fn(-T.Int, +T.String) -> +I.Fn(-T.Int, +T.String),
      +T.Int -> +T.Int)
  }

  it should "substitute top and bottom types with internal top and bottom" in {
    assertNorm(
      +T.Any -> +I.Top(),
      -T.Any -> -I.Top(),
      ~T.Any -> ~I.Top(),
      +T.Nothing -> +I.Bottom(),
      -T.Nothing -> -I.Bottom(),
      ~T.Nothing -> ~I.Bottom())
  }

  it should "substitute nested top and bottom" in {
    assertNorm(
      +T.Fn(-T.Any, +T.Nothing) -> +I.Fn(-I.Top(), +I.Bottom()),
      +T.List(+T.Nothing) -> +T.List(+I.Bottom()))
  }

  it should "unify function calls and method invocations" in {
    assertAllEqualNorm(
      +T.Fn(-T.Int, -T.Long, +T.String),
      +T.Fn(-T.Int, +T.Fn(-T.Long, +T.String)),
      +T.Fn(-T.Int, -T.Long, +T.Fn(+T.String)),
      +T.MethodInvocation(-T.Int, -T.Long, +T.String),
      +T.MethodInvocation(-T.Int, +T.MethodInvocation(-T.Long, +T.String)),
      +T.MemberAccess(-T.Int, +T.MethodInvocation(-T.Long, +T.String)))
  }

  it should "curry inner functions" in {
    assertNorm(
      +T.Fn(-T.Fn(+T.Int, -T.Fn(+T.Long, -T.String)), +T.String) ->
        +I.Fn(-I.Fn(+T.Int, -I.Fn(+T.Long, -T.String)), +T.String),
      +T.Fn(-T.Fn(+T.Int, +T.Long, -T.String), +T.String) ->
        +I.Fn(-I.Fn(+T.Int, -I.Fn(+T.Long, -T.String)), +T.String))
  }
}
