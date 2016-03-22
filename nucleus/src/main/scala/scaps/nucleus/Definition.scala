/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

sealed trait Definition {
  def tpe: Type
  def source: String
}

case class ValueDef(
    name: String,
    tpe: Type,
    isImplicit: Boolean,
    source: String) extends Definition {
  assert(tpe.ref.variance == Covariant)
}

case class TypeDef(
    tpe: Type,
    supertypes: List[TypeRef],
    source: String) extends Definition {
  assert(tpe.ref.variance == Covariant)
  assert(supertypes.forall(_.variance == Covariant))
}

case class TypeParam(
  name: String,
  variance: Option[Variance],
  lowerBound: Option[TypeRef],
  upperBound: Option[TypeRef])
