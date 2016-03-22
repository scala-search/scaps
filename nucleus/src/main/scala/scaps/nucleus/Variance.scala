/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

sealed trait Variance {
  def prefix: Char
  def flip: Variance
  def *(other: Variance): Variance
}
case object Invariant extends Variance {
  val prefix = '/'
  val flip = Invariant
  def *(other: Variance) = Invariant
}
case object Covariant extends Variance {
  val prefix = '+'
  val flip = Contravariant
  def *(other: Variance) = other
}
case object Contravariant extends Variance {
  val prefix = '-'
  val flip = Covariant
  def *(other: Variance) = other.flip
}
