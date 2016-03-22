/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.nucleus

sealed trait Document {
  def keys: List[String]
  def data: Array[Byte]
  def source: String
}

case class ValueDoc(
    name: String,
    keys: List[String],
    data: Array[Byte],
    source: String) extends Document {

  override def toString =
    s"ValueDoc($name, $keys, ${new String(data)}, $source)"
}

case class MetaDoc(
    keys: List[String],
    data: Array[Byte],
    source: String) extends Document {

  override def toString =
    s"MetaDoc($keys, ${new String(data)}, $source)"
}
