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
  source: String) extends Document

case class MetaDoc(
  keys: List[String],
  data: Array[Byte],
  source: String) extends Document
