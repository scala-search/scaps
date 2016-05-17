package scaps.api

sealed trait Source {
  def artifactPath: Option[String] = None
  def startPos: Option[Int] = None
  def endPos: Option[Int] = None
}

case object UnknownSource extends Source

case class PosSource(start: Int, end: Int) extends Source {
  override val startPos = Some(start)
  override val endPos = Some(end)
}

case class FileSource(path: String, innerSource: Source) extends Source {
  override val artifactPath = Some(path)
  override val startPos = innerSource.startPos
  override val endPos = innerSource.endPos
}
