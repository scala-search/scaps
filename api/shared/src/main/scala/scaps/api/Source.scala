package scaps.api

sealed trait Source {
  def artifactPath: String
  def startPos: Option[Int] = None
  def endPos: Option[Int] = None
}

case object UnknownSource extends Source {
  override val artifactPath = "<unknown>"
}

case class PosSource(start: Int, end: Int) extends Source {
  override val artifactPath = "<unknown>"
  override val startPos = Some(start)
  override val endPos = Some(end)
}

case class FileSource(artifactPath: String, innerSource: Source) extends Source {
  override val startPos = innerSource.startPos
  override val endPos = innerSource.endPos
}
