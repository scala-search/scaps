package scaps.sbtPlugin

import dispatch._
import dispatch.Defaults._

class DispatchClient(hostName: String) extends autowire.Client[String, upickle.Reader, upickle.Writer] {
  override def doCall(req: Request): Future[String] = {
    val service = host(hostName)

    val path = req.path.foldLeft(service / "api")(_ / _)

    val request = path.POST
      .setContentType("application/json", "UTF-8")
      .<<(upickle.write(req.args))

    Http(request).map(_.getResponseBody)
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
