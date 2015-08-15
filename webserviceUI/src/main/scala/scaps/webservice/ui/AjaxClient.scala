package scaps.webservice.ui

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import org.scalajs.dom

class AjaxClient(apiPath: String) extends autowire.Client[String, upickle.Reader, upickle.Writer] {
  override def doCall(req: Request) = {
    val path = s"/$apiPath/${req.path.mkString("/")}"
    val pickled = upickle.write(req.args)
    val xmlReq =
      if (req.path.last == "search") {
        val encoded = js.Dynamic.global.encodeURIComponent(pickled).asInstanceOf[String]
        dom.ext.Ajax.get(
          url = s"$path?data=$encoded")
      } else {
        dom.ext.Ajax.post(
          url = path,
          data = pickled)
      }

    xmlReq.map(_.responseText)
  }

  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
