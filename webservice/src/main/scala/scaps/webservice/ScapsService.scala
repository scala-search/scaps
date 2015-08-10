package scaps.webservice

import spray.routing.HttpService
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.http.HttpEntity
import scalatags.Text
import spray.http.MediaTypes
import spray.http.HttpEntity
import scaps.webapi.ScapsApi
import akka.io.Tcp.Bound
import scaps.webservice.ui.Pages
import spray.http.Uri
import spray.http.HttpHeaders._
import spray.http.CacheDirectives._

class ScapsServiceActor(val apiImpl: Scaps) extends Actor with ScapsService {
  def actorRefFactory = context

  def receive = runRoute(route)
}

trait ScapsService extends HttpService {
  implicit val _ = actorRefFactory.dispatcher

  val apiImpl: ScapsApi

  def cacheControl = respondWithHeader(`Cache-Control`(`public`, `max-age`(60 * 60)))

  def route =
    path(ScapsApi.apiPath / Segments) { path =>
      post {
        extract(_.request.entity.asString) { e =>
          complete {
            Router.route[ScapsApi](apiImpl)(
              autowire.Core.Request(path, upickle.read[Map[String, String]](e)))
          }
        }
      } ~
        get {
          cacheControl {
            parameters('data) { data =>
              complete {
                Router.route[ScapsApi](apiImpl)(
                  autowire.Core.Request(path, upickle.read[Map[String, String]](data)))
              }
            }
          }
        }
    } ~
      cacheControl {
        pathSingleSlash {
          get {
            parameterMultiMap { params =>
              val indexStatus = apiImpl.getStatus()

              val moduleIds = indexStatus.map { status =>
                val modulesFromQuery = (for {
                  moduleIds <- params.lift("m").toList
                  moduleId <- moduleIds
                } yield moduleId).toSet

                if (modulesFromQuery.isEmpty)
                  status.indexedModules
                    .filter(m => m.name.contains("scala-library") || m.name.contains("scalaz"))
                    .map(_.moduleId)
                    .toSet
                else
                  modulesFromQuery
              }

              parameters('q, 'p.as[Int] ? 0) { (query, resultPage) =>
                if (query.isEmpty())
                  reject
                else
                  complete {
                    val resultOffset = resultPage * ScapsApi.defaultPageSize

                    for {
                      status <- indexStatus
                      enabledModuleIds <- moduleIds
                      result <- apiImpl.search(query, moduleIds = enabledModuleIds, offset = resultOffset)
                      page = HtmlPages.skeleton(status, enabledModuleIds,
                        result.fold(HtmlPages.queryError(_), HtmlPages.results(resultPage, query, enabledModuleIds, _)), query)
                    } yield HttpEntity(MediaTypes.`text/html`, page.toString())
                  }
              } ~
                complete {
                  for {
                    status <- indexStatus
                    enabledModuleIds <- moduleIds
                    page = HtmlPages.skeleton(status, enabledModuleIds, HtmlPages.main(status, enabledModuleIds))
                  } yield HttpEntity(MediaTypes.`text/html`, page.toString())
                }
            }
          }
        } ~
          path("scaps.css") {
            get {
              complete {
                HttpEntity(MediaTypes.`text/css`, HtmlPages.ScapsStyle.styleSheetText)
              }
            }
          } ~
          get { getFromResourceDirectory("") }
      }
}

object HtmlPages extends Pages(scalatags.Text) {
  def encodeUri(path: String, params: List[(String, String)]): String =
    (Uri(path) withQuery (params: _*)).toString()

  override val prodMode = WebserviceSettings.fromApplicationConf.prodMode

  override val analyticsScript = Some(WebserviceSettings.fromApplicationConf.analyticsScript)
}

object Router extends autowire.Server[String, upickle.Reader, upickle.Writer] {
  def read[Result: upickle.Reader](p: String) = upickle.read[Result](p)
  def write[Result: upickle.Writer](r: Result) = upickle.write(r)
}
