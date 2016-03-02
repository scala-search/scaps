/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.webservice

import spray.routing.HttpService
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.http.HttpEntity
import scalatags.Text
import spray.http.MediaTypes
import spray.http.HttpEntity
import scaps.api.ScapsApi
import akka.io.Tcp.Bound
import scaps.webservice.ui.Pages
import spray.http.Uri
import spray.http.HttpHeaders._
import spray.http.CacheDirectives._
import spray.routing.ExceptionHandler
import scaps.api.IndexReady
import scaps.api.ValueDef

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
              autowire.Core.Request(path, upickle.default.read[Map[String, String]](e)))
          }
        }
      } ~
        get {
          cacheControl {
            parameters('data) { data =>
              complete {
                Router.route[ScapsApi](apiImpl)(
                  autowire.Core.Request(path, upickle.default.read[Map[String, String]](data)))
              }
            }
          }
        }
    } ~
      handleExceptions(errorHandler) {
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

                parameters('q) { (query) =>
                  if (query.isEmpty())
                    reject
                  else
                    complete {
                      import scaps.api.Result

                      def groupResults(rs: Seq[Result[ValueDef]]): Seq[(ValueDef, Seq[Result[ValueDef]])] =
                        rs.groupBy(_.entity.group).toSeq.sortBy(-_._2.head.score)

                      for {
                        status <- indexStatus
                        enabledModuleIds <- moduleIds
                        result <- apiImpl.search(query, moduleIds = enabledModuleIds, noResults = 100)
                        grouped = result.right.map(groupResults _)
                        content = grouped.fold(
                          HtmlPages.queryError(_),
                          HtmlPages.groupedResults(query, enabledModuleIds, _))
                        page = HtmlPages.skeleton(status, enabledModuleIds, content, query)
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
            pathSuffixTest("""(?i)^.*\.(css|js|png|gif|svg|pdf|jpg|jpeg|woff2|js\.map)$""".r) { _ =>
              get { getFromResourceDirectory("") }
            }
        }
      }

  val errorHandler = ExceptionHandler {
    case _ =>
      complete {
        val indexStatus = apiImpl.getStatus().recover {
          case _ => IndexReady(Nil, Nil)
        }

        for {
          status <- indexStatus
          page = HtmlPages.skeleton(status, Set(),
            HtmlPages.error("There was an internal server error, please try another query."))
        } yield HttpEntity(MediaTypes.`text/html`, page.toString())
      }
  }
}

object HtmlPages extends Pages(scalatags.Text) {
  def encodeUri(path: String, params: List[(String, String)]): String =
    (Uri(path) withQuery (params: _*)).toString()

  override val prodMode = WebserviceSettings.fromApplicationConf.prodMode

  override val analyticsScript = Some(WebserviceSettings.fromApplicationConf.analyticsScript)
}

object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {
  def read[Result: upickle.default.Reader](p: String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}
