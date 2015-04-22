package scaps.webservice

import scalatags.Text.all._
import scaps.webapi.IndexStatus

object Pages {
  def index(status: IndexStatus) =
    html(
      head(),
      body(
        h1("Scaps: Scala API Search"),
        if (status.workQueue.isEmpty)
          div("ready to search")
        else
          div(s"building index with ${status.workQueue.size} documents remaining:",
            ul(for { sourceFile <- status.workQueue } yield li(sourceFile)))))
}
