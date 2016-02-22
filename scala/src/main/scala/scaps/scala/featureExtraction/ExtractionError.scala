/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package scaps.scala.featureExtraction

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.monad._
import scaps.api.Definition
import scala.collection.generic.CanBuildFrom
import java.io.StringWriter
import java.io.PrintWriter

case class ExtractionError(entityName: String, error: Throwable)

object ExtractionError {
  def handleErrors[M[_]: MonadPlus](errorOrDefs: M[ExtractionError \/ Definition])(handler: ExtractionError => Unit): M[Definition] =
    for {
      errorOrDef <- errorOrDefs
      definition <- errorOrDef.fold(e => { handler(e); MonadPlus[M].empty }, MonadPlus[M].point(_))
    } yield definition

  def ignoreErrors[M[_]: MonadPlus](errorOrDefs: M[ExtractionError \/ Definition]): M[Definition] =
    handleErrors(errorOrDefs)(_ => ())

  def logErrors[M[_]: MonadPlus](errorOrDefs: M[ExtractionError \/ Definition], log: String => Unit): M[Definition] =
    handleErrors(errorOrDefs) { e =>
      val sts = new StringWriter
      val pw = new PrintWriter(sts)
      e.error.printStackTrace(pw)
      log(s"Error during extraction of ${e.entityName}: ${sts.toString()}")
    }
}
