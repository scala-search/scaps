package scaps.featureExtraction

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.monad._
import scaps.webapi.Definition
import scala.collection.generic.CanBuildFrom

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
    handleErrors(errorOrDefs) { e => log(s"Error during extraction of ${e.entityName}: ${e.error}") }
}
