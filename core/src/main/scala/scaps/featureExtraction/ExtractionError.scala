package scaps.featureExtraction

import scala.language.higherKinds
import scalaz._
import scalaz.syntax.monad._
import scaps.webapi.Definition
import scala.collection.generic.CanBuildFrom

case class ExtractionError(entityName: String, error: Throwable)

object ExtractionError {
  def handleErrors[M[_]: MonadPlus](errorOrEntities: M[ExtractionError \/ Definition])(handler: ExtractionError => Unit): M[Definition] =
    for {
      errorOrEntity <- errorOrEntities
      entity <- errorOrEntity.fold(e => { handler(e); MonadPlus[M].empty }, MonadPlus[M].point(_))
    } yield entity

  def ignoreErrors[M[_]: MonadPlus](errorOrEntities: M[ExtractionError \/ Definition]): M[Definition] =
    handleErrors(errorOrEntities)(_ => ())

  def logErrors[M[_]: MonadPlus](errorOrEntities: M[ExtractionError \/ Definition], log: String => Unit): M[Definition] =
    handleErrors(errorOrEntities) { e => log(s"Error during extraction of ${e.entityName}: ${e.error}") }
}
