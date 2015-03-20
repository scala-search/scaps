package scala.tools.apiSearch.index

import scala.tools.apiSearch.model.TermEntity
import scala.tools.apiSearch.model.ClassEntity

/**
 * Collection of helper methods for (de-)serialisation of entities.
 *
 * These methods have been put in a separate module to spare Eclipse from macro magic
 * and to allow easy migration to other serialisation libraries.
 */
object Serialization {
  import scala.pickling._
  import scala.pickling.binary._

  def pickle(term: TermEntity): Array[Byte] =
    term.pickle.value

  def unpickleTerm(bytes: Array[Byte]) =
    BinaryPickle(bytes).unpickle[TermEntity]

  def pickle(cls: ClassEntity): Array[Byte] =
    cls.pickle.value

  def unpickleClass(bytes: Array[Byte]) =
    BinaryPickle(bytes).unpickle[ClassEntity]
}
