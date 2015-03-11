package scala.tools.apiSearch.index

import scala.tools.apiSearch.model.TermEntity

object Serialization {
  import scala.pickling._
  import scala.pickling.binary._

  def pickle(term: TermEntity): Array[Byte] = {
    term.pickle.value
  }

  def unpickleTerm(bytes: Array[Byte]) = {
    BinaryPickle(bytes).unpickle[TermEntity]
  }
}
