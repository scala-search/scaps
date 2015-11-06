package scaps.searchEngine.queries

import scala.Ordering
import org.apache.lucene.store.RAMDirectory
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ExpandedQuery.Alternative
import ExpandedQuery.Leaf
import ExpandedQuery.Max
import ExpandedQuery.Part
import ExpandedQuery.Sum
import scaps.searchEngine.index.ViewIndex
import scaps.settings.Settings
import scaps.api.Contravariant
import scaps.api.Covariant
import scaps.api.TypeRef
import scaps.api.Variance
import scaps.api.ViewDef
import scaps.scala.featureExtraction.Scala
import scaps.api.Module

class QueryAnalyzerExpansionSpecs extends FlatSpec with Matchers {
  import ExpandedQuery._

  /*
   * Mocked type hierarchies:
   *
   *        A          X          Box[+T]      Box[C]       Box[C]       MyBox[Loop]    Bag[+T]  Y   Tup[+T1, +T2]
   *        ^                       ^            ^            ^              ^            ^      ^
   *    /---|---\                   |            |            |              |            |      |
   *    B       C                MyBox[+T]      CBox    GenericCBox[+T]   Loop[+T]        YBag[+T]
   *            ^                                                            ^
   *            |                                                            |
   *            D                                                        MyLoop[+T]
   */
  val A = new TypeRef.PrimitiveType("A")
  val B = new TypeRef.PrimitiveType("B")
  val C = new TypeRef.PrimitiveType("C")
  val D = new TypeRef.PrimitiveType("D")
  val X = new TypeRef.PrimitiveType("X")
  val Y = new TypeRef.PrimitiveType("Y")

  val T = (v: Variance) => TypeRef("T", v, Nil, isTypeParam = true)
  val Wildcard = (v: Variance) => TypeRef("_", v, Nil, isTypeParam = true)

  val Box = new TypeRef.GenericType("Box")
  val MyBox = new TypeRef.GenericType("MyBox")
  val CBox = new TypeRef.PrimitiveType("CBox")
  val GenericCBox = new TypeRef.GenericType("GenericCBox")
  val Loop = new TypeRef.GenericType("Loop")
  val MyLoop = new TypeRef.GenericType("MyLoop")
  val Bag = new TypeRef.GenericType("Bag")
  val YBag = new TypeRef.GenericType("YBag")
  val Tup = new TypeRef.VariantType("Tup")

  val views = {
    def isSubTypeOf(cls: Variance => TypeRef, base: Variance => TypeRef) =
      ViewDef.bidirectional(base(Covariant), cls(Covariant), 0.5f, "")

    List(
      isSubTypeOf(B(_), A(_)),
      isSubTypeOf(C(_), A(_)),
      isSubTypeOf(D(_), C(_)),
      isSubTypeOf(D(_), A(_)),
      isSubTypeOf(v => MyBox(T(v), v), v => Box(T(v), v)),
      isSubTypeOf(CBox(_), v => Box(C(v), v)),
      isSubTypeOf(v => GenericCBox(T(v), v), v => Box(C(v), v)),
      isSubTypeOf(v => Loop(T(v), v), v => MyBox(Loop(T(v), v), v)),
      isSubTypeOf(v => Loop(T(v), v), v => Box(Loop(T(v), v), v)),
      isSubTypeOf(v => MyLoop(T(v), v), v => Loop(T(v), v)),
      isSubTypeOf(v => MyLoop(T(v), v), v => MyBox(Loop(T(v), v), v)),
      isSubTypeOf(v => MyLoop(T(v), v), v => Box(Loop(T(v), v), v)),
      isSubTypeOf(v => YBag(T(v), v), v => Bag(T(v), v)),
      isSubTypeOf(v => YBag(T(v), v), Y(_))).flatten
  }

  "the query analyzer expansion" should "split a simple type query into its parts" in {
    // Corresponds to the query "(A, X) => D"
    val q = TypeRef.Ignored(A(Contravariant) :: X(Contravariant) :: D(Covariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Leaf(A(Contravariant), 1d / 3, 0, 1),
        Leaf(X(Contravariant), 1d / 3, 0, 1),
        Leaf(D(Covariant), 1d / 3, 0, 1))))
  }

  it should "use alternative types at covariant positions" in {
    // A
    val q = A(Covariant)

    expand(q) should be(unified(
      Sum(
        Max(
          Leaf(A(Covariant), 1, 0, 1),
          Leaf(B(Covariant), 1, 0, 0.5f),
          Leaf(C(Covariant), 1, 0, 0.5f),
          Leaf(D(Covariant), 1, 0, 0.5f)))))
  }

  it should "use alternative types at contravariant positions" in {
    // D => _
    val q = TypeRef.Ignored(D(Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Leaf(D(Contravariant), 1, 0, 1),
          Leaf(C(Contravariant), 1, 0, 0.5f),
          Leaf(A(Contravariant), 1, 0, 0.5f)))))
  }

  it should "split types with args into a sum query" in {
    // Box[A] => _
    val q = TypeRef.Ignored(Box(A(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(A(Contravariant), 1d / 2, 1, 1))))))
  }

  it should "handle alternatives of types with args" in {
    // MyBox[B] => _
    val q = TypeRef.Ignored(MyBox(B(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(MyBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Max(
              Leaf(B(Contravariant), 1d / 2, 1, 1),
              Leaf(A(Contravariant), 1d / 2, 1, 0.5f))),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Max(
              Leaf(B(Contravariant), 1d / 2, 1, 1),
              Leaf(A(Contravariant), 1d / 2, 1, 0.5f)))))))
  }

  it should "handle alternatives with additional args" in {
    // CBox => _
    val q = TypeRef.Ignored(CBox(Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(CBox(Contravariant), 1, 0, 1)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Max(
              Leaf(C(Contravariant), 1d / 2, 1, 1),
              Leaf(A(Contravariant), 1d / 2, 1, 0.5f)))))))
  }

  it should "handle alternatives with unrelated args" in {
    // GenericCBox[X] => _
    val q = TypeRef.Ignored(GenericCBox(X(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(GenericCBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(X(Contravariant), 1d / 2, 1, 1)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Max(
              Leaf(C(Contravariant), 1d / 2, 1, 1),
              Leaf(A(Contravariant), 1d / 2, 1, 0.5f)))))))
  }

  it should "expand nested types" in {
    // Box[Box[B]]
    val q = Box(Box(B(Covariant), Covariant), Covariant)

    val innerBoxParts =
      Max(
        Sum(
          Leaf(Box(Wildcard(Covariant)), 1d / 4, 1, 1),
          Max(
            Leaf(B(Covariant), 1d / 4, 2, 1))),
        Sum(
          Leaf(MyBox(Wildcard(Covariant)), 1d / 4, 1, 0.5f),
          Max(
            Leaf(B(Covariant), 1d / 4, 2, 1))))

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(Box(Wildcard(Covariant)), 1d / 2, 0, 1),
            innerBoxParts),
          Sum(
            Leaf(MyBox(Wildcard(Covariant)), 1d / 2, 0, 0.5f),
            innerBoxParts)))))
  }

  it should "expand repeated types at covariant positions" in {
    // _ => (X, X)
    val q = Tup(X(Covariant) :: X(Covariant) :: Nil, Covariant)

    expand(q) should be(unified(
      Sum(
        Leaf(Tup(Wildcard(Covariant) :: Wildcard(Covariant) :: Nil, Covariant), 1d / 3, 0, 1),
        Leaf(X(Covariant), 1d / 3, 1, 1),
        Leaf(X(Covariant), 1d / 3, 1, 1))))
  }

  it should "expand types with a subtype equal to one of the type arguments" in {
    // YBag[Y] => _
    val q = TypeRef.Ignored(YBag(Y(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(YBag(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(Y(Contravariant), 1d / 2, 1, 1)),
          Sum(
            Leaf(Bag(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Leaf(Y(Contravariant), 1d / 2, 1, 1)),
          Leaf(Y(Contravariant), 1d, 0, 0.5f)))))
  }

  it should "not recurse on self referencing types" in {
    // MyLoop[A] => _
    val q = TypeRef.Ignored(MyLoop(A(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(MyLoop(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(A(Contravariant), 1d / 2, 1, 1)),
          Sum(
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Leaf(A(Contravariant), 1d / 2, 1, 1)),
          Sum(
            Leaf(MyBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 1, 1)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0.5f),
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 1, 1))))))
  }

  val viewIndex = {
    val index = new ViewIndex(new RAMDirectory)
    index.addEntities(views).get
    index
  }

  val analyzer = new QueryAnalyzer(
    Settings.fromApplicationConf.query,
    _ => ???,
    viewIndex.findAlternativesWithDistance(_).get.filter(_._1.name != TypeRef.Nothing.name))

  def expand(q: TypeRef) =
    unified(analyzer.expandQuery(q))

  implicit val ordering: Ordering[ExpandedQuery] = Ordering[(Int, Int)].on {
    case l: Leaf => (1, l.hashCode())
    case s: Sum  => (2, s.hashCode())
    case m: Max  => (3, m.hashCode())
  }
  implicit val partOrdering: Ordering[Part] = Ordering[ExpandedQuery].on(identity)
  implicit val altOrdering: Ordering[Alternative] = Ordering[ExpandedQuery].on(identity)

  def unified(q: Part): Part = (q match {
    case Max(Sum(Max(children) :: Nil) :: Nil) =>
      Max(children.map(unified).sorted)
    case Max(children) =>
      children.map(unified).sorted match {
        case (child: Leaf) :: Nil => child
        case cs                   => Max(cs)
      }
    case Leaf(t, fraction, depth, dist) =>
      Leaf(t.renameTypeParams(_ => "_"), fraction, depth, dist)
  })

  def unified(q: Alternative): Alternative = q match {
    case Sum(Max(Sum(children) :: Nil) :: Nil) =>
      Sum(children.map(unified).sorted)
    case Sum(children) =>
      children.map(unified).sorted match {
        case (child: Leaf) :: Nil => child
        case cs                   => Sum(cs)
      }
    case Leaf(t, fraction, depth, dist) =>
      Leaf(t.renameTypeParams(_ => "_"), fraction, depth, dist)
  }
}
