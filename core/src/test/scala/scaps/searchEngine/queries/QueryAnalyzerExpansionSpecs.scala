package scaps.searchEngine.queries

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scaps.webapi._
import scaps.searchEngine.View
import scaps.searchEngine.SubType
import scaps.settings.Settings
import scaps.searchEngine.index.ViewIndex
import org.apache.lucene.store.RAMDirectory

class QueryAnalyzerExpansionSpecs extends FlatSpec with Matchers {
  import ExpandedQuery._

  /*
   * Mocked type hierarchies:
   *
   *        A          X          Box[+T]      Box[C]       Box[C]
   *        ^                       ^            ^            ^
   *    /---|---\                   |            |            |
   *    B       C                MyBox[+T]      CBox    GenericCBox[+T]
   *            ^
   *            |
   *            D
   */
  val A = new TypeEntity.PrimitiveType("A")
  val B = new TypeEntity.PrimitiveType("B")
  val C = new TypeEntity.PrimitiveType("C")
  val D = new TypeEntity.PrimitiveType("D")
  val X = new TypeEntity.PrimitiveType("X")

  val T = (v: Variance) => TypeEntity("T", v, Nil, isTypeParam = true)
  val Wildcard = (v: Variance) => TypeEntity("_", v, Nil, isTypeParam = true)
  val Box = new TypeEntity.GenericType("Box")
  val MyBox = new TypeEntity.GenericType("MyBox")
  val CBox = new TypeEntity.PrimitiveType("CBox")
  val GenericCBox = new TypeEntity.GenericType("GenericCBox")

  val views = {
    def isSubTypeOf(cls: Variance => TypeEntity, base: Variance => TypeEntity, dist: Int): View =
      SubType(cls(Covariant), base(Covariant), dist)

    List(
      isSubTypeOf(B(_), A(_), 1),
      isSubTypeOf(C(_), A(_), 1),
      isSubTypeOf(D(_), C(_), 1),
      isSubTypeOf(D(_), A(_), 2),
      isSubTypeOf(v => MyBox(T(v), v), v => Box(T(v), v), 1),
      isSubTypeOf(CBox(_), v => Box(C(v), v), 1),
      isSubTypeOf(v => GenericCBox(T(v), v), v => Box(C(v), v), 1))
  }

  "the query analyzer expansion" should "split a simple type query into its parts" in {
    // Corresponds to the query "(A, X) => D"
    val q = TypeEntity.Ignored(A(Contravariant) :: X(Contravariant) :: D(Covariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Leaf(A(Contravariant), 0, 0),
        Leaf(X(Contravariant), 0, 0),
        Leaf(D(Covariant), 0, 0))))
  }

  it should "use alternative types at covariant positions" in {
    // () => A
    val q = TypeEntity.Ignored(A(Covariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Leaf(A(Covariant), 0, 0),
        Leaf(B(Covariant), 0, 1),
        Leaf(C(Covariant), 0, 1),
        Leaf(D(Covariant), 0, 2))))
  }

  it should "use alternative types at contravariant positions" in {
    // D => _
    val q = TypeEntity.Ignored(D(Contravariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Leaf(D(Contravariant), 0, 0),
        Leaf(C(Contravariant), 0, 1),
        Leaf(A(Contravariant), 0, 2))))
  }

  it should "split types with args into a sum query" in {
    // Box[A] => _
    val q = TypeEntity.Ignored(Box(A(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Sum(
          Leaf(Box(Wildcard(Contravariant), Contravariant), 0, 0),
          Leaf(A(Contravariant), 1, 0)))))
  }

  it should "handle alternatives of types with args" in {
    // MyBox[B] => _
    val q = TypeEntity.Ignored(MyBox(B(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Sum(
          Leaf(MyBox(Wildcard(Contravariant), Contravariant), 0, 0),
          Max(
            Leaf(B(Contravariant), 1, 0),
            Leaf(A(Contravariant), 1, 1))),
        Sum(
          Leaf(Box(Wildcard(Contravariant), Contravariant), 0, 1),
          Max(
            Leaf(B(Contravariant), 1, 1),
            Leaf(A(Contravariant), 1, 2))))))
  }

  it should "handle alternatives with additional args" in {
    // CBox => _
    val q = TypeEntity.Ignored(CBox(Contravariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Sum(
          Leaf(CBox(Contravariant), 0, 0)),
        Sum(
          Leaf(Box(Wildcard(Contravariant), Contravariant), 0, 1),
          Max(
            Leaf(C(Contravariant), 1, 1),
            Leaf(A(Contravariant), 1, 2))))))
  }

  it should "handle alternatives with unrelated args" in {
    // GenericCBox[X] => _
    val q = TypeEntity.Ignored(GenericCBox(X(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Sum(
          Leaf(GenericCBox(Wildcard(Contravariant), Contravariant), 0, 0),
          Leaf(X(Contravariant), 1, 0)),
        Sum(
          Leaf(Box(Wildcard(Contravariant), Contravariant), 0, 1),
          Max(
            Leaf(C(Contravariant), 1, 1),
            Leaf(A(Contravariant), 1, 2))))))
  }

  it should "expand nested types" in {
    // () => Box[Box[B]]
    val q = TypeEntity.Ignored(Box(Box(B(Covariant), Covariant), Covariant) :: Nil)

    expand(q) should be(unified(
      Max(
        Sum(
          Leaf(Box(Wildcard(Covariant)), 0, 0),
          Max(
            Sum(
              Leaf(Box(Wildcard(Covariant)), 1, 0),
              Max(
                Leaf(B(Covariant), 2, 0))),
            Sum(
              Leaf(MyBox(Wildcard(Covariant)), 1, 1),
              Max(
                Leaf(B(Covariant), 2, 1))))),
        Sum(
          Leaf(MyBox(Wildcard(Covariant)), 0, 1),
          Max(
            Sum(
              Leaf(Box(Wildcard(Covariant)), 1, 1),
              Max(
                Leaf(B(Covariant), 2, 1))),
            Sum(
              Leaf(MyBox(Wildcard(Covariant)), 1, 2),
              Max(
                Leaf(B(Covariant), 2, 2))))))))
  }

  val viewIndex = {
    val index = new ViewIndex(new RAMDirectory)
    index.addEntities(views).get
    index
  }

  val analyzer = new QueryAnalyzer(
    Settings.fromApplicationConf,
    _ => ???,
    viewIndex.findAlternativesWithDistance(_).get)

  def expand(q: TypeEntity) =
    unified(analyzer.expandQuery(q))

  implicit val ordering: Ordering[ExpandedQuery] = Ordering[(Int, Int)].on {
    case l: Leaf => (1, l.hashCode())
    case s: Sum  => (2, s.hashCode())
    case m: Max  => (3, m.hashCode())
  }

  def unified(q: ExpandedQuery): ExpandedQuery = q.children match {
    case Nil => q match {
      case Leaf(t, depth, dist) =>
        Leaf(t.renameTypeParams(_ => "_"), depth, dist)
      case _ => q
    }
    case Seq(child) => unified(child)
    case cs         => q.withChildren(cs.map(unified).sorted)
  }
}
