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
   *        A          X          Box[+T]      Box[C]       Box[C]       MyBox[Loop]    Bag[+T]  Y
   *        ^                       ^            ^            ^              ^            ^      ^
   *    /---|---\                   |            |            |              |            |      |
   *    B       C                MyBox[+T]      CBox    GenericCBox[+T]   Loop[+T]        YBag[+T]
   *            ^                                                            ^
   *            |                                                            |
   *            D                                                        MyLoop[+T]
   */
  val A = new TypeEntity.PrimitiveType("A")
  val B = new TypeEntity.PrimitiveType("B")
  val C = new TypeEntity.PrimitiveType("C")
  val D = new TypeEntity.PrimitiveType("D")
  val X = new TypeEntity.PrimitiveType("X")
  val Y = new TypeEntity.PrimitiveType("Y")

  val T = (v: Variance) => TypeEntity("T", v, Nil, isTypeParam = true)
  val Wildcard = (v: Variance) => TypeEntity("_", v, Nil, isTypeParam = true)

  val Box = new TypeEntity.GenericType("Box")
  val MyBox = new TypeEntity.GenericType("MyBox")
  val CBox = new TypeEntity.PrimitiveType("CBox")
  val GenericCBox = new TypeEntity.GenericType("GenericCBox")
  val Loop = new TypeEntity.GenericType("Loop")
  val MyLoop = new TypeEntity.GenericType("MyLoop")
  val Bag = new TypeEntity.GenericType("Bag")
  val YBag = new TypeEntity.GenericType("YBag")

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
      isSubTypeOf(v => GenericCBox(T(v), v), v => Box(C(v), v), 1),
      isSubTypeOf(v => Loop(T(v), v), v => MyBox(Loop(T(v), v), v), 1),
      isSubTypeOf(v => Loop(T(v), v), v => Box(Loop(T(v), v), v), 2),
      isSubTypeOf(v => MyLoop(T(v), v), v => Loop(T(v), v), 1),
      isSubTypeOf(v => MyLoop(T(v), v), v => MyBox(Loop(T(v), v), v), 2),
      isSubTypeOf(v => MyLoop(T(v), v), v => Box(Loop(T(v), v), v), 3),
      isSubTypeOf(v => YBag(T(v), v), v => Bag(T(v), v), 1),
      isSubTypeOf(v => YBag(T(v), v), Y(_), 1))
  }

  "the query analyzer expansion" should "split a simple type query into its parts" in {
    // Corresponds to the query "(A, X) => D"
    val q = TypeEntity.Ignored(A(Contravariant) :: X(Contravariant) :: D(Covariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Leaf(A(Contravariant), 1d / 3, 0, 0),
        Leaf(X(Contravariant), 1d / 3, 0, 0),
        Leaf(D(Covariant), 1d / 3, 0, 0))))
  }

  it should "use alternative types at covariant positions" in {
    // A
    val q = A(Covariant)

    expand(q) should be(unified(
      Sum(
        Max(
          Leaf(A(Covariant), 1, 0, 0),
          Leaf(B(Covariant), 1, 0, 1),
          Leaf(C(Covariant), 1, 0, 1),
          Leaf(D(Covariant), 1, 0, 2)))))
  }

  it should "use alternative types at contravariant positions" in {
    // D => _
    val q = TypeEntity.Ignored(D(Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Leaf(D(Contravariant), 1, 0, 0),
          Leaf(C(Contravariant), 1, 0, 1),
          Leaf(A(Contravariant), 1, 0, 2)))))
  }

  it should "split types with args into a sum query" in {
    // Box[A] => _
    val q = TypeEntity.Ignored(Box(A(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0),
            Leaf(A(Contravariant), 1d / 2, 1, 0))))))
  }

  it should "handle alternatives of types with args" in {
    // MyBox[B] => _
    val q = TypeEntity.Ignored(MyBox(B(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(MyBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0),
            Max(
              Leaf(B(Contravariant), 1d / 2, 1, 0),
              Leaf(A(Contravariant), 1d / 2, 1, 1))),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Max(
              Leaf(B(Contravariant), 1d / 2, 1, 0),
              Leaf(A(Contravariant), 1d / 2, 1, 1)))))))
  }

  it should "handle alternatives with additional args" in {
    // CBox => _
    val q = TypeEntity.Ignored(CBox(Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(CBox(Contravariant), 1, 0, 0)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Max(
              Leaf(C(Contravariant), 1d / 2, 1, 0),
              Leaf(A(Contravariant), 1d / 2, 1, 1)))))))
  }

  it should "handle alternatives with unrelated args" in {
    // GenericCBox[X] => _
    val q = TypeEntity.Ignored(GenericCBox(X(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(GenericCBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0),
            Leaf(X(Contravariant), 1d / 2, 1, 0)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Max(
              Leaf(C(Contravariant), 1d / 2, 1, 0),
              Leaf(A(Contravariant), 1d / 2, 1, 1)))))))
  }

  it should "expand nested types" in {
    // Box[Box[B]]
    val q = Box(Box(B(Covariant), Covariant), Covariant)

    val innerBoxParts =
      Max(
        Sum(
          Leaf(Box(Wildcard(Covariant)), 1d / 4, 1, 0),
          Max(
            Leaf(B(Covariant), 1d / 4, 2, 0))),
        Sum(
          Leaf(MyBox(Wildcard(Covariant)), 1d / 4, 1, 1),
          Max(
            Leaf(B(Covariant), 1d / 4, 2, 0))))

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(Box(Wildcard(Covariant)), 1d / 2, 0, 0),
            innerBoxParts),
          Sum(
            Leaf(MyBox(Wildcard(Covariant)), 1d / 2, 0, 1),
            innerBoxParts)))))
  }

  it should "expand types with a subtype equal to one of the type arguments" in {
    // YBag[Y] => _
    val q = TypeEntity.Ignored(YBag(Y(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(YBag(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0),
            Leaf(Y(Contravariant), 1d / 2, 1, 0)),
          Sum(
            Leaf(Bag(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(Y(Contravariant), 1d / 2, 1, 0)),
          Leaf(Y(Contravariant), 1, 0, 1)))))
  }

  it should "not recurse on self referencing types" in {
    // MyLoop[A] => _
    val q = TypeEntity.Ignored(MyLoop(A(Contravariant), Contravariant) :: Nil)

    expand(q) should be(unified(
      Sum(
        Max(
          Sum(
            Leaf(MyLoop(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 0),
            Leaf(A(Contravariant), 1d / 2, 1, 0)),
          Sum(
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 1),
            Leaf(A(Contravariant), 1d / 2, 1, 0)),
          Sum(
            Leaf(MyBox(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 2),
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 1, 0)),
          Sum(
            Leaf(Box(Wildcard(Contravariant), Contravariant), 1d / 2, 0, 3),
            Leaf(Loop(Wildcard(Contravariant), Contravariant), 1d / 2, 1, 0))))))
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
  implicit val partOrdering: Ordering[Part] = Ordering[ExpandedQuery].on(identity)
  implicit val altOrdering: Ordering[Alternative] = Ordering[ExpandedQuery].on(identity)

  def unified(q: Part): Part = (q match {
    case Max(Sum(Max(children) :: Nil) :: Nil) =>
      Max(children.map(unified))
    case Max(children) =>
      children.map(unified).sorted match {
        case (child: Leaf) :: Nil => child
        case cs                   => Max(cs)
      }
    case Leaf(t, fraction, depth, dist) =>
      Leaf(t.renameTypeParams(_ => "_"), fraction, depth, dist)
  })

  def unified(q: Alternative): Alternative = (q match {
    case Sum(Max(Sum(children) :: Nil) :: Nil) =>
      Sum(children.map(unified))
    case Sum(children) =>
      children.map(unified).sorted match {
        case (child: Leaf) :: Nil => child
        case cs                   => Sum(cs)
      }
    case Leaf(t, fraction, depth, dist) =>
      Leaf(t.renameTypeParams(_ => "_"), fraction, depth, dist)
  })
}
