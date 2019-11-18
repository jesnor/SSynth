package ssynth.math

trait Point_2 [T] {
  def x : T
  def y : T
}

//case class CPoint_2 [T] (x : T, y : T) extends Point_2 [T]
case class Point_2i (x : Int, y : Int) extends Point_2 [Int]
case class Point_2f (x : Float, y : Float) extends Point_2 [Float]

trait Point_2_factory [T] extends ((T, T) => Point_2 [T])

object Point_2 {
  class Point_2_arithmetic [T] (implicit arith : Arithmetic [T], f : Point_2_factory [T])
      extends Arithmetic [Point_2 [T]] {
    import Arithmetic.Implicits._

    override def plus (a : Point_2 [T], b : Point_2 [T]) = f (a.x + b.x, a.y + b.y)
    override def minus (a : Point_2 [T], b : Point_2 [T]) = f (a.x - b.x, a.y - b.y)
    override def times (a : Point_2 [T], b : Point_2 [T]) = f (a.x * b.x, a.y * b.y)
    override def negate (a : Point_2 [T]) = f (-a.x, -a.y)
    override def zero = f (arith.zero, arith.zero)
    override def one = f (arith.one, arith.one)
    override def abs (a : Point_2 [T]) = f (a.x.abs, a.y.abs)
  }

  implicit def point_2_arithmetic [T : Arithmetic : Point_2_factory] : Point_2_arithmetic [T] = new Point_2_arithmetic
  //  implicit val point_2f_arithmetic : Point_2_arithmetic [Float] = new Point_2_arithmetic
  //implicit val point_2i_arithmetic : Point_2_arithmetic [Int] = new Point_2_arithmetic

  class Point_2_numeric [T] (p : Point_2 [T])(implicit arith : Numeric [T]) {
    import Numeric.Implicits._

    def to_int = Point_2i (p.x.toInt, p.y.toInt)
    def to_float = Point_2f (p.x.toFloat, p.y.toFloat)
  }

  implicit def point_2_numeric [T : Numeric] (p : Point_2 [T]) : Point_2_numeric [T] = new Point_2_numeric (p)

  class Point_2f_arithmetic (a : Point_2 [Float]) extends Arithmetic.Ops (a) {
    def + (v : Float) = Point_2f (a.x + v, a.y + v)
    def - (v : Float) = Point_2f (a.x - v, a.y - v)
    def * (v : Float) = Point_2f (a.x * v, a.y * v)
    def / (v : Float) = Point_2f (a.x / v, a.y / v)
    def / (b : Point_2 [Float]) = Point_2f (a.x / b.x, a.y / b.y)
  }

  implicit def point_2f_arithmetic (p : Point_2 [Float]) : Point_2f_arithmetic = new Point_2f_arithmetic (p)

  class Point_2i_arithmetic (a : Point_2 [Int]) extends Arithmetic.Ops (a) {
    def + (v : Int) = Point_2i (a.x + v, a.y + v)
    def - (v : Int) = Point_2i (a.x - v, a.y - v)
    def * (v : Int) = Point_2i (a.x * v, a.y * v)
    def / (v : Int) = Point_2i (a.x / v, a.y / v)
    def / (b : Point_2 [Int]) = Point_2i (a.x / b.x, a.y / b.y)
  }

  implicit def point_2i_arithmetic (p : Point_2 [Int]) : Point_2i_arithmetic = new Point_2i_arithmetic (p)

  class Point_2_partial_ordering [T] (implicit o : PartialOrdering [T]) extends PartialOrdering [Point_2 [T]] {
    override def tryCompare (a : Point_2 [T], b : Point_2 [T]) : Option [Int] =
      o.tryCompare (a.x, b.x).flatMap (v1 =>
        o.tryCompare (a.y, b.y).flatMap (v2 => if (v1 == v2) Option (v1) else Option.empty))

    override def lteq (a : Point_2 [T], b : Point_2 [T]) = tryCompare (a, b).exists (c => c <= 0)
  }

  implicit def point_2_partial_ordering [T : PartialOrdering] : PartialOrdering [Point_2 [T]] =
    new Point_2_partial_ordering

  case class Point_2_ordering [T] (a : Point_2 [T])(implicit o : Ordering [T], f : Point_2_factory [T]) {
    def min_elems (b : Point_2 [T]) = f (o.min (a.x, b.x), o.min (a.y, b.y))
    def max_elems (b : Point_2 [T]) = f (o.max (a.x, b.x), o.max (a.y, b.y))
  }

  implicit def point_2_ordering [T : Ordering : Point_2_factory] (p : Point_2 [T]) : Point_2_ordering [T] =
    Point_2_ordering (p)

  def min_max_elems [T : Ordering] (s : Iterable [Point_2 [T]])(implicit f : Point_2_factory [T]) : Option [(Point_2 [T], Point_2 [T])] =
    for (
      h <- s.headOption
    ) yield s.tail.foldLeft ((h, h))((m, v) => (m._1.min_elems (v), m._2.max_elems (v)))

  implicit def point_2i_factory : Point_2_factory [Int] = (x : Int, y : Int) => Point_2i (x, y)
  implicit def point_2f_factory : Point_2_factory [Float] = (x : Float, y : Float) => Point_2f (x, y)
}

object Point_2i {
  val zero = Point_2i (0)
  val one = Point_2i (1)
  val minus_one = Point_2i (-1)

  def apply (v : Int) : Point_2i = Point_2i (v, v)
}

object Point_2f {
  val zero = Point_2f (0)
  val one = Point_2f (1)
  val minus_one = Point_2f (-1)

  def apply (v : Float) : Point_2f = Point_2f (v, v)
}
