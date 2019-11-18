package ssynth.math

trait Arithmetic [T] {
  def plus (x : T, y : T) : T
  def minus (x : T, y : T) : T
  def times (x : T, y : T) : T
  def negate (x : T) : T
  def zero : T
  def one : T
  def abs (x : T) : T
}

object Arithmetic {
  import scala.math.Numeric

  implicit def numeric_arithmetic [T] (implicit n : Numeric [T]) : Arithmetic [T] = new Arithmetic [T] {
    override def plus (x : T, y : T) = n.plus (x, y)
    override def minus (x : T, y : T) = n.minus (x, y)
    override def times (x : T, y : T) = n.times (x, y)
    override def negate (x : T) = n.negate (x)
    override def zero = n.zero
    override def one = n.one
    override def abs (x : T) = n.abs (x)
  }

  implicit val float_arithmetic : Arithmetic [Float] = numeric_arithmetic [Float]

  class Ops [T] (lhs : T)(implicit a : Arithmetic [T]) {
    def + (rhs : T) = a.plus (lhs, rhs)
    def - (rhs : T) = a.minus (lhs, rhs)
    def * (rhs : T) = a.times (lhs, rhs)
    def unary_- = a.negate (lhs)
    def abs : T = a.abs (lhs)
  }

  trait ExtraImplicits {
    implicit def infix_arithmetic_ops [T : Arithmetic] (x : T) : Ops [T] = new Ops (x)
  }

  object Implicits extends ExtraImplicits {}
}
