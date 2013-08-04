package scas.math

import java.util.Comparator
import spire.macros.Ops
import Ordering.Ops

@annotation.implicitNotFound(msg = "No implicit Ordering defined for ${T}.")
trait Ordering[@specialized(Byte, Short, Int, Long, Double) T] extends Comparator[T] with PartialOrdering[T] with Serializable {
  outer =>

  def tryCompare(x: T, y: T) = Some(compare(x, y))
  def compare(x: T, y: T): Int
  override def lteq(x: T, y: T): Boolean = compare(x, y) <= 0
  override def gteq(x: T, y: T): Boolean = compare(x, y) >= 0
  override def lt(x: T, y: T): Boolean = compare(x, y) < 0
  override def gt(x: T, y: T): Boolean = compare(x, y) > 0
  override def equiv(x: T, y: T): Boolean = compare(x, y) == 0
  def max(x: T, y: T): T = if (gteq(x, y)) x else y
  def min(x: T, y: T): T = if (lteq(x, y)) x else y

  override def reverse: Ordering[T] = new Ordering[T] {
    override def reverse = outer
    def compare(x: T, y: T) = outer.compare(y, x)
  }

  def on[U](f: U => T): Ordering[U] = new Ordering[U] {
    def compare(x: U, y: U) = outer.compare(f(x), f(y))
  }

  implicit def mkOrderingOps(lhs: T) = new Ops(lhs)(this)
}

trait LowPriorityOrderingImplicits {
  implicit def asScalaOrdering[A](ord: Ordering[A]): scala.Ordering[A] = new scala.Ordering[A] {
    def compare(x: A, y: A) = ord.compare(x, y)
  }
}

object Ordering extends LowPriorityOrderingImplicits {
  def apply[T](implicit ord: Ordering[T]) = ord

  trait ExtraImplicits {
    implicit def infixOrderingOps[T: Ordering](x: T) = implicitly[Ordering[T]].mkOrderingOps(x)
  }
  object Implicits extends ExtraImplicits { }

  class Ops[T: Ordering](lhs: T) {
    def <(rhs: T) = macro Ops.binop[T, Boolean]
    def <=(rhs: T) = macro Ops.binop[T, Boolean]
    def >(rhs: T) = macro Ops.binop[T, Boolean]
    def >=(rhs: T) = macro Ops.binop[T, Boolean]
    def equiv(rhs: T) = macro Ops.binop[T, Boolean]
    def max(rhs: T): T = macro Ops.binop[T, Boolean]
    def min(rhs: T): T = macro Ops.binop[T, Boolean]
  }

  trait UnitOrdering extends Ordering[Unit] {
    def compare(x: Unit, y: Unit) = 0
  }
  implicit object Unit extends UnitOrdering

  trait BooleanOrdering extends Ordering[Boolean] {
    def compare(x: Boolean, y: Boolean) = (x, y) match {
      case (false, true) => -1
      case (true, false) => 1
      case _ => 0
    }
  }
  implicit object Boolean extends BooleanOrdering

  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = x.toInt - y.toInt
  }
  implicit object Byte extends ByteOrdering

  trait CharOrdering extends Ordering[Char] {
    def compare(x: Char, y: Char) = x.toInt - y.toInt
  }
  implicit object Char extends CharOrdering

  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = x.toInt - y.toInt
  }
  implicit object Short extends ShortOrdering

  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Int extends IntOrdering

  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }
  implicit object Long extends LongOrdering

  trait FloatOrdering extends Ordering[Float] {
    outer =>

    def compare(x: Float, y: Float) = java.lang.Float.compare(x, y)

    override def lteq(x: Float, y: Float): Boolean = x <= y
    override def gteq(x: Float, y: Float): Boolean = x >= y
    override def lt(x: Float, y: Float): Boolean = x < y
    override def gt(x: Float, y: Float): Boolean = x > y
    override def equiv(x: Float, y: Float): Boolean = x == y
    override def max(x: Float, y: Float): Float = math.max(x, y)
    override def min(x: Float, y: Float): Float = math.min(x, y)

    override def reverse: Ordering[Float] = new FloatOrdering {
      override def reverse = outer
      override def compare(x: Float, y: Float) = outer.compare(y, x)

      override def lteq(x: Float, y: Float): Boolean = outer.lteq(y, x)
      override def gteq(x: Float, y: Float): Boolean = outer.gteq(y, x)
      override def lt(x: Float, y: Float): Boolean = outer.lt(y, x)
      override def gt(x: Float, y: Float): Boolean = outer.gt(y, x)
    }
  }
  implicit object Float extends FloatOrdering

  trait DoubleOrdering extends Ordering[Double] {
    outer =>

    def compare(x: Double, y: Double) = java.lang.Double.compare(x, y)

    override def lteq(x: Double, y: Double): Boolean = x <= y
    override def gteq(x: Double, y: Double): Boolean = x >= y
    override def lt(x: Double, y: Double): Boolean = x < y
    override def gt(x: Double, y: Double): Boolean = x > y
    override def equiv(x: Double, y: Double): Boolean = x == y
    override def max(x: Double, y: Double): Double = math.max(x, y)
    override def min(x: Double, y: Double): Double = math.min(x, y)

    override def reverse: Ordering[Double] = new DoubleOrdering {
      override def reverse = outer
      override def compare(x: Double, y: Double) = outer.compare(y, x)

      override def lteq(x: Double, y: Double): Boolean = outer.lteq(y, x)
      override def gteq(x: Double, y: Double): Boolean = outer.gteq(y, x)
      override def lt(x: Double, y: Double): Boolean = outer.lt(y, x)
      override def gt(x: Double, y: Double): Boolean = outer.gt(y, x)
    }
  }
  implicit object Double extends DoubleOrdering
}
