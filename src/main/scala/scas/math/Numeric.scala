package scas.math

import spire.macros.Ops
import Numeric.Ops

trait Numeric[@specialized(Byte, Short, Int, Long) T] extends Ordering[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def negate(x: T): T
  def fromInt(x: Int): T
  def toInt(x: T): Int
  def toLong(x: T): Long
  def toFloat(x: T): Float
  def toDouble(x: T): Double

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0

  implicit def mkNumericOps(lhs: T) = new Ops(lhs)(this)
}

object Numeric {
  trait ExtraImplicits {
    implicit def infixNumericOps[T: Numeric](x: T) = implicitly[Numeric[T]].mkNumericOps(x)
  }
  object Implicits extends ExtraImplicits { }

  class Ops[T: Numeric](lhs: T) {
    def +(rhs: T) = macro Ops.binop[T, T]
    def -(rhs: T) = macro Ops.binop[T, T]
    def *(rhs: T) = macro Ops.binop[T, T]
    def unary_-() = macro Ops.unop[T]
    def abs(): T = macro Ops.unop[T]
    def signum(): Int = macro Ops.unop[Int]
    def toInt(): Int = macro Ops.unop[Int]
    def toLong(): Long = macro Ops.unop[Long]
    def toFloat(): Float = macro Ops.unop[Float]
    def toDouble(): Double = macro Ops.unop[Double]
  }

  trait IntIsIntegral extends Integral[Int] {
    def plus(x: Int, y: Int): Int = x + y
    def minus(x: Int, y: Int): Int = x - y
    def times(x: Int, y: Int): Int = x * y
    def quot(x: Int, y: Int): Int = x / y
    def rem(x: Int, y: Int): Int = x % y
    def negate(x: Int): Int = -x
    def fromInt(x: Int): Int = x
    def toInt(x: Int): Int = x
    def toLong(x: Int): Long = x
    def toFloat(x: Int): Float = x
    def toDouble(x: Int): Double = x
  }
  implicit object IntIsIntegral extends IntIsIntegral with Ordering.IntOrdering

  trait LongIsIntegral extends Integral[Long] {
    def plus(x: Long, y: Long): Long = x + y
    def minus(x: Long, y: Long): Long = x - y
    def times(x: Long, y: Long): Long = x * y
    def quot(x: Long, y: Long): Long = x / y
    def rem(x: Long, y: Long): Long = x % y
    def negate(x: Long): Long = -x
    def fromInt(x: Int): Long = x
    def toInt(x: Long): Int = x.toInt
    def toLong(x: Long): Long = x
    def toFloat(x: Long): Float = x
    def toDouble(x: Long): Double = x
  }
  implicit object LongIsIntegral extends LongIsIntegral with Ordering.LongOrdering
}
