package scas.structure

import spire.macros.Ops
import AbelianGroup.OpsImpl

trait AbelianGroup[@specialized(Int, Long, Double) T] extends Structure[T] {
  def zero = apply(0)
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def identity(x: T) = x
  def negate(x: T) = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T): Int
  def isZero(x: T) = x >< zero
  override implicit def mkOps(lhs: T): AbelianGroup.Ops[T] = new OpsImpl(lhs)(this)
}

object AbelianGroup {
  trait ExtraImplicits extends Structure.ExtraImplicits {
    implicit def infixAbelianGroupOps[T: AbelianGroup](lhs: T) = implicitly[AbelianGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] { this: T =>
    val factory: AbelianGroup[T]
    def isZero = factory.isZero(this)
    def +(that: T) = factory.plus(this, that)
    def -(that: T) = factory.minus(this, that)
    def unary_- = factory.negate(this)
    def unary_+ = factory.identity(this)
  }
  trait Ops[T] extends Structure.Ops[T] {
    def isZero() = macro Ops.unop[Boolean]
    def +(rhs: T) = macro Ops.binop[T, T]
    def -(rhs: T) = macro Ops.binop[T, T]
    def unary_-() = macro Ops.unop[T]
    def unary_+() = macro Ops.unop[T]
  }
  class OpsImpl[T: AbelianGroup](lhs: T) extends Ops[T]
}
