package scas.structure

import spire.macros.Ops
import AbelianGroup.OpsImpl

trait AbelianGroup[@specialized(Int, Long) T] extends Structure[T] {
  def zero = apply(0)
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def identity(x: T) = x
  def negate(x: T) = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
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
    def isZero = factory.isZero(lhs)
    def +(rhs: T) = factory.plus(lhs, rhs)
    def -(rhs: T) = factory.minus(lhs, rhs)
    def unary_- = factory.negate(lhs)
    def unary_+ = factory.identity(lhs)
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
