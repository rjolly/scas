package scas.math

import spire.macros.Ops
import Integral.Ops

trait Integral[@specialized(Byte, Short, Int, Long) T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T
  def quotrem(x: T, y: T) = (quot(x, y), rem(x, y))
  override implicit def mkNumericOps(lhs: T) = new Ops(lhs)(this)
}

object Integral {
  trait ExtraImplicits {
    implicit def infixIntegralOps[T: Integral](x: T) = implicitly[Integral[T]].mkNumericOps(x)
  }
  object Implicits extends ExtraImplicits

  class Ops[T: Integral](lhs: T) extends Numeric.Ops(lhs) {
    def /(rhs: T) = macro Ops.binop[T, T]
    def %(rhs: T) = macro Ops.binop[T, T]
    def /%(rhs: T) = macro Ops.binop[T, T]
  }
}
