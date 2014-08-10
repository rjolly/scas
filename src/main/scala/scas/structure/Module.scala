package scas.structure

import spire.macros.Ops
import scas.Implicits.infixModuleOps
import Module.Scalar

trait Module[T, R] extends AbelianGroup[T] {
  implicit def self: Module[T, R]
  implicit def ring: Ring[R]
  def ltimes(x: R, y: T): T
  def rtimes(x: T, y: R) = ltimes(y, x)
  def scalar(lhs: R) = new Scalar(lhs)(this)
}

object Module {
  trait ExtraImplicits extends AbelianGroup.ExtraImplicits {
    implicit def infixModuleOps[T, R](lhs: T)(implicit factory: Module[T, R]): Ops[T, R] = new OpsImpl[T, R](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends AbelianGroup.Element[T] { this: T =>
    val factory: Module[T, R]
    def *:(that: R) = factory.ltimes(that, this)
    def :*(that: R) = factory.rtimes(this, that)
  }
  trait Ops[T, R] extends AbelianGroup.Ops[T] {
    def *:(lhs: R) = macro Ops.rbinop[R, T]
    def :*(rhs: R) = macro Ops.binop[R, T]
  }
  class OpsImpl[T, R](lhs: T)(implicit factory: Module[T, R]) extends Ops[T, R]
  class Scalar[T, R](lhs: R)(factory: Module[T, R]) {
    def *(rhs: T) = factory.ltimes(lhs, rhs)
  }
}
