package scas.structure

import spire.macros.Ops
import Module.{Scalar, OpsImpl}

trait Module[T, R] extends AbelianGroup[T] {
  implicit val ring: Ring[R]
  def ltimes(x: R, y: T): T
  def rtimes(x: T, y: R) = y *: x
  def scalar(lhs: R) = new Scalar(lhs)(this)
  override implicit def mkOps(lhs: T): Module.Ops[T, R] = new OpsImpl[T, R](lhs)(this)
}

object Module {
  trait ExtraImplicits extends AbelianGroup.ExtraImplicits {
    implicit def infixModuleOps[T, R](lhs: T)(implicit factory: Module[T, R]) = factory.mkOps(lhs)
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
  class OpsImpl[T, R](lhs: T)(factory: Module[T, R]) extends Ops[T, R]
  class Scalar[T, R](lhs: R)(factory: Module[T, R]) {
    def *(rhs: T) = factory.ltimes(lhs, rhs)
  }
}
