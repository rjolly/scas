package scas.structure

import spire.macros.Ops
import Module.{Scalar, OpsImpl}

trait Module[T <: (Int => R), R] extends AbelianGroup[T] {
  implicit val ring: Ring[R]
  def times(x: T, y: R): T
  def times(x: R, y: T): T
  def scalar(lhs: R) = new Scalar(lhs)(this)
  override implicit def mkOps(lhs: T): Module.Ops[T, R] = new OpsImpl[T, R](lhs)(this)
}

object Module {
  trait ExtraImplicits extends AbelianGroup.ExtraImplicits {
    implicit def infixModuleOps[T <: (Int => R), R](lhs: T)(implicit factory: Module[T, R]) = factory.mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends (Int => R) with AbelianGroup.Element[T] { this: T =>
    val factory: Module[T, R]
    def *(rhs: R) = factory.times(lhs, rhs)
  }
  trait Ops[T <: (Int => R), R] extends AbelianGroup.Ops[T] {
    def *(rhs: R) = macro Ops.binop[R, T]
  }
  class OpsImpl[T <: (Int => R), R](lhs: T)(implicit factory: Module[T, R]) extends Ops[T, R]
  class Scalar[T <: (Int => R), R](lhs: R)(implicit factory: Module[T, R]) {
    def *(rhs: T) = factory.times(lhs, rhs)
  }
}
