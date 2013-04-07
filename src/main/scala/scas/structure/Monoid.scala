package scas.structure

import scas.BigInteger
import spire.macros.Ops
import Monoid.OpsImpl

trait Monoid[@specialized(Int, Long) T] extends SemiGroup[T] {
  def one = apply(1)
  def pow(x: T, exp: BigInteger) = {
    assert (exp.intValue() >= 0)
    (one /: (1 to exp.intValue())) { (l, _) => l * x }
  }
  def isUnit(x: T): Boolean
  def isOne(x: T) = x >< one
  override implicit def mkOps(lhs: T): Monoid.Ops[T] = new OpsImpl(lhs)(this)
}

object Monoid {
  trait ExtraImplicits extends SemiGroup.ExtraImplicits {
    implicit def infixMonoidOps[T: Monoid](lhs: T) = implicitly[Monoid[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends SemiGroup.Element[T] { this: T =>
    val factory: Monoid[T]
    def isUnit = factory.isUnit(lhs)
    def isOne = factory.isOne(lhs)
  }
  trait Ops[T] extends SemiGroup.Ops[T] {
    def isUnit() = macro Ops.unop[Boolean]
    def isOne() = macro Ops.unop[Boolean]
  }
  class OpsImpl[T: Monoid](lhs: T) extends Ops[T]
}
