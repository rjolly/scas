package scas.structure

import spire.macros.Ops
import BooleanAlgebraWithUFD.OpsImpl

trait BooleanAlgebraWithUFD[@specialized(Boolean) T] extends BooleanAlgebra[T] with UniqueFactorizationDomain[T] {
  def gcd(x: T, y: T) = x || y
  def not(x: T) = x ^ one
  override implicit def mkOps(lhs: T): BooleanAlgebraWithUFD.Ops[T] = new OpsImpl[T](lhs)(this)
}

object BooleanAlgebraWithUFD {
  trait ExtraImplicits extends BooleanAlgebra.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits {
    implicit def infixBooleanOps[T: BooleanAlgebraWithUFD](lhs: T) = implicitly[BooleanAlgebraWithUFD[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends BooleanAlgebra.Element[T] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: BooleanAlgebraWithUFD[T]
  }
  trait Ops[T] extends BooleanAlgebra.Ops[T] with UniqueFactorizationDomain.Ops[T]
  class OpsImpl[T: BooleanAlgebraWithUFD](lhs: T) extends Ops[T]
}
