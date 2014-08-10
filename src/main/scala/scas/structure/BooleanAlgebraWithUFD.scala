package scas.structure

import spire.macros.Ops
import scas.Implicits.infixBooleanOps

trait BooleanAlgebraWithUFD[@specialized(Boolean) T] extends BooleanAlgebra[T] with UniqueFactorizationDomain[T] {
  implicit def self: BooleanAlgebraWithUFD[T]
  def gcd(x: T, y: T) = x || y
  def not(x: T) = x ^ one
}

object BooleanAlgebraWithUFD {
  trait ExtraImplicits extends BooleanAlgebra.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits {
    implicit def infixBooleanOps[T: BooleanAlgebraWithUFD](lhs: T): Ops[T] = new OpsImpl[T](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends BooleanAlgebra.Element[T] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: BooleanAlgebraWithUFD[T]
  }
  trait Ops[T] extends BooleanAlgebra.Ops[T] with UniqueFactorizationDomain.Ops[T]
  class OpsImpl[T: BooleanAlgebraWithUFD](lhs: T) extends Ops[T]
}
