package scas.structure

import spire.macros.Ops
import scas.Implicits.infixStarOps

trait StarRingWithUFD[@specialized(Boolean) T] extends StarRing[T] with UniqueFactorizationDomain[T] {
  implicit def self: StarRingWithUFD[T]
}

object StarRingWithUFD {
  trait ExtraImplicits extends StarRing.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits {
    implicit def infixStarOps[T: StarRingWithUFD](lhs: T): Ops[T] = new OpsImpl[T](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends StarRing.Element[T] with UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: StarRingWithUFD[T]
  }
  trait Ops[T] extends StarRing.Ops[T] with UniqueFactorizationDomain.Ops[T]
  class OpsImpl[T: StarRingWithUFD](lhs: T) extends Ops[T]
}
