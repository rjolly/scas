package scas.structure

import spire.macros.Ops
import scas.Implicits.infixStarRingOps

trait StarRing[@specialized(Int, Long, Double) T] extends Ring[T] {
  implicit def self: StarRing[T]
  def conjugate(x: T): T
  def isReal(x: T): Boolean
  def isImag(x: T): Boolean
}

object StarRing {
  trait ExtraImplicits extends Ring.ExtraImplicits {
    implicit def infixStarRingOps[T: StarRing](lhs: T): Ops[T] = new OpsImpl(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ring.Element[T] { this: T =>
    val factory: StarRing[T]
    def isReal = factory.isReal(this)
    def isImag = factory.isImag(this)
  }
  trait Ops[T] extends Ring.Ops[T] {
    def isReal() = macro Ops.unop[Boolean]
    def isImag() = macro Ops.unop[Boolean]
  }
  class OpsImpl[T: StarRing](lhs: T) extends Ops[T]
}
