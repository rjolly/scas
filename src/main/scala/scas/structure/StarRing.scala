package scas.structure

import spire.macros.Ops
import StarRing.OpsImpl

trait StarRing[@specialized(Int, Long, Double) T] extends UniqueFactorizationDomain[T] {
  def conjugate(x: T): T
  def isReal(x: T): Boolean
  def isImag(x: T): Boolean
  override implicit def mkOps(lhs: T): StarRing.Ops[T] = new OpsImpl(lhs)(this)
}

object StarRing {
  trait ExtraImplicits extends UniqueFactorizationDomain.ExtraImplicits {
    implicit def infixStarRingOps[T: StarRing](lhs: T) = implicitly[StarRing[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: StarRing[T]
    def isReal = factory.isReal(this)
    def isImag = factory.isImag(this)
  }
  trait Ops[T] extends UniqueFactorizationDomain.Ops[T] {
    def isReal() = macro Ops.unop[Boolean]
    def isImag() = macro Ops.unop[Boolean]
  }
  class OpsImpl[T: StarRing](lhs: T) extends Ops[T]
}
