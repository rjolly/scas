package scas.structure

import scas.Boolean
import spire.macros.Ops
import BooleanAlgebra.OpsImpl

trait BooleanAlgebra[@specialized(scala.Boolean) T] extends Algebra[T, Boolean] {
  val ring = Boolean
  def ltimes(x: Boolean, y: T) = if(x) y else zero
  def and(x: T, y: T) = x * y
  def or(x: T, y: T) = x ^ y ^ (x & y)
  def xor(x: T, y: T) = x + y
  def not(x: T): T
  def implies(x: T, y: T) = y || !x
  override implicit def mkOps(lhs: T): BooleanAlgebra.Ops[T] = new OpsImpl[T](lhs)(this)
}

object BooleanAlgebra {
  trait ExtraImplicits extends Algebra.ExtraImplicits {
    implicit def infixBooleanAlgebraOps[T: BooleanAlgebra](lhs: T) = implicitly[BooleanAlgebra[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Algebra.Element[T, Boolean] { this: T =>
    val factory: BooleanAlgebra[T]
    def & (that: T) = factory.and(this, that)
    def ||(that: T) = factory.or(this, that)
    def ^ (that: T) = factory.xor(this, that)
    def unary_! = factory.not(this)
    def >>(that: T) = factory.implies(this, that)
  }
  trait Ops[T] extends Algebra.Ops[T, Boolean] {
    def & (rhs: T) = macro Ops.binop[T, T]
    def ||(rhs: T) = macro Ops.binop[T, T]
    def ^ (rhs: T) = macro Ops.binop[T, T]
    def unary_!() = macro Ops.unop[T]
    def >>(rhs: T) = macro Ops.binop[T, T]
  }
  class OpsImpl[T: BooleanAlgebra](lhs: T) extends Ops[T]
}
