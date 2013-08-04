package scas.structure

import spire.macros.Ops
import UniqueFactorizationDomain.OpsImpl

trait UniqueFactorizationDomain[@specialized(Int, Long, Double) T] extends Ring[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def divide(x: T, y: T) = { val (q, _) = x /% y ; q }
  def remainder(x: T, y: T) = { val (_, r) = x /% y ; r }
  def divideAndRemainder(x: T, y: T): (T, T)
  def factorOf(x: T, y: T) = (y % x).isZero
  override implicit def mkOps(lhs: T): UniqueFactorizationDomain.Ops[T] = new OpsImpl(lhs)(this)
}

object UniqueFactorizationDomain {
  trait ExtraImplicits extends Ring.ExtraImplicits {
    implicit def infixUFDOps[T: UniqueFactorizationDomain](lhs: T) = implicitly[UniqueFactorizationDomain[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ring.Element[T] { this: T =>
    val factory: UniqueFactorizationDomain[T]
    def /  (rhs: T) = factory.divide(lhs, rhs)
    def %  (rhs: T) = factory.remainder(lhs, rhs)
    def /% (rhs: T) = factory.divideAndRemainder(lhs, rhs)
    def |  (rhs: T) = factory.factorOf(lhs, rhs)
  }
  trait Ops[T] extends Ring.Ops[T] {
    def /  (rhs: T) = macro Ops.binop[T, T]
    def %  (rhs: T) = macro Ops.binop[T, T]
    def /% (rhs: T) = macro Ops.binop[T, (T, T)]
    def |  (rhs: T) = macro Ops.binop[T, Boolean]
  }
  class OpsImpl[T: UniqueFactorizationDomain](lhs: T) extends Ops[T]
}
