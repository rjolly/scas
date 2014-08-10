package scas.structure

import spire.macros.Ops
import scas.Implicits.infixUFDOps

trait UniqueFactorizationDomain[@specialized(Int, Long, Double) T] extends Ring[T] {
  implicit def self: UniqueFactorizationDomain[T]
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def divide(x: T, y: T) = { val (q, _) = x /% y ; q }
  def remainder(x: T, y: T) = { val (_, r) = x /% y ; r }
  def divideAndRemainder(x: T, y: T): (T, T)
  def factorOf(x: T, y: T) = (y % x).isZero
}

object UniqueFactorizationDomain {
  trait ExtraImplicits extends Ring.ExtraImplicits {
    implicit def infixUFDOps[T: UniqueFactorizationDomain](lhs: T): Ops[T] = new OpsImpl(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ring.Element[T] { this: T =>
    val factory: UniqueFactorizationDomain[T]
    def / (that: T) = factory.divide(this, that)
    def % (that: T) = factory.remainder(this, that)
    def /%(that: T) = factory.divideAndRemainder(this, that)
    def | (that: T) = factory.factorOf(this, that)
  }
  trait Ops[T] extends Ring.Ops[T] {
    def / (rhs: T) = macro Ops.binop[T, T]
    def % (rhs: T) = macro Ops.binop[T, T]
    def /%(rhs: T) = macro Ops.binop[T, (T, T)]
    def | (rhs: T) = macro Ops.binop[T, Boolean]
  }
  class OpsImpl[T: UniqueFactorizationDomain](lhs: T) extends Ops[T]
}
