package scas.structure

trait UniqueFactorizationDomain[T] extends Ring[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def divide(x: T, y: T): T
  def remainder(x: T, y: T): T
  def divideAndRemainder(x: T, y: T): (T, T)
  def factorOf(x: T, y: T) = (x % y).isZero
  trait Ops extends super.Ops {
    def /  (rhs: T) = divide(lhs, rhs)
    def %  (rhs: T) = remainder(lhs, rhs)
    def /% (rhs: T) = divideAndRemainder(lhs, rhs)
    def |  (rhs: T) = factorOf(lhs, rhs)
  }
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object UniqueFactorizationDomain {
  trait ExtraImplicits {
    implicit def infixUFDOps[T: UniqueFactorizationDomain](lhs: T) = implicitly[UniqueFactorizationDomain[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ring.Element[T] { this: T =>
    val factory: UniqueFactorizationDomain[T]
    def /  (that: T) = factory.divide(this, that)
    def %  (that: T) = factory.remainder(this, that)
    def /% (that: T) = factory.divideAndRemainder(this, that)
    def |  (that: T) = factory.factorOf(this, that)
  }
}
