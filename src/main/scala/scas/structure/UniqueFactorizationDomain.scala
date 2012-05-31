package scas.structure

trait UniqueFactorizationDomain[T] extends Ring[T] { outer =>
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def divide(x: T, y: T) = { val (q, r) = x /% y ; q }
  def remainder(x: T, y: T) = { val (q, r) = x /% y ; r }
  def divideAndRemainder(x: T, y: T): (T, T)
  def factorOf(x: T, y: T) = (x % y).isZero
  override implicit def mkOps(value: T): UniqueFactorizationDomain.Ops[T] = new UniqueFactorizationDomain.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object UniqueFactorizationDomain {
  trait ExtraImplicits {
    implicit def infixUFDOps[T: UniqueFactorizationDomain](lhs: T) = implicitly[UniqueFactorizationDomain[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ring.Element[T] with Ops[T] { this: T =>
  }
  trait Ops[T] extends Ring.Ops[T] {
    val factory: UniqueFactorizationDomain[T]
    def /  (rhs: T) = factory.divide(lhs, rhs)
    def %  (rhs: T) = factory.remainder(lhs, rhs)
    def /% (rhs: T) = factory.divideAndRemainder(lhs, rhs)
    def |  (rhs: T) = factory.factorOf(lhs, rhs)
  }
}
