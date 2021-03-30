package scas.structure.commutative

trait UniqueFactorizationDomain[T] extends scas.structure.NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  extension (x: T) {
    def divide(y: T) = {
      val (q, _) = x /%y
      q
    }
    def remainder(y: T) = {
      val (_, r) = x /%y
      r
    }
    def divideAndRemainder(y: T): (T, T)
    def factorOf(y: T) = (y % x).isZero
    def % (y: T) = x.remainder(y)
    def /%(y: T) = x.divideAndRemainder(y)
    def | (y: T) = x.factorOf(y)
  }
}

object UniqueFactorizationDomain {
  def apply[T : UniqueFactorizationDomain] = summon[UniqueFactorizationDomain[T]]
}
