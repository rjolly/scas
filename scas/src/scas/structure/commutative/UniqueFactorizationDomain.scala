package scas.structure.commutative

trait UniqueFactorizationDomain[T] extends scas.structure.NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  extension[U] (x: U)(using Conversion[U, T]) {
    def % (y: T): T = (x: T) % y
    def /%(y: T): (T, T) = (x: T) /%y
    def | (y: T): Boolean = (x: T) | y
  }
  extension (x: T) {
    def / (y: T) = {
      val (q, _) = x /%y
      q
    }
    def % (y: T) = {
      val (_, r) = x /%y
      r
    }
    def /%(y: T): (T, T)
    def | (y: T) = (y % x).isZero
  }
}

object UniqueFactorizationDomain {
  def apply[T : UniqueFactorizationDomain] = summon[UniqueFactorizationDomain[T]]
}
