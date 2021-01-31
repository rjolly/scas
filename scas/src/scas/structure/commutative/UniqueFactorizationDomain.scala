package scas.structure.commutative

trait UniqueFactorizationDomain[T] extends scas.structure.NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  extension[U] (x: U)(using c: U => T) {
    def % (y: T): T = c(x).remainder(y)
    def /%(y: T): (T, T) = c(x).divideAndRemainder(y)
    def | (y: T): Boolean = c(x).factorOf(y)
  }
  extension (x: T) {
    def % [U](y: U)(using c: U => T): T = x.remainder(c(y))
    def /%[U](y: U)(using c: U => T): (T, T) = x.divideAndRemainder(c(y))
    def | [U](y: U)(using c: U => T): Boolean = x.factorOf(c(y))
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
  }
}

object UniqueFactorizationDomain {
  def apply[T : UniqueFactorizationDomain] = summon[UniqueFactorizationDomain[T]]
}
