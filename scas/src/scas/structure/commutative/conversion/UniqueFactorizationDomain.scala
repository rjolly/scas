package scas.structure.commutative.conversion

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.conversion.NotQuiteField[T] {
  abstract override def gcd(x: T, y: T) = super.gcd(x, y)
  def gcd[U, V](x: U, y: V)(using c: U => T, d: V => T): T = gcd(c(x), d(y))
  def lcm[U, V](x: U, y: V)(using c: U => T, d: V => T) = super.lcm(c(x), d(y))
  extension[U] (x: U)(using c: U => T) {
    def % (y: T) = c(x).remainder(y)
    def /%(y: T) = c(x).divideAndRemainder(y)
    def | (y: T) = c(x).factorOf(y)
  }
  extension (x: T) {
    def % [U](y: U)(using c: U => T) = x.remainder(c(y))
    def /%[U](y: U)(using c: U => T) = x.divideAndRemainder(c(y))
    def | [U](y: U)(using c: U => T) = x.factorOf(c(y))
  }
}
