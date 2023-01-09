package scas.structure.commutative.conversion

import scas.util.{Conversion, unary_~}

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.conversion.NotQuiteField[T] {
  abstract override def gcd(x: T, y: T) = super.gcd(x, y)
  def gcd[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = gcd(~x, ~y)
  def lcm[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = lcm(~x, ~y)
  override def lcm(x: T, y: T) = super.lcm(x, y)
  extension[U: Conversion[T]] (x: U) {
    def % [V: Conversion[T]](y: V) = (~x).remainder(~y)
    def /%[V: Conversion[T]](y: V) = (~x).divideAndRemainder(~y)
    def | [V: Conversion[T]](y: V) = (~x).factorOf(~y)
  }
  extension (x: T) {
    def % [U: Conversion[T]](y: U) = x.remainder(~y)
    def /%[U: Conversion[T]](y: U) = x.divideAndRemainder(~y)
    def | [U: Conversion[T]](y: U) = x.factorOf(~y)
  }
}
