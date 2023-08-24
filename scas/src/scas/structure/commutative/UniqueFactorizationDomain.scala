package scas.structure.commutative

import scas.util.{Conversion, unary_~}

trait UniqueFactorizationDomain[T] extends UniqueFactorizationDomain.Impl[T] with scas.structure.NotQuiteField[T] {
  abstract override def gcd(x: T, y: T) = super.gcd(x, y)
  def gcd[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = gcd(~x, ~y)
  def lcm[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = lcm(~x, ~y)
  override def lcm(x: T, y: T) = super.lcm(x, y)
  extension (x: T) {
    inline def % [U: Conversion[T]](y: U) = x.remainder(~y)
    inline def /%[U: Conversion[T]](y: U) = x.divideAndRemainder(~y)
    inline def | [U: Conversion[T]](y: U) = x.factorOf(~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def % (y: T) = (~x).remainder(y)
    inline def /%(y: T) = (~x).divideAndRemainder(y)
    inline def | (y: T) = (~x).factorOf(y)
  }
}

object UniqueFactorizationDomain {
  trait Impl[T] extends scas.structure.NotQuiteField.Impl[T] {
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
      inline def % (y: T) = x.remainder(y)
      inline def /%(y: T) = x.divideAndRemainder(y)
      inline def | (y: T) = x.factorOf(y)
    }
  }
}
