package scas.structure.commutative

import scas.structure.NotQuiteField
import scas.util.{Conversion, unary_~}

trait UniqueFactorizationDomain[T] extends NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def gcd[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = gcd(~x, ~y)
  def lcm[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = lcm(~x, ~y)
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
    inline def % [U: Conversion[T]](y: U) = x.remainder(~y)
    inline def /%[U: Conversion[T]](y: U) = x.divideAndRemainder(~y)
    inline def | [U: Conversion[T]](y: U) = x.factorOf(~y)
  }
}

object UniqueFactorizationDomain {
  trait Conv[T] extends UniqueFactorizationDomain[T] with NotQuiteField.Conv[T] {
    extension[U: Conversion[T]] (x: U) {
      inline def % [V: Conversion[T]](y: V) = (~x).remainder(~y)
      inline def /%[V: Conversion[T]](y: V) = (~x).divideAndRemainder(~y)
      inline def | [V: Conversion[T]](y: V) = (~x).factorOf(~y)
    }
  }
}
