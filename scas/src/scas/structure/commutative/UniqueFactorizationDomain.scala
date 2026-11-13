package scas.structure.commutative

import scas.structure.NotQuiteField
import scas.util.{Conversion, unary_~}
import scas.base.BigInteger.given
import scas.base.Rational

trait UniqueFactorizationDomain[T] extends NotQuiteField[T] {
  def gcd(x: T, y: T): T
  def lcm(x: T, y: T) = (x * y) / gcd(x, y)
  def gcd[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = gcd(~x, ~y)
  def lcm[U: Conversion[T], V: Conversion[T]](x: U, y: V): T = lcm(~x, ~y)
  def pow[U: Conversion[T], V: Conversion[Rational]](a: U, b: V): T = pow(~a, ~b)
  def sqrt[U: Conversion[T]](x: U): T = sqrt(~x)
  def sqrt(x: T) = x.nthrt(2)
  def pow(a: T, b: Rational) = {
    val Rational(n, d) = b
    a.nthrt(d.intValue()) \ n
  }
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
    def isSquare = x.isPower(2)
    def isPower(n: Int): Boolean
    def nthrt(n: Int): T
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
