package scas.residue.growable.convertable

import scas.base.{BigInteger, Rational, Complex}
import BigInteger.given
import Rational.given
import Complex.Impl

class ComplexNumber extends AlgebraicNumber.Conv(0)(Rational)() with Impl {
  given instance: ComplexNumber = this
  override def solve(equ: Complex, x: Complex, n: Int) = {
    val s = ring.coefficients(equ)(x)
    s.length - 1 match {
      case 1 => linear(s(0) / s(1))
      case 2 => quadratic(s(1) / s(2), s(0) / s(2), n)
      case 3 => cubic(s(2) / s(3), s(1) / s(3), s(0) / s(3), n)
      case 4 => quartic(s(3) / s(4), s(2) / s(4), s(1) / s(4), s(0) / s(4), n)
      case _ => super.solve(equ, x, n)
    }
  }
  def linear(a: Complex) = -a
  def quadratic(a: Complex, b: Complex, n: Int) = {
    val d = a \ 2 - 4 * b
    n % 2 match {
      case 0 => -(a - sqrt(d)) / 2
      case _ => -(a + sqrt(d)) / 2
    }
  }
  def cubic(a: Complex, b: Complex, c: Complex, n: Int) = {
    val y = for i <- 0 until 2 yield pow(quadratic(2 * a \ 3 - 9 * a * b + 27 * c, a \ 6 - 9 * a \ 4 * b + 27 * a \2 * b \ 2 - 27 * b \ 3, i), 1%%3)
    val j = -inverse(2) * (1 - sqrt(-3))
    n % 3 match {
      case 0 => -(a - y(0) - y(1)) / 3
      case 1 => -(a - j * y(0) - conjugate(j) * y(1)) / 3
      case _ => -(a - conjugate(j) * y(0) - j * y(1)) / 3
    }
  }
  def quartic(a: Complex, b: Complex, c: Complex, d: Complex, n: Int) = {
    val y = for i <- 0 until 3 yield sqrt(cubic(-3 * a \ 2 + 8 * b, 3 * a \ 4 - 16 * a \ 2 * b + 16 * b \ 2 + 16 * a * c - 64 * d, -a \ 6 + 8 * a \ 4 * b - 16 * a \ 2 * b \ 2 - 16 * a \ 3 * c + 64 * a * b * c - 64 * c \ 2, i))
    n % 4 match {
      case 0 => -(a - y(0) - y(1) + y(2)) / 4
      case 1 => -(a + y(0) - y(1) - y(2)) / 4
      case 2 => -(a - y(0) + y(1) - y(2)) / 4
      case _ => -(a + y(0) + y(1) + y(2)) / 4
    }
  }
}
