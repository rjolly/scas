package scas.ufd

import scala.annotation.tailrec
import scas.polynomial.PolynomialOverUFD
import scas.long2bigInteger
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait SubresGCD[T <: Element[T, C, N], C, @specialized(Int, Long) N] { this: PolynomialOverUFD[T, C, N] =>
  @tailrec final def gcd1(x: T, y: T): T = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  @tailrec final def gcd(x: T, y: T, beta: C, phi: C): T = if (y.isZero) x else if (x.isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, divide(reduce(x, y), beta), headCoefficient(x) * ring.pow(phi, d), if (d == 0) phi else if (d == 1) headCoefficient(y) else ring.pow(headCoefficient(y), d) / ring.pow(phi, d - 1))
  }
  abstract override def subtract(x: T, m: Array[N], a: C, y: T, b: C) = multiply(x, b) - multiply(y, m, a)
}
