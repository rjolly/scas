package scas.ufd

import scala.annotation.tailrec
import scas.polynomial.PolynomialOverUFD
import scas.long2bigInteger
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialWithSubresGCD[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] {
  @tailrec final def gcd1(x: T, y: T): T = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  @tailrec final def gcd(x: T, y: T, beta: C, phi: C): T = if (y.isZero) x else if (x.isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, divide(reduce(x, y), beta), headCoefficient(x) * ring.pow(phi, d), if (d == 0) phi else if (d == 1) headCoefficient(y) else ring.pow(headCoefficient(y), d) / ring.pow(phi, d - 1))
  }
  override def reduce(x: T, m: Array[N], a: C, y: T, b: C) = subtract(multiply(x, b), m, a, y)
}
