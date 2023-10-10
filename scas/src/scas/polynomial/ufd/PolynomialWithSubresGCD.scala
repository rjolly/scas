package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait PolynomialWithSubresGCD[T[C, M], C, M](using ClassTag[T[C, M]])(using ring: UniqueFactorizationDomain[C], pp: PowerProduct[M]) extends MultivariatePolynomial[T, C, M] {
  @tailrec final def gcd1(x: T[C, M], y: T[C, M]): T[C, M] = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  @tailrec final def gcd(x: T[C, M], y: T[C, M], beta: C, phi: C): T[C, M] = if (y.isZero) x else if (x.isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, x.reduce(y).coefDivide(beta), headCoefficient(x) * phi\d, if (d >< 0) phi else if (d >< 1) headCoefficient(y) else headCoefficient(y)\d / phi\(d - 1))
  }
  extension (x: T[C, M]) {
    override def reduce(m: M, a: C, y: T[C, M], b: C) = x.coefMultiply(b).subtract(m, a, y)
    override def reduce(y: T[C, M]) = super.reduce(x)(y)
  }
}
