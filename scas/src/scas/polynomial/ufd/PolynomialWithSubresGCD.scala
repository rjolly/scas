package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait PolynomialWithSubresGCD[T : ClassTag, C, M : PowerProduct](using ring: UniqueFactorizationDomain[C]) extends PolynomialOverUFD[T, C, M] {
  @tailrec final def gcd1(x: T, y: T): T = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  @tailrec final def gcd(x: T, y: T, beta: C, phi: C): T = if (y.isZero) x else if (x.isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, x.reduce(y).coefDivide(beta), headCoefficient(x) * phi\d, if (d >< 0) phi else if (d >< 1) headCoefficient(y) else headCoefficient(y)\d / phi\(d - 1))
  }
  extension (x: T) {
    override def reduce(m: M, a: C, y: T, b: C) = x.coefMultiply(b).subtract(m, a, y)
    override def reduce(y: T) = super.reduce(x)(y)
  }
}