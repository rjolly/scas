package scas.polynomial.ufd

import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait PolynomialWithSubresGCD[T[C, M], C, N] extends MultivariatePolynomial[T, C, N] {
  @tailrec final def gcd1(x: T[C, Array[N]], y: T[C, Array[N]]): T[C, Array[N]] = if x.degree < y.degree then gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  @tailrec final def gcd(x: T[C, Array[N]], y: T[C, Array[N]], beta: C, phi: C): T[C, Array[N]] = if y.isZero then x else if x.isZero then y else {
    val d = x.degree - y.degree
    gcd(y, x.reduce(y)%/ beta, x.headCoefficient * phi\d, if d >< 0 then phi else if d >< 1 then y.headCoefficient else y.headCoefficient\d / phi\(d - 1))
  }
  extension (x: T[C, Array[N]]) {
    override def reduce(m: Array[N], a: C, y: T[C, Array[N]], b: C, remainder: Boolean) = (x%* b).subtract(m, a, y)
    override def reduce(ys: T[C, Array[N]]*) = super.reduce(x)(ys*)
  }
}
