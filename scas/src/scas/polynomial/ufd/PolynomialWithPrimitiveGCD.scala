package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scala.annotation.tailrec

trait PolynomialWithPrimitiveGCD[T[C, M], C : UniqueFactorizationDomain, M : PowerProduct](using ClassTag[T[C, M]]) extends MultivariatePolynomial[T, C, M] {
  @tailrec final def gcd1(x: T[C, M], y: T[C, M]): T[C, M] = if (y.isZero) primitivePart(x) else gcd1(y, primitivePart(x.reduce(y)))
}
