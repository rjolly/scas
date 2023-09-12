package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scala.annotation.tailrec

trait PolynomialWithPrimitiveGCD[T : ClassTag, C : UniqueFactorizationDomain, M : PowerProduct] extends PolynomialOverUFD[T, C, M] {
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) primitivePart(x) else gcd1(y, primitivePart(x.reduce(y)))
}
