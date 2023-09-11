package scas.polynomial.ufd.impl

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scala.annotation.tailrec

trait PolynomialWithSimpleGCD[T : ClassTag, C : UniqueFactorizationDomain, M : PowerProduct] extends PolynomialOverUFD[T, C, M] {
  @tailrec final def gcd1(x: T, y: T): T = if (y.isZero) x else gcd1(y, x.reduce(y))
}
