package scas.polynomial.ufd.growable

import scala.compiletime.deferred
import scas.power.growable.PowerProduct
import scas.polynomial.growable.Polynomial
import scas.structure.commutative.UniqueFactorizationDomain

trait PolynomialOverUFD[T, C, M] extends scas.polynomial.ufd.PolynomialOverUFD[T, C, M] with Polynomial[T, C, M] {
  given pp: PowerProduct[M] = deferred

  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T*): PolynomialOverUFD[T, C, M] = {
    same(s*)
    this
  }
}
