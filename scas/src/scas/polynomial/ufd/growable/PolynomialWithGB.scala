package scas.polynomial.ufd.growable

import scala.compiletime.deferred
import scas.power.growable.PowerProduct

trait PolynomialWithGB[T[M], C, M] extends PolynomialOverUFD[T[M], C, M] with scas.polynomial.ufd.PolynomialWithGB[T, C, M] {
  given pp: PowerProduct[M] = deferred
}
