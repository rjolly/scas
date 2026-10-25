package scas.polynomial.growable.ufd

import scala.compiletime.deferred
import scas.power.growable.PowerProduct

trait PolynomialWithGB[T, C, M] extends PolynomialOverUFD[T, C, M] with scas.polynomial.ufd.PolynomialWithGB[T, C, M] {
  given pp: PowerProduct[M] = deferred
}
