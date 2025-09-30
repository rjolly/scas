package scas.polynomial

import scala.compiletime.deferred
import scas.power.growable.PowerProduct

trait GrowablePolynomial[T, C, M] extends Polynomial[T, C, M] {
  given pp: PowerProduct[M] = deferred
}
