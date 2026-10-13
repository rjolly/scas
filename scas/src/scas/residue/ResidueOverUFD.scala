package scas.residue

import scala.compiletime.deferred
import scas.polynomial.ufd.PolynomialOverUFD

trait ResidueOverUFD[T, C, M] extends Residue[T, C, M] {
  given ring: PolynomialOverUFD[T, C, M] = deferred

  extension (ring: PolynomialOverUFD[T, C, M]) def apply(s: T*) = {
    same(s*)
    this
  }
}
