package scas.residue.binary

import scala.compiletime.deferred
import scas.polynomial.binary.Polynomial

trait Residue[T, C] extends scas.residue.Residue[T, C, Array[Int]] {
  given ring: Polynomial[T, C] = deferred
}
