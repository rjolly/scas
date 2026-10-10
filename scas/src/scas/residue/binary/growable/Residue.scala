package scas.residue.binary.growable

import scala.compiletime.deferred
import scas.polynomial.ufd.binary.growable.Polynomial

trait Residue[T, C] extends scas.residue.binary.Residue[T, C] with scas.residue.growable.Residue[T, C, Array[Int]] {
  given ring: Polynomial[T, C] = deferred
}
