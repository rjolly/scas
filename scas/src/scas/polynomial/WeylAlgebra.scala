package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

trait WeylAlgebra[T : ClassTag, C : Ring, M](using pp: PowerProduct[M]) extends SolvablePolynomial[T, C, M] {
  val n = variables.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator(i)
    val xj = generator(j)
    update(xj, xi, xi * xj + one)
  }
}
