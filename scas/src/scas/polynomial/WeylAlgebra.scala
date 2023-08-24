package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

trait WeylAlgebra[T : ClassTag, C : Ring.Impl, M : PowerProduct.Impl] extends WeylAlgebra.Impl[T, C, M] with SolvablePolynomial[T, C, M]

object WeylAlgebra {
  trait Impl[T : ClassTag, C : Ring.Impl, M : PowerProduct.Impl] extends SolvablePolynomial.Impl[T, C, M] {
    val n = variables.length >> 1
    for (i <- 0 until n; j = i + n) {
      val xi = generator(i)
      val xj = generator(j)
      update(xj, xi, xi * xj + one)
    }
  }
}
