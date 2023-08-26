package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.impl.Ring
import scas.power.impl.PowerProduct

trait WeylAlgebra[T : ClassTag, C : Ring, M : PowerProduct] extends impl.WeylAlgebra[T, C, M] with SolvablePolynomial[T, C, M]
