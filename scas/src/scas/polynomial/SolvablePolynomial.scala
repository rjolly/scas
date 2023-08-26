package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.impl.Ring
import scas.power.impl.PowerProduct

trait SolvablePolynomial[T : ClassTag, C : Ring, M : PowerProduct] extends impl.SolvablePolynomial[T, C, M] with Polynomial[T, C, M]
