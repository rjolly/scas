package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.Field
import scas.power.impl.PowerProduct

trait PolynomialOverField[T : ClassTag, C : Field, M : PowerProduct] extends impl.PolynomialOverField[T, C, M] with PolynomialOverUFD[T, C, M]
