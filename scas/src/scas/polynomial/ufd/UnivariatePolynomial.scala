package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.Field
import scas.structure.commutative.EuclidianDomain
import scas.power.impl.PowerProduct

trait UnivariatePolynomial[T : ClassTag, C : Field, M : PowerProduct] extends impl.UnivariatePolynomial[T, C, M] with PolynomialOverField[T, C, M] with EuclidianDomain[T]
