package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.power.splittable.impl.PowerProduct
import scas.structure.commutative.impl.UniqueFactorizationDomain

trait MultivariatePolynomial[T[C, M], C : UniqueFactorizationDomain, M : PowerProduct](using ClassTag[T[C, M]]) extends impl.MultivariatePolynomial[T, C, M] with PolynomialOverUFD[T[C, M], C, M]
