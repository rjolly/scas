package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct
import scas.polynomial.Polynomial

trait PolynomialOverUFD[T : ClassTag, C : UniqueFactorizationDomain, M : PowerProduct] extends impl.PolynomialOverUFD[T, C, M] with Polynomial[T, C, M] with scas.structure.commutative.UniqueFactorizationDomain[T]
