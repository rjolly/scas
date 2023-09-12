package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.commutative.impl.UniqueFactorizationDomain
import scas.power.impl.PowerProduct

trait PolynomialOverUFD[T : ClassTag, C : UniqueFactorizationDomain, M : PowerProduct] extends ufd.PolynomialOverUFD[T, C, M] with Polynomial[T, C, M] with scas.structure.commutative.UniqueFactorizationDomain[T]
