package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.power.splitable.{ArrayPowerProduct, Lexicographic}
import scas.structure.commutative.{UniqueFactorizationDomain, Field}
import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import scas.polynomial.TreePolynomial
import TreePolynomial.Element
import MultivariatePolynomial.Impl

abstract class MultivariatePolynomial[C, N : {Numeric, ClassTag}](using UniqueFactorizationDomain[C], ArrayPowerProduct[N]) extends Impl[C, N] with UniqueFactorizationDomain.Conv[Element[C, Array[N]]] {
  def this(degree: N)(ring: UniqueFactorizationDomain[C])(variables: Variable*) = this(using summon, summon, ring, new Lexicographic(degree)(variables*))
  given instance: MultivariatePolynomial[C, N] = this
}

object MultivariatePolynomial {
  trait Impl[C, N] extends TreePolynomial[C, Array[N]] with scas.polynomial.ufd.MultivariatePolynomial[Element, C, N]

  def withSimpleGCD[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSimpleGCD(degree)(ring)(s.map(~_)*)
  def withPrimitiveGCD[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithPrimitiveGCD(degree)(ring)(s.map(~_)*)
  def withSubresGCD[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: UniqueFactorizationDomain[C])(s: S*) = new PolynomialWithSubresGCD(degree)(ring)(s.map(~_)*)
  def apply[C, N : {Numeric, ClassTag}, S : Conversion[Variable]](degree: N)(ring: Field[C])(s: S*) = new MultivariatePolynomialOverField(degree)(ring)(s.map(~_)*)
}
