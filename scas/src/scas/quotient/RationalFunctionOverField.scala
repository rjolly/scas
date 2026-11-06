package scas.quotient

import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.Field
import scas.structure.commutative.Quotient
import scas.polynomial.tree.MultivariatePolynomialOverField
import scas.polynomial.ufd.PolynomialOverField
import scas.polynomial.TreePolynomial.Element
import scas.variable.Variable
import RationalFunctionOverField.Impl

class RationalFunctionOverField[C, N : {Numeric, ClassTag}](using PolynomialOverField[Element[C, Array[N]], C, Array[N]]) extends Impl[C, Array[N]] with Field.Conv[Quotient.Element[Element[C, Array[N]]]] {
  def this(degree: N)(ring: Field[C])(variables: Variable*) = this(using summon, summon, new MultivariatePolynomialOverField(degree)(ring)(variables*))
    given instance: RationalFunctionOverField[C, N] = this
}

object RationalFunctionOverField {
  trait Impl[C, M](using PolynomialOverField[Element[C, M], C, M]) extends QuotientOverField[Element[C, M], C, M]
}
