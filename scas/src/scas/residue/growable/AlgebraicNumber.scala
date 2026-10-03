package scas.residue.growable

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialOverFieldWithGB
import scas.power.growable.DegreeReverseLexicographic
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import AlgebraicNumber.Impl

class AlgebraicNumber[C, N : {Numeric, ClassTag}](using Field[C])(degree: N)(variables: Variable*) extends Impl(using new scas.polynomial.tree.growable.PolynomialOverFieldWithGB(using summon, new DegreeReverseLexicographic[N](variables*)))

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(0)(s.map(~_)*)

  class Impl[C, N](using PolynomialOverFieldWithGB[Element[C, Array[N]], C, N]) extends ResidueOverField[Element[C, Array[N]], C, N]

  class Conv[C, N : {Numeric, ClassTag}](ring: Field[C])(degree: N)(variables: Variable*) extends AlgebraicNumber(using ring)(degree)(variables*) with Field.Conv[Element[C, Array[N]]] {
    given instance: Conv[C, N] = this
  }
}
