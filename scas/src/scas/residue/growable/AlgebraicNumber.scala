package scas.residue.growable

import scas.math.Numeric
import scala.reflect.ClassTag
import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialOverFieldWithGB
import scas.power.growable.DegreeReverseLexicographic
import scas.structure.commutative.Field
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class AlgebraicNumber[C, N : {Numeric, ClassTag}](using Field[C])(degree: N)(variables: Variable*) extends ResidueOverField[Element[C, Array[N]], C, N] {
  override given ring: PolynomialOverFieldWithGB[Element[C, Array[N]], C, N] = new scas.polynomial.tree.growable.PolynomialOverFieldWithGB(using summon, new DegreeReverseLexicographic[N](variables*))
}

object AlgebraicNumber {
  def apply[C, S : Conversion[Variable]](ring: Field[C])(s: S*) = new Conv(ring)(0)(s.map(~_)*)

  class Conv[C, N : {Numeric, ClassTag}](ring: Field[C])(degree: N)(variables: Variable*) extends AlgebraicNumber(using ring)(degree)(variables*) with Field.Conv[Element[C, Array[N]]] {
    given instance: Conv[C, N] = this
  }
}
