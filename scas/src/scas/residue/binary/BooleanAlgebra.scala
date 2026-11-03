package scas.residue.binary

import scas.structure.BooleanRing
import scas.polynomial.binary.Polynomial
import scas.power.compact.binary.{PowerProduct, Lexicographic}
import scas.power.compact.degree.DegreeReverseLexicographic
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import scas.base.Boolean
import BooleanAlgebra.Conv
import scas.residue.BooleanAlgebra.Element

class BooleanAlgebra(using Polynomial[Element[Array[Int]], Boolean]) extends Conv {
  def this(pp: PowerProduct) = this(using new scas.polynomial.tree.binary.mutable.Polynomial(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic(variables*))
  given instance: BooleanAlgebra = this
}

object BooleanAlgebra {
  def apply[S : Conversion[Variable]](s: S*) = new BooleanAlgebra(s.map(~_)*)
  def degreeReverse[S : Conversion[Variable]](s: S*) = new BooleanAlgebra(DegreeReverseLexicographic.binary(s*))

  trait Impl extends scas.residue.BooleanAlgebra.Impl[Array[Int]] with Residue[Element[Array[Int]], Boolean] {
    extension (x: Element[Array[Int]]) {
      override def defining = ring.defining(x)
    }
  }

  abstract class Conv(using Polynomial[Element[Array[Int]], Boolean]) extends Impl with BooleanRing.Conv[Element[Array[Int]]] {
    def instance: Conv
  }
}
