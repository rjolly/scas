package scas.residue.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialWithGB
import scas.power.growable.Lexicographic
import scas.variable.Variable
import scas.base.{BigInteger, Boolean}
import BigInteger.given

open class BooleanAlgebra(using PolynomialWithGB[Element[Boolean, Array[Int]], Boolean, Int]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new scas.polynomial.tree.growable.PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
}

object BooleanAlgebra {
  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean, Int] with scas.residue.BooleanAlgebra.Impl {
    override def extend(variables: Variable*): Unit = {
      super.extend(variables*)
      update(generators.drop(ring.pp.variables.length - variables.length).map(x => x+x\2)*)
    }
  }
}
