package scas.residue.growable

import scas.polynomial.TreePolynomial.Element
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.polynomial.tree.growable.PolynomialWithGB
import scas.power.growable.Lexicographic
import scas.variable.Variable
import scas.base.{BigInteger, Boolean}
import BigInteger.given

open class BooleanAlgebra(using PolynomialOverUFD[Element[Boolean, Array[Int]], Boolean, Array[Int]]) extends BooleanAlgebra.Impl {
  def this(variables: Variable*) = this(using new PolynomialWithGB(using Boolean, new Lexicographic[Int](variables*)))
}

object BooleanAlgebra {
  trait Impl extends Residue[Element[Boolean, Array[Int]], Boolean, Array[Int]] with scas.residue.BooleanAlgebra.Impl {
    override def extend(variables: Variable*): Unit = {
      super.extend(variables*)
      update(generators.drop(ring.pp.variables.length - variables.length).map(_.defining)*)
    }
  }
}
