package scas.residue.growable

import scas.polynomial.growable.PolynomialWithGB
import scas.power.growable.{ArrayPowerProduct, Lexicographic}
import scas.variable.Variable
import scas.base.{BigInteger, Boolean}
import BigInteger.given
import BooleanAlgebra.Impl
import scas.residue.BooleanAlgebra.Element

open class BooleanAlgebra(using PolynomialWithGB[Element[Array[Int]], Boolean, Array[Int]]) extends Impl[Array[Int]] {
  def this(pp: ArrayPowerProduct[Int]) = this(using new scas.polynomial.tree.growable.PolynomialWithGB(using Boolean, pp))
  def this(variables: Variable*) = this(new Lexicographic[Int](variables*))
}

object BooleanAlgebra {
  trait Impl[M] extends scas.residue.BooleanAlgebra.Impl[M] with Residue[Element[M], Boolean, M] {
    override def extend(variables: Variable*): Unit = {
      super.extend(variables*)
      update(generators.drop(ring.pp.variables.length - variables.length).map(_.defining)*)
    }
  }
}
